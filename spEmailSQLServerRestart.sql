USE [master]
GO
/****** Object:  StoredProcedure [dbo].[spEmailSQLServerRestart]    Script Date: 8/3/2020 1:13:41 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER PROC [dbo].[spEmailSQLServerRestart] AS
BEGIN
/* Rev 4 - This is a merge between my original spEmailSQLServerRestart store proc and More info: https://www.BrentOzar.com/go/startupmail
Contributors from the live stream: JediMindGorilla, GSerdjinn, WetSeal, RenegadeLarsen

Rev 1 Way to much data 02/29/2020
Rev 2 Filter messages by message type looking for what caused the restart... 06/07/2020
Rev 3 Merge with Brent's code.  to add in DB health 06/28/2020
Rev 4 Updated the list header to show normal or not.  Hence I will be doing a check DB ASAP if not normal or something like it.... (6/29/2020)

Known issues:
Just not happy with the Why and what is logged.  Need to look into this more.  I see three causes
	1. Human restarted the server
	2. Human restarted the service
	3. Something crashed or major dump occured

*/
DECLARE @DatabaseMailProfileName SYSNAME = NULL, 
	@Recipients VARCHAR(MAX) = NULL,
	@StringToExecute NVARCHAR(4000);


/* If the config table exists, get recipients & valid email profile */
IF EXISTS (SELECT * FROM sys.all_objects WHERE name = 'sp_SendStartupEmail_Config')
	BEGIN
	SET @StringToExecute = N'SELECT TOP 1 @DatabaseMailProfileName_Table = DatabaseMailProfileName, @Recipients_Table = Recipients 
		FROM dbo.sp_SendStartupEmail_Config mc
		INNER JOIN msdb.dbo.sysmail_profile p ON mc.DatabaseMailProfileName = p.name;'
	EXEC sp_executesql @StringToExecute, N'@DatabaseMailProfileName_Table SYSNAME OUTPUT, @Recipients_Table VARCHAR(MAX) OUTPUT',
		@DatabaseMailProfileName_Table = @DatabaseMailProfileName OUTPUT, @Recipients_Table = @Recipients OUTPUT;
	END



IF @DatabaseMailProfileName IS NULL AND 0 < (SELECT COUNT(*) FROM msdb.dbo.sysmail_profile)
	SELECT TOP 1 @DatabaseMailProfileName = name
	FROM msdb.dbo.sysmail_profile order by profile_id;

/* If they didn't specify a recipient, use the last operator that got an email */
IF @Recipients IS NULL
	SELECT TOP (1) @Recipients = email_address 
	FROM msdb.dbo.sysoperators o 
	WHERE o.[enabled] = 1 ORDER BY o.last_email_date DESC;

IF @DatabaseMailProfileName IS NULL OR @Recipients IS NULL 
	RETURN;

DECLARE @email_subject NVARCHAR(255) = N'SQL Server Started: ' + COALESCE(@@SERVERNAME, N'Unknown Server Name');
DECLARE	@email_body NVARCHAR(MAX);

IF NOT EXISTS (SELECT * FROM sys.databases WHERE state NOT IN (0, 1, 7, 10))
	SET @email_body = N'<h1>All databases okay.</h1>';
ELSE
	BEGIN
	IF (SELECT count(1) FROM sys.databases WHERE state NOT IN (0, 1, 7, 10))>=1 
		BEGIN
		SELECT @email_body =  COALESCE(@email_body + NCHAR(13) + NCHAR(10),N'') + x.TextOut
		FROM (
		SELECT 1 as Orderby, '<h1>Databases all NOT online;</h1><ul>' as TextOut
		UNION
		SELECT 2, N'<li>' + COALESCE(name, N' Database ID ' + CAST(database_id AS NVARCHAR(10))) COLLATE SQL_Latin1_General_CP1_CI_AS + N' state: ' + state_desc + N'</li>'
			FROM sys.databases
			WHERE state NOT IN (0, 1, 7, 10)
		UNION 
		SELECT 3, N'</ul>'
		) x
		ORDER BY x.orderby
		--print @email_body
		END
	ELSE
		BEGIN
			IF @email_body IS NULL
				SET @email_body = N'We couldn''t get a list of databases with problems. Better check on this server manually.';

		END
	END


-- Search through all available logs in one go
-- Looking for hardware issue, and restart reason
DECLARE @SearchString1 NVARCHAR(4000)
DECLARE @SearchString2 NVARCHAR(4000)
DECLARE @LogType INT

-- ------------------------------------------------------------------------------
-- User configurable settings - set up the search conditions here.

-- First search string (or leave blank for everything)
SET @SearchString1 = ''
-- Second search string (or leave blank for everything)
SET @SearchString2 = ''
-- Set log to be searched - 1=SQL Server log, 2=SQL Agent log
SET @LogType = 2
-- ------------------------------------------------------------------------------

-- Generate a list of all logs, and store in a temporary table.
CREATE TABLE #ListOfLogs (LogNumber INT, StartDate DATETIME, SizeInBytes INT)
INSERT INTO #ListOfLogs EXEC xp_enumerrorlogs @LogType

-- Iterate around all the logs gathering results
CREATE TABLE #Results (LogDate DATETIME,ProcessInfo NVARCHAR(4000),Text NVARCHAR(4000))
DECLARE @Count INT
SET @Count = 0
WHILE @Count <= (SELECT MAX(LogNumber) FROM #ListOfLogs)
  BEGIN
  INSERT INTO #Results EXEC xp_readerrorlog 
     @Count
    ,@LogType             -- 1=SQL Server log, 2=SQL Agent log
    ,@SearchString1       -- Search string
    ,@SearchString2       -- 2nd search string
  SET @Count = @Count + 1
  END

Declare @xLogDate DATETIME;
DECLARE @B1 NVARCHAR(Max);
DECLARE @header NVARCHAR(200);

-- Return the results from the temporary table.
SELECT Top 1 @xLogDate = LogDate --, processinfo, text, left(text,5) --,
 FROM #Results 
where left(Text,5) = '[000]' -- common message
ORDER BY LogDate DESC

-- Set the header if normal or not
IF NOT EXISTS (SELECT Top 1  text
					 FROM #Results
					 WHERE left(Text,5) = '[130]' --reboot type, Seems this is normal for manual shutdown
						AND LogDate >= CONVERT( datetime, convert( varchar(8), @xLogDate, 112)) -- since last reboot
					 ORDER BY LogDate DESC)
 SET @header = N'<h1>Boot Details (UNKNOWN)</h1>';
ELSE
 SET @header = N'<h1>Boot Details (normal)</h1>';


-- results in mutiple reboots....
-- limited with @xLogDate to the most resent day of reboots
SELECT @B1 = COALESCE(@B1 + NCHAR(13) + NCHAR(10),'') + T2
FROM (
SELECT 1 as orderby ,GETDATE()+1 as LogDate, @header as T2
UNION
SELECT  2, LogDate, '<li>'+replace(replace(convert(NVARCHAR,LogDate,20)+' '+left(text,5) + IIF(len(text)>5,' - ' + right(text,len(text)-5),''),NCHAR(13),''),NCHAR(10),'')+'</li>' as T1
 FROM #Results 
where LogDate >= CONVERT( datetime, convert( varchar(8), @xLogDate, 112)) --@xLogDate
UNION 
SELECT 3,GETDATE(), '</ul>'
) x
ORDER BY orderby,LogDate DESC

-- Tidy up.
DROP TABLE #ListOfLogs
DROP TABLE #Results

Set @email_body = @email_body + NCHAR(13) + NCHAR(10) + @B1

/*-- debug details
print @DatabaseMailProfileName;
Print @Recipients;
Print @email_body;
Print @email_subject;--*/

EXEC msdb.dbo.sp_send_dbmail  
    @profile_name = @DatabaseMailProfileName,  
    @recipients = @Recipients,  
	@body_format = 'HTML',
    @body = @email_body,  
    @subject = @email_subject ;

END
