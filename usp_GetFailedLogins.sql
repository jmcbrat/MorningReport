/****** Object:  StoredProcedure [dbo].[usp_GetFailedLogins]    Script Date: 6/29/2020 2:48:10 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFailedLogins') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFailedLogins AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetFailedLogins] (
 @TXTOUT NVARCHAR(Max) OUTPUT
 )
AS
/**********************************************************
--
-- Create 3/28/2020 JMCBRATNIE
-- List login attempts that failed inthe past day
--
***********************************************************/

BEGIN
   SET NOCOUNT ON;

   DECLARE @ErrorLogCount INT; 
   DECLARE @LastLogDate DATETIME;
   DECLARE @TXTOUT2 NVARCHAR(MAX);
   DECLARE @serviceState NVARCHAR(512);


   DECLARE @ErrorLogInfo TABLE (
       LogDate DATETIME
      ,ProcessInfo NVARCHAR (50)
      ,[Text] NVARCHAR (MAX)
      );
   
   DECLARE @EnumErrorLogs TABLE (
       [Archive#] INT
      ,[Date] DATETIME
      ,LogFileSizeMB INT
      );

   INSERT INTO @EnumErrorLogs
   EXEC sp_enumerrorlogs;

   SELECT @ErrorLogCount = MIN([Archive#]), @LastLogDate = MAX([Date])
   FROM @EnumErrorLogs;

   WHILE @ErrorLogCount IS NOT NULL
   BEGIN

      INSERT INTO @ErrorLogInfo
      EXEC sp_readerrorlog @ErrorLogCount

      SELECT @ErrorLogCount = MIN([Archive#]), @LastLogDate = MAX([Date])
      FROM @EnumErrorLogs
      WHERE [Archive#] > @ErrorLogCount
      AND @LastLogDate > getdate() - 1; 
  
   END;

   -- if not data then skip the output
   IF NOT EXISTS(SELECT * 
					FROM @ErrorLogInfo 
					WHERE ProcessInfo = 'Logon' 
					AND TEXT LIKE '%fail%' --+char(39)+"
					AND LogDate > getdate() - 1)
		BEGIN
			PRINT 'No matching row exists';
		END;
	ELSE
        -- create the output in html format
		BEGIN
			SELECT @TXTOUT =CAST('<h2>Failed log on attempts: </h2><ul>' AS NVARCHAR(max));

			SELECT @TXTOUT = @TXTOUT + CAST('<li>' AS NVARCHAR(MAX))+CONVERT(nvarchar(max), x.NumberOfAttempts)+CAST(' - '+x.Details AS nvarchar(max))+CAST(char(13)+char(10) AS nvarchar(max))+CAST('</li>' AS NVARCHAR(MAX))
			from (
			SELECT COUNT (TEXT) AS NumberOfAttempts, TEXT AS Details, MIN(LogDate) AS MinLogDate, MAX(LogDate) AS MaxLogDate
				FROM @ErrorLogInfo
				WHERE ProcessInfo = 'Logon' --+char(39)+"
				AND TEXT LIKE '%fail%' --+char(39)+"
				AND LogDate > getdate() - 1
				GROUP BY TEXT) x;
			
			SELECT @TXTOUT = @TXTOUT+CAST('</ul>' AS NVARCHAR(MAX));
		END;
		

   SET NOCOUNT OFF;
END;              
