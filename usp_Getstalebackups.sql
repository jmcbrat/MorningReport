/****** Object:  StoredProcedure [dbo].[usp_Getstalebackups]    Script Date: 6/29/2020 2:48:39 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getstalebackups') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getstalebackups AS RETURN 0;');
GO

------------------------------------------------------------------------------------------- 
--Databases Missing a Data (aka Full) Back-Up Within Past 24 Hours 
------------------------------------------------------------------------------------------- 
--Databases with data backup over 24 hours old 
ALTER PROC [dbo].[usp_Getstalebackups] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- Check all the database for old backups that have not ran.
--
********************************************************************/

BEGIN
    -- only create data if backup are old
    IF NOT EXISTS(SELECT 'X' 
					FROM    msdb.dbo.backupset 
					WHERE     msdb.dbo.backupset.type = 'D'  
					GROUP BY msdb.dbo.backupset.database_name 
					HAVING      (MAX(msdb.dbo.backupset.backup_finish_date) < DATEADD(hh, - 24, GETDATE()))  )
		BEGIN
			SELECT @TXTOUT =  CAST('<h2> NO Stale Backups: </h2>' AS NVARCHAR(MAX));
			PRINT 'No matching row exists';
		END
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2> Stale Backups: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<ul>' AS NVARCHAR(MAX));

			SELECT @TXTOUT = @TXTOUT +CAST('<li>' AS NVARCHAR(MAX)) + CAST(x.server AS NVARCHAR(MAX)) + CAST(' ' AS NVARCHAR(MAX)) +  CAST(x.database_name AS NVARCHAR(MAX)) + CAST(' hours old: ' AS NVARCHAR(MAX)) + CONVERT(NVARCHAR(MAX),x.Backup_Age_hours) + CAST('</li>' AS NVARCHAR(Max))
			FROM (
				SELECT 
				   CONVERT(CHAR(100), SERVERPROPERTY('Servername')) AS Server, 
				   msdb.dbo.backupset.database_name, 
				   MAX(msdb.dbo.backupset.backup_finish_date) AS last_db_backup_date, 
				   DATEDIFF(hh, MAX(msdb.dbo.backupset.backup_finish_date), GETDATE()) AS [Backup_Age_hours] 
				FROM    msdb.dbo.backupset 
				WHERE     msdb.dbo.backupset.type = 'D'  
				GROUP BY msdb.dbo.backupset.database_name 
				HAVING      (MAX(msdb.dbo.backupset.backup_finish_date) < DATEADD(hh, - 24, GETDATE()))  

				UNION  

				--Databases without any backup history 
				SELECT      
				   CONVERT(CHAR(100), SERVERPROPERTY('Servername')) AS Server,  
				   sd.NAME AS database_name,  
				   NULL AS [Last Data Backup Date],  
				   9999 AS [Backup Age (Hours)]  
				FROM 
				   master.dbo.sysdatabases sd LEFT JOIN msdb.dbo.backupset bs
					   ON sd.name  = bs.database_name 
				WHERE bs.database_name IS NULL AND sd.name <> 'tempdb' ) x
				ORDER BY  database_name;
				
   			SELECT @TXTOUT = @TXTOUT + CAST('</ul>' AS NVARCHAR(MAX));

		END;

END;
