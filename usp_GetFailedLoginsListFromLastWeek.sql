/****** Object:  StoredProcedure [dbo].[usp_GetFailedLoginsListFromLastWeek]    Script Date: 6/29/2020 2:48:13 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFailedLoginsListFromLastWeek') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFailedLoginsListFromLastWeek AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetFailedLoginsListFromLastWeek]
AS
BEGIN
   SET NOCOUNT ON;

   DECLARE @ErrorLogCount INT; 
   DECLARE @LastLogDate DATETIME;

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
   FROM @EnumErrorLogs

   WHILE @ErrorLogCount IS NOT NULL
   BEGIN

      INSERT INTO @ErrorLogInfo
      EXEC sp_readerrorlog @ErrorLogCount;

      SELECT @ErrorLogCount = MIN([Archive#]), @LastLogDate = MAX([Date])
      FROM @EnumErrorLogs
      WHERE [Archive#] > @ErrorLogCount
      AND @LastLogDate > getdate() - 7; 
  
   END;

   -- List all last week failed logins count of attempts and the Login failure message
   SELECT COUNT (TEXT) AS NumberOfAttempts, TEXT AS Details, MIN(LogDate) AS MinLogDate, MAX(LogDate) AS MaxLogDate
   FROM @ErrorLogInfo
   WHERE ProcessInfo = 'Logon'
      AND TEXT LIKE '%fail%'
      AND LogDate > getdate() - 7
   GROUP BY TEXT
   ORDER BY NumberOfAttempts DESC;

   SET NOCOUNT OFF;
END;              
