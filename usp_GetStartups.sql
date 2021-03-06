/****** Object:  StoredProcedure [dbo].[usp_GetStartups]    Script Date: 6/29/2020 2:48:47 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetStartups') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetStartups AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetStartups] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- email a script what runs at start up.
-- Might need to be adjusted to handle the jobs that run at startup.
--
********************************************************************/

BEGIN
	-- resolve issue with collation on the databases. Why we cant standize this??
	CREATE TABLE [dbo].[#TMP] 
	(
		[ord]             INT,
		[NAME]          NVARCHAR(256) NULL,
		[type_desc]       NVARCHAR(MAX) NULL,
		[Create_Date]     NVARCHAR(22) NULL,
		[Modify_Date]     NVARCHAR(22) NULL 
	);

    IF NOT EXISTS(SELECT 'x'
						  FROM master.sys.procedures
						WHERE is_auto_executed = 1
						union 
						SELECT 'x' 
						FROM sysobjects
						WHERE type = 'P'
						  AND OBJECTPROPERTY(id, 'ExecIsStartUp') = 1
						UNION 
						SELECT 'x'
							FROM msdb.dbo.sysschedules sched
							  JOIN msdb.dbo.sysjobschedules jsched 
								 ON sched.schedule_id = jsched.schedule_id
							  JOIN msdb.dbo.sysjobs j 
								 ON jsched.job_id = j.job_id
							WHERE sched.freq_type = 64)
		BEGIN
			SELECT @TXTOUT =  CAST('<h2> NO Procedures that run at start-up: </h2>' AS NVARCHAR(MAX));
			PRINT 'No matching row exists';
		END;
	ELSE
		BEGIN

			INSERT INTO #TMP
				SELECT 1, CAST(name AS NVARCHAR(MAX)) AS name, CAST(type_desc AS NVARCHAR(MAX)) AS type_desc, CONVERT(NVARCHAR(MAX),create_date,22), CONVERT(NVARCHAR(MAX),modify_date,22)
				FROM Master.sys.procedures
				WHERE is_auto_executed = 1
				ORDER BY 3,2;

			INSERT INTO #TMP
				SELECT 2, [name] , CAST('Stored Proc' AS NVARCHAR(MAX)), CAST('' AS NVARCHAR(MAX)),CAST('' AS NVARCHAR(MAX))
				FROM sysobjects
				WHERE type = 'P'
				  AND OBJECTPROPERTY(id, 'ExecIsStartUp') = 1
				ORDER BY 3,2;

			INSERT INTO #TMP
				SELECT 3,CAST(j.name AS NVARCHAR(MAX)) , CAST('JOB' AS NVARCHAR(MAX)) , CAST('' AS NVARCHAR(MAX)),CAST('' AS NVARCHAR(MAX)) 
					FROM msdb.dbo.sysschedules sched
					  JOIN msdb.dbo.sysjobschedules jsched 
						 ON sched.schedule_id = jsched.schedule_id
					  JOIN msdb.dbo.sysjobs j 
						 ON jsched.job_id = j.job_id
					WHERE sched.freq_type = 64
					ORDER BY 3,2;

		END;

		IF (SELECT count('X') FROM #TMP)>0 
		BEGIN
					-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2>Procedures that run at start-up: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<table><tr><th>Job Name</th><th>Object Type</th><th>Create Date</th><th>Modified Date</th></tr>' AS NVARCHAR(MAX));

			SELECT @TXTOUT = @TXTOUT 
				+ CAST('<tr><td>' AS NVARCHAR(MAX)) 
				+ tt.NAME + CAST('</td><td>' AS NVARCHAR(MAX)) 
				+ tt.type_desc + CAST('</td><td>' AS NVARCHAR(MAX)) 
				+ tt.Create_Date + CAST('</td><td>' AS NVARCHAR(MAX)) 
				+ tt.Modify_Date + CAST('</td></tr>' AS NVARCHAR(MAX)) 
			FROM #TMP tt 
			ORDER BY tt.ord, tt.type_desc, tt.NAME;

			SELECT @TXTOUT = @TXTOUT + CAST('</table><P>May include duplicates</p>' AS NVARCHAR(MAX));

		END;
	DROP TABLE #TMP;
END;
