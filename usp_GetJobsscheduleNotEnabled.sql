
/****** Object:  StoredProcedure [dbo].[usp_GetJobsscheduleNotEnabled]    Script Date: 6/29/2020 2:48:29 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsscheduleNotEnabled') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsscheduleNotEnabled AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetJobsscheduleNotEnabled] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 4/1/2020 JmcBratnie
-- jobs that are enabled but are not scheduled
--
********************************************************************/

BEGIN

    -- if no data skip output file
    IF NOT EXISTS(SELECT 'X'
							FROM MSDB.DBO.sysjobs sj2
								right join (
						SELECT sj.job_id, 'Job enabled but scheduled not enabled' AS issue --'sj', sj.*, 'sjs',sjs.*,'ss',ss.* 
						FROM MSDB.DBO.sysjobs sj
							 left join msdb.dbo.sysjobschedules sjs ON sjs.job_id = sj.job_id
							 left join msdb.dbo.sysschedules ss ON ss.schedule_id = sjs.schedule_id
						WHERE sj.enabled = 1
						  and ss.enabled = 0
								) jj ON jj.job_id = sj2.job_id )
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2>ALL jobs are scheduled:</h2>' AS NVARCHAR(MAX));
		END
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2>Job enabled but not scheduled: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<table><tr>
				<th>Job Name</th>
				<th>Description</th>
				<th>Created</th>
				<th>Modified</th>
				</tr>' AS NVARCHAR(MAX));

			SELECT @TXTOUT = @TXTOUT + CAST('<tr><td>' 
												+ sj2.name + '</td><td>' 
												+ sj2.description + '</td><td>' AS NVARCHAR(MAX)) 
											+ CONVERT(NVARCHAR(MAX), sj2.date_created,22) + CAST('</td><td>' AS NVARCHAR(MAX))
											+ CONVERT(NVARCHAR(MAX), sj2.date_modified,22) + CAST('</td></tr>' AS NVARCHAR(MAX))
			FROM MSDB.DBO.sysjobs sj2
				right join (
		SELECT sj.job_id, 'Job enabled but scheduled not enabled' AS issue --'sj', sj.*, 'sjs',sjs.*,'ss',ss.* 
		FROM MSDB.DBO.sysjobs sj
			 left join msdb.dbo.sysjobschedules sjs ON sjs.job_id = sj.job_id
			 left join msdb.dbo.sysschedules ss ON ss.schedule_id = sjs.schedule_id
		WHERE sj.enabled = 1
		  and ss.enabled = 0
				) jj ON jj.job_id = sj2.job_id
				ORDER BY issue, name, date_modified;

			SELECT @TXTOUT = @TXTOUT + CAST('</table>' AS NVARCHAR(MAX));
		END;
END;
