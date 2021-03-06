/****** Object:  StoredProcedure [dbo].[usp_GetJobsnotscheduled]    Script Date: 6/29/2020 2:48:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsnotscheduled') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsnotscheduled AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetJobsnotscheduled] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 4/1/2020 JmcBratnie
-- enabled jobs without schedule
--
********************************************************************/

BEGIN

    -- if no data skip output file
    IF NOT EXISTS(SELECT 'X'
							FROM MSDB.DBO.sysjobs sj2
								right join (
								-- Jobs in either state with out a schedule
								SELECT sj.job_id, 'Active job without a schedule' AS issue --'sj', sj.*, 'sjs',sjs.*,'ss',ss.* 
								FROM MSDB.DBO.sysjobs sj
										left join msdb.dbo.sysjobschedules sjs ON sjs.job_id = sj.job_id
										--left join msdb.dbo.sysschedules ss on ss.schedule_id <> sjs.schedule_id
								WHERE sj.enabled = 1
									and not exists (SELECT 1 FROM msdb.dbo.sysschedules ss WHERE ss.schedule_id =sjs.schedule_id)
							) jj ON jj.job_id = sj2.job_id )
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2>ALL jobs enabled and scheduled:</h2>' AS NVARCHAR(MAX));
		END;
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2>Job enabled with no scheduled: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<table><tr>
				<th>Job Name</th>
				<th>Description</th>
				<th>Created</th>
				<th>Modified</th>
				</tr>' AS NVARCHAR(MAX));

			SELECT @TXTOUT = @TXTOUT + 
			CAST('<tr><td>' 
												+ sj2.name + '</td><td>' 
												+ sj2.description + '</td><td>' AS NVARCHAR(MAX)) 
											+ CONVERT(NVARCHAR(MAX), sj2.date_created,22) + CAST('</td><td>' AS NVARCHAR(MAX))
											+ CONVERT(NVARCHAR(MAX), sj2.date_modified,22) + CAST('</td></tr>' AS NVARCHAR(MAX))
			FROM MSDB.DBO.sysjobs sj2
				 inner join (
					-- Jobs in either state with out a schedule
				SELECT sj.job_id, 'Active job without a schedule' AS issue--'sj', sj.*, 'sjs',sjs.*,'ss',ss.* 
				FROM MSDB.DBO.sysjobs sj
					 left join msdb.dbo.sysjobschedules sjs ON sjs.job_id = sj.job_id
					 --left join msdb.dbo.sysschedules ss on ss.schedule_id <> sjs.schedule_id
				WHERE sj.enabled = 1
				  and not exists (SELECT 1 FROM msdb.dbo.sysschedules ss WHERE ss.schedule_id =sjs.schedule_id)
				) jj ON jj.job_id = sj2.job_id
				ORDER BY issue, name, date_modified;

			SELECT @TXTOUT = @TXTOUT + CAST('</table>' AS NVARCHAR(MAX));

		END
END
