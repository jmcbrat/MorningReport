/****** Object:  StoredProcedure [dbo].[usp_GetJobsFailed]    Script Date: 6/29/2020 2:48:19 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsFailed') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsFailed AS RETURN 0;');
GO

-- jobs that failed in the past 24 hours.  
-- Need to convert this to generate an html email and email the DBA group
ALTER PROC [dbo].[usp_GetJobsFailed] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 4/1/2020 JmcBratnie
-- jobs that failed in the past 24 hours.
--
********************************************************************/

BEGIN

    -- if no data skip output file
    IF NOT EXISTS(SELECT 'X'
						FROM msdb..sysjobhistory T1 
						  INNER JOIN msdb..sysjobs T2 ON T1.job_id = T2.job_id
						WHERE T1.run_status NOT IN (1, 4)
						  AND T1.step_id != 0
						  AND run_date >= CONVERT(CHAR(8), (SELECT DATEADD (DAY,(-1), GETDATE())), 112)  )
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2>NO Jobs Failed: </h2>' AS NVARCHAR(MAX));
		END
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2>Failed Jobs:</h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<table><tr>
				<th>Server Name</th>
				<th>Step ID</th>
				<th>Step Name</th>
				<th>Job Name</th>
				<th>Run Date</th>
				<th>Duration</th>
				<th>Status</th>
				<th>Message</th></tr>' AS NVARCHAR(MAX));

			SELECT DISTINCT @TXTOUT = @TXTOUT + CAST('<tr><td>' AS NVARCHAR(MAX)) + CAST(T1.server AS NVARCHAR(MAX)) + CAST('</td><td>'  AS NVARCHAR(MAX))
				+ CONVERT(NVARCHAR(MAX),T1.step_id) 
				+ CAST('</td><td>' + T1.step_name + '</td><td>'
				+ SUBSTRING(T2.name,1,140) + '</td><td>' AS NVARCHAR(MAX)) 
				+ CONVERT(NVARCHAR(MAX), msdb.dbo.agent_datetime(run_date, run_time), 22)
				+ CAST('</td><td>' AS NVARCHAR(MAX))
				+ CONVERT(NVARCHAR(MAX), T1.run_duration) 
				+ CAST('</td><td>' 
				+ CASE T1.run_status
					WHEN 0 THEN 'Failed'
					WHEN 1 THEN 'Succeeded'
					WHEN 2 THEN 'Retry'
					WHEN 3 THEN 'Cancelled'
					WHEN 4 THEN 'In Progress'
					END +'</td><td>'
				+ T1.message + '</td></tr>' AS NVARCHAR(MAX))
			FROM
			msdb..sysjobhistory T1 INNER JOIN msdb..sysjobs T2 ON T1.job_id = T2.job_id
			WHERE
			T1.run_status NOT IN (1, 4)
			AND T1.step_id != 0
			AND run_date >= CONVERT(CHAR(8), (SELECT DATEADD (DAY,(-1), GETDATE())), 112)
			SELECT @TXTOUT = @TXTOUT + CAST('</table>' AS NVARCHAR(MAX));

		END;
END;
