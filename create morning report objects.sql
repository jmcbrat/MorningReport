USE [IT.Macomb_DBA]
/****** Object:  StoredProcedure [dbo].[usp_GetStartups]    Script Date: 6/29/2020 2:48:47 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetStartups') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetStartups AS RETURN 0;');
GO

create PROC [dbo].[usp_GetStartups] (
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
CREATE PROC [dbo].[usp_Getstalebackups] (
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

/****** Object:  StoredProcedure [dbo].[usp_Getservices]    Script Date: 6/29/2020 2:48:36 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getservices') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getservices AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_Getservices] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- What services are active and are they running
--
********************************************************************/

BEGIN

	SET NOCOUNT ON;
	
	-- Temp table to store the data.
	CREATE TABLE #ServicesStatus
	( 
	myid int identity(1,1),
	serverName nvarchar(100) default @@serverName,
	serviceName varchar(100),
	Status varchar(50),
	checkdatetime datetime default (getdate())
	);
	
	-- Collect the data
	INSERT #ServicesStatus (Status)
	EXEC xp_servicecontrol N'QUERYSTATE',N'MSSQLServer';
	update #ServicesStatus set serviceName = 'MSSQLServer' where myid = @@identity;
	INSERT #ServicesStatus (Status)
	EXEC xp_servicecontrol N'QUERYSTATE',N'SQLServerAGENT';
	update #ServicesStatus set serviceName = 'SQLServerAGENT' where myid = @@identity;
	INSERT #ServicesStatus (Status)
	EXEC xp_servicecontrol N'QUERYSTATE',N'msdtc';
	update #ServicesStatus set serviceName = 'msdtc' where myid = @@identity;
	INSERT #ServicesStatus (Status)
	EXEC xp_servicecontrol N'QUERYSTATE',N'sqlbrowser';
	update #ServicesStatus set serviceName = 'sqlbrowser' where myid = @@identity;

    -- Generate the output 	
	SELECT @TXTOUT =  CAST('<h2> SQL Services: </h2>' AS NVARCHAR(MAX));
	SELECT @TXTOUT = @TXTOUT + CAST('<ol>' AS NVARCHAR(MAX));
	SELECT @TXTOUT = @TXTOUT + CAST('<li>' AS NVARCHAR(MAX)) + CAST(serverName AS NVARCHAR(MAX)) + CAST(' (' AS NVARCHAR(MAX)) + CAST(serviceName AS NVARCHAR(MAX)) + CAST(') : ' AS NVARCHAR(MAX)) + CAST(status AS NVARCHAR(MAX)) + CAST('</li>' AS NVARCHAR(MAX))
	  FROM #ServicesStatus; 
	SELECT @TXTOUT = @TXTOUT + CAST('</ol>' AS NVARCHAR(MAX));

	DROP TABLE #ServicesStatus;
END;

/****** Object:  StoredProcedure [dbo].[usp_GetLinked]    Script Date: 6/29/2020 2:48:33 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetLinked') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetLinked AS RETURN 0;');
GO

--------------------------------------------------------------------------------------- 
--Linked servers listing 
------------------------------------------------------------------------------------------- 
--Databases with data backup over 24 hours old 
CREATE PROC [dbo].[usp_GetLinked] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- List of linked servers 
-- note two columns were null on my test might need to add them in
--
********************************************************************/

BEGIN
    -- if no data skip output file
    IF NOT EXISTS(SELECT 'X'
					  FROM sys.Servers ss 
				 LEFT JOIN sys.linked_logins sl 
						ON ss.server_id = sl.server_id 
				 LEFT JOIN sys.server_principals ssp 
						ON ssp.principal_id = sl.local_principal_id  )
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2> NO Linked Servers: </h2>' AS NVARCHAR(MAX));
		END;
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2> Linked Servers: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<table><tr><th>Server ID</th><th>Name</th><th>Type</th><th>Product</th><th>Providor</th><th>Catalog</th><th>Local Login</th><th>Remote Login</th><th>
			       RPC Enabled</th><th>Data Access</th><th>Modified</th></tr>' AS NVARCHAR(MAX));  --TABLE TITLE



			SELECT @TXTOUT = @TXTOUT + 
			CAST('<tr><td>' AS NVARCHAR(MAX)) 
			+ CONVERT(NVARCHAR(MAX),ss.server_id) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(ss.name AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(Case ss.Server_id 
						when 0 then 'Current Server' 
						else 'Remote Server' 
						end AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(isnull(ss.product,' ') AS  NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(ss.provider AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CONVERT(NVARCHAR(MAX), isnull(ss.catalog, ' ') ) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(case isnull(sl.uses_self_credential, ' ') 
						when 1 then 'Uses Self Credentials'
						when ' ' then ' '
						else ssp.name 
						end AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(isnull(sl.remote_name, ' ') AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) -- NULL FAILS....
			+ CAST(case ss.is_rpc_out_enabled 
						when 1 then 'True' 
						else 'False' 
						end  AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CAST(case ss.is_data_access_enabled 
						when 1 then 'True' 
						else 'False' 
						end AS NVARCHAR(MAX))+ CAST('</td><td>' AS NVARCHAR(MAX)) 
			+ CONVERT(NVARCHAR(MAX), ss.modify_date, 22) + CAST('</td></tr>' AS NVARCHAR(Max)) 
			FROM sys.Servers ss 
			 LEFT JOIN sys.linked_logins sl 
				ON ss.server_id = sl.server_id 
			 LEFT JOIN sys.server_principals ssp 
				ON ssp.principal_id = sl.local_principal_id;
			--where is_linked = 1 -- Need to run this on a work server to see if it is needed



   			SELECT @TXTOUT = @TXTOUT + CAST('</table>' AS NVARCHAR(MAX));
			print @TXTOUT;

		END;

END;


/****** Object:  StoredProcedure [dbo].[usp_GetJobsscheduleNotEnabled]    Script Date: 6/29/2020 2:48:29 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsscheduleNotEnabled') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsscheduleNotEnabled AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetJobsscheduleNotEnabled] (
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

/****** Object:  StoredProcedure [dbo].[usp_GetJobsnotscheduled]    Script Date: 6/29/2020 2:48:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsnotscheduled') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsnotscheduled AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetJobsnotscheduled] (
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


/****** Object:  StoredProcedure [dbo].[usp_GetJobsNotEnabled]    Script Date: 6/29/2020 2:48:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetJobsNotEnabled') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetJobsNotEnabled AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetJobsNotEnabled] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 4/1/2020 JmcBratnie
-- jobs that are not enabled potentially are useless
-- Too many cooks in the kitchen
--
********************************************************************/

BEGIN

    -- if no data skip output file
    IF NOT EXISTS(SELECT 'X'
						FROM MSDB.DBO.sysjobs sj2
							right join (
								SELECT sj.job_id, 'Job not enabled' AS issue
								FROM MSDB.DBO.sysjobs sj
								WHERE sj.enabled = 0  )
							 jj ON jj.job_id = sj2.job_id )
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2>ALL jobs are enabled:</h2>' AS NVARCHAR(MAX));
		END;
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2>Jobs that are not enabled: </h2>' AS NVARCHAR(MAX));
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
					SELECT sj.job_id, 'Jobs not enabled' AS issue
					FROM MSDB.DBO.sysjobs sj
					WHERE sj.enabled = 0
				) jj ON jj.job_id = sj2.job_id
				ORDER BY issue, name, date_modified;

			SELECT @TXTOUT = @TXTOUT + CAST('</table>' AS NVARCHAR(MAX));

		END;
END;

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
CREATE PROC [dbo].[usp_GetJobsFailed] (
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

/****** Object:  StoredProcedure [dbo].[usp_GetFillFactor]    Script Date: 6/29/2020 2:48:17 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFillFactor') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFillFactor AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetFillFactor] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- email a script of indexes that need to have the fill factor adjusted
--
********************************************************************/

BEGIN
   declare @command Nvarchar(MAX);
	declare @rowcount AS int;
        --this has to be ran on each db.  Store the results here
	CREATE TABLE [dbo].[#TMP] (
	[DBNAME]     NVARCHAR(256) NULL,
	[CMD]       NVARCHAR(MAX) NULL);


                -- the command to be ran on each db
                -- use ? is the current DB
                -- can the command be skiped for this db?
		SELECT @command = 
		'USE [?] 
		IF NOT EXISTS(SELECT ''X''
		FROM sys.indexes i 
			join sys.objects o on i.object_id = o.object_id
			join sys.schemas s on o.schema_id = s.schema_id
			--join sys.databases d on i.object_id = d.
		WHERE fill_factor < 80 AND fill_factor <> 0
		AND is_disabled = 0 AND is_hypothetical = 0)
			BEGIN
				PRINT ''No matching row exists''
			END
		ELSE
			INSERT INTO #TMP 
			SELECT CAST(''[''+DB_NAME()+'']'' AS NVARCHAR(MAX)), CAST(''ALTER INDEX ''+i.name+ '' ON ''+s.name+ ''.''+o.name+'' REBUILD WITH (FILLFACTOR = 100);'' AS NVARCHAR(MAX))
			FROM sys.indexes i 
				join sys.objects o on i.object_id = o.object_id
				join sys.schemas s on o.schema_id = s.schema_id
				--join sys.databases d on i.object_id = d.
			WHERE fill_factor < 80 AND fill_factor <> 0
			AND is_disabled = 0 AND is_hypothetical = 0;';

	EXEC sp_MSforeachdb @command;
	-- 'ALTER INDEX '+i.name+ ' ON '+s.name+'.'+o.name+ 'REBUILD WITH (FILLFACTOR = 100); '

	-- Generate the output 	
	SELECT @TXTOUT =  CAST('<h2> FillFactor Repairs: </h2><ul style="list-style-type:none;">' AS NVARCHAR(MAX));

	SELECT @rowcount = count('x') FROM #TMP;
	if @rowcount >0  
	BEGIN

	SELECT @TXTOUT = @TXTOUT + CAST('<li>' AS NVARCHAR(MAX)) + tt.DBNAME + CAST(' - ' AS NVARCHAR(MAX)) + tt.CMD + CAST('</li>' AS NVARCHAR(MAX)) FROM #TMP tt;
	SELECT @TXTOUT = @TXTOUT + '</ul>';

	END;
	ELSE
	BEGIN
		SELECT @TXTOUT = @TXTOUT + CAST('<li>' AS NVARCHAR(MAX)) + CAST(' NONE ' AS NVARCHAR(MAX)) + CAST('</li>' AS NVARCHAR(MAX)) ;
	SELECT @TXTOUT = @TXTOUT + '</ul>';
	END;

	drop table #TMP;
END;

/****** Object:  StoredProcedure [dbo].[usp_GetFailedLoginsListFromLastWeek]    Script Date: 6/29/2020 2:48:13 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFailedLoginsListFromLastWeek') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFailedLoginsListFromLastWeek AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetFailedLoginsListFromLastWeek]
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


/****** Object:  StoredProcedure [dbo].[usp_GetFailedLogins]    Script Date: 6/29/2020 2:48:10 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFailedLogins') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFailedLogins AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_GetFailedLogins] (
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

/****** Object:  StoredProcedure [dbo].[usp_Getdumps]    Script Date: 6/29/2020 2:48:07 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getdumps') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getdumps AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_Getdumps] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- Did SQL server have major issues and create a dump file
--
********************************************************************/

BEGIN
    -- if not data skip the output file
    IF NOT EXISTS(SELECT 'X' 
					FROM sys.dm_server_memory_dumps SMD
					WHERE SMD.creation_time > getdate() - 1)
		BEGIN
			PRINT 'No matching row exists';
			SELECT @TXTOUT =  CAST('<h2> NO Dumps have occured: </h2>' AS NVARCHAR(MAX));

		END;
	ELSE
		BEGIN
			-- Generate the output 	
			SELECT @TXTOUT =  CAST('<h2> Dumps have occured: </h2>' AS NVARCHAR(MAX));
			SELECT @TXTOUT = @TXTOUT + CAST('<ul>' AS NVARCHAR(MAX));

			SELECT @TXTOUT = @TXTOUT +CAST('<li>' AS NVARCHAR(MAX)) + CONVERT(nvarchar(20), SMD.creation_time, 22) + CAST(' ' AS NVARCHAR(MAX)) + CAST(SMD.size_in_bytes AS NVARCHAR(MAX)) + CAST(SMD.filename AS NVARCHAR(MAX)) + CAST('</li>' AS NVARCHAR(Max))
			FROM sys.dm_server_memory_dumps SMD
			WHERE SMD.creation_time > getdate() - 1;

			SELECT @TXTOUT = @TXTOUT + CAST('</ul>' AS NVARCHAR(MAX));

		END;
END; 

/****** Object:  StoredProcedure [dbo].[usp_Getadmins]    Script Date: 6/29/2020 2:48:04 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getadmins') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getadmins AS RETURN 0;');
GO

CREATE PROC [dbo].[usp_Getadmins] (
 --@output char(1),  -- Not needed on this style.
 @TXTOUT NVARCHAR(MAX) OUTPUT
 )
AS
/*******************************************************************
--
--Create 3/29/2020 JmcBratnie
-- Who has sysadmin or security Admin rights
--Update Corrected <h2>Title</h2> 
--
********************************************************************/

BEGIN
    -- if no data skip the output
    IF NOT EXISTS(SELECT 'x'
					FROM sys.server_role_members rm
						,sys.server_principals sp
					WHERE rm.role_principal_id = SUSER_ID('Sysadmin')
						AND rm.member_principal_id = sp.principal_id
				  UNION
				  SELECT 'x'
					FROM sys.server_role_members rm
						,sys.server_principals sp
					WHERE rm.role_principal_id = SUSER_ID('Securityadmin')
						AND rm.member_principal_id = sp.principal_id)
		BEGIN
			PRINT 'No matching row exists';
		END;
	ELSE

		-- Generate the output 	
		SELECT @TXTOUT =  CAST('<h2> Users with elevated privileges: </h2>' AS NVARCHAR(MAX));
		SELECT @TXTOUT = @TXTOUT + CAST('<table><tr><th>Access</th><th>User</th><th>Created</th><th>Modified</th><th></th></tr>' AS NVARCHAR(MAX));

                -- sysadmin
		SELECT @TXTOUT = @TXTOUT +CAST('<tr><td>' AS NVARCHAR(MAX)) 
		  + CAST(x.ACCESS AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
		  + CAST(x.Name AS NVARCHAR(MAX)) + CAST('</td><td>' AS NVARCHAR(MAX)) 
		  + x.create_date+ CAST('</td><td>' AS NVARCHAR(MAX)) 
		  + x.Modifed_Date + CAST('</td><td>' AS NVARCHAR(MAX)) 
		  + CAST(x.is_disabled AS NVARCHAR(MAX)) 
		  + CAST('</td></tr>' AS NVARCHAR(Max))
		from (
		SELECT 1 AS ord
			, 'SYSADMIN' AS 'ACCESS'
			, sp.NAME AS 'Name'
			, CONVERT(nvarchar(20), sp.create_date, 22) AS Create_Date
			, CONVERT(nvarchar(20), sp.modify_date, 22) AS Modifed_Date
			, CASE sp.is_disabled 
			  WHEN 1 THEN '*'
			  WHEN 0 THEN ''
			  ELSE 'E'
			  END AS [Is_disabled]

		FROM sys.server_role_members rm
			,sys.server_principals sp
		WHERE rm.role_principal_id = SUSER_ID('Sysadmin')
			AND rm.member_principal_id = sp.principal_id
		UNION
                -- Security admins
		SELECT 2 AS ord
			, 'SECURITYADMIN' AS 'ACCESS'
			, sp.NAME AS 'Name'
			, CONVERT(nvarchar(20), sp.create_date, 22) AS Create_Date
			, CONVERT(nvarchar(20), sp.modify_date, 22) AS Modifed_Date
			, CASE sp.is_disabled 
			  WHEN 1 THEN '*'
			  WHEN 0 THEN ''
			  ELSE 'E'
			  END AS [Is_disabled]
		FROM sys.server_role_members rm
			,sys.server_principals sp
		WHERE rm.role_principal_id = SUSER_ID('Securityadmin')
			AND rm.member_principal_id = sp.principal_id

		) x
		ORDER BY x.ord, x.Name;

		SELECT @TXTOUT = @TXTOUT + CAST('</table><p>* denotes disabled accounts</p>' AS NVARCHAR(MAX));

END;
