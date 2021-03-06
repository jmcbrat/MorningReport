/****** Object:  StoredProcedure [dbo].[usp_emailMorningDBStatus]    Script Date: 6/29/2020 2:32:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_emailMorningDBStatus') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_emailMorningDBStatus AS RETURN 0;');
GO

ALTER  PROC [dbo].[usp_emailMorningDBStatus] 
AS

/*******************************************************************
--
--Create 3/29/2020 JMcBratnie
-- Email the results of checks on the database nightly
--
********************************************************************/
BEGIN
	DECLARE @output NVARCHAR(MAX); 
	DECLARE @body NVARCHAR(MAX);
	DECLARE @subject NVARCHAR(MAX);

	-- initialize vars
	SELECT @body = CAST('' AS NVARCHAR(MAX));
	SELECT @output = CAST('' AS NVARCHAR(MAX));
	SELECT @subject = CAST('' AS NVARCHAR(MAX));


	-- Get nightly failed login attempts
	EXEC [IT.Macomb_DBA].dbo.usp_GetFailedLogins @TXTOUT = @output OUTPUT;
	--store it for email
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;

	-- reset var
	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- What services are running and not
	EXEC [IT.Macomb_DBA].dbo.usp_GetServices @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL
		BEGIN
			SELECT @body = @body + @output;
		END;

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	--Who is an admin of the server
	EXEC [IT.Macomb_DBA].dbo.usp_Getadmins @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- Did sql puke on me
	EXEC [IT.Macomb_DBA].dbo.usp_Getdumps @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- What servers are linked to this server
	EXEC [IT.Macomb_DBA].dbo.usp_GetLinked @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	--Are the backups old?
	EXEC [IT.Macomb_DBA].dbo.usp_Getstalebackups @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- what precedures are running on start (might haveduplicates here)
	-- Also, might want to check jobs for start up
	EXEC [IT.Macomb_DBA].dbo.usp_GetStartups @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;
	

	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- Might want to list jobs that failed in the past 24 hours

	-- Who needs a fill tactor adjustment
	EXEC [IT.Macomb_DBA].dbo.usp_GetFILLFactor @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;
	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- What jobs failed
	EXEC [IT.Macomb_DBA].dbo.usp_GetJobsFailed @TXTOUT = @output output;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;
	SELECT @output = CAST('' AS NVARCHAR(MAX));


	--What jobs are not enabled
	EXEC [IT.Macomb_DBA].dbo.usp_GetJobsNotEnabled @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END
	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- Enabled but scheduled not enabled
	EXEC [IT.Macomb_DBA].dbo.usp_GetJobsscheduleNotEnabled @TXTOUT = @output OUTPUT;
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END;
	SELECT @output = CAST('' AS NVARCHAR(MAX));

	-- Enabled jobs with not schedule
	EXEC [IT.Macomb_DBA].dbo.usp_GetJobsnotscheduled @TXTOUT = @output output
	IF @output IS NOT NULL 
		BEGIN
			SELECT @body = @body + @output;
		END 
	SELECT @output = CAST('' AS NVARCHAR(MAX));

	/*--DEBUG CODE: What does the body look like?  Comment this out on production
	SELECT @body;
	--*/

	SELECT @subject = CAST('Morning issue report - ' AS NVARCHAR(Max)) + @@SERVERNAME;


	-- Finally, SEND Email
	EXEC msdb.dbo.sp_send_dbmail @profile_name = 'SMTP',
	@recipients = 'Macomb DBA <macomb-dba@macombgov.org>',
	@subject = @subject, 
	@body = @body,
	@body_format = 'HTML',
	@from_address='<NOREPLY@macombgov.org>';
	--*/
END;
