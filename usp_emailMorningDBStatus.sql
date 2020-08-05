USE [IT.Macomb_DBA]
GO
/****** Object:  StoredProcedure [dbo].[usp_emailMorningDBStatus]    Script Date: 8/3/2020 1:43:31 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROC [dbo].[usp_emailMorningDBStatus] 
AS

/*******************************************************************
--
--Create 3/29/2020 JMcBratnie
-- Email the results of checks on the database nightly
--
********************************************************************/
BEGIN
declare @output NVARCHAR(MAX); 
declare @body NVARCHAR(MAX);
declare @subject NVARCHAR(MAX);

-- initialize vars
select @body = cast('' AS NVARCHAR(MAX));
select @output = cast('' AS NVARCHAR(MAX));
select @subject = cast('' AS NVARCHAR(MAX));

-- Get any databases that are not in a good state
exec [IT.Macomb_DBA].[dbo].[usp_GetDBHealth] @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

-- Get nightly failed login attempts
exec [IT.Macomb_DBA].dbo.usp_GetFailedLogins @TXTOUT = @output output;
--store it for email
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

-- reset var
select @output = cast('' AS NVARCHAR(MAX));

-- What services are running and not
exec [IT.Macomb_DBA].dbo.usp_GetServices @TXTOUT = @output output;
if @output IS NOT NULL
	BEGIN
		select @body = @body + @output;
	END

select @output = cast('' AS NVARCHAR(MAX));

--Who is an admin of the server
exec [IT.Macomb_DBA].dbo.usp_Getadmins @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

select @output = cast('' AS NVARCHAR(MAX));

-- Did sql puke on me
exec [IT.Macomb_DBA].dbo.usp_Getdumps @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

select @output = cast('' AS NVARCHAR(MAX));

-- What servers are linked to this server
exec [IT.Macomb_DBA].dbo.usp_GetLinked @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

select @output = cast('' AS NVARCHAR(MAX));

--Are the backups old?
exec [IT.Macomb_DBA].dbo.usp_Getstalebackups @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END

select @output = cast('' AS NVARCHAR(MAX));

-- what precedures are running on start (might haveduplicates here)
-- Also, might want to check jobs for start up
exec [IT.Macomb_DBA].dbo.usp_GetStartups @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END
	

select @output = cast('' AS NVARCHAR(MAX));

-- Might want to list jobs that failed in the past 24 hours

-- Who needs a fill tactor adjustment
exec [IT.Macomb_DBA].dbo.usp_GetFILLFactor @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END
select @output = cast('' AS NVARCHAR(MAX));

-- What jobs failed
exec [IT.Macomb_DBA].dbo.usp_GetJobsFailed @TXTOUT = @output output
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END
select @output = cast('' AS NVARCHAR(MAX));


--What jobs are not enabled
exec [IT.Macomb_DBA].dbo.usp_GetJobsNotEnabled @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END
select @output = cast('' AS NVARCHAR(MAX));

-- Enabled but scheduled not enabled
exec [IT.Macomb_DBA].dbo.usp_GetJobsscheduleNotEnabled @TXTOUT = @output output;
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END
select @output = cast('' AS NVARCHAR(MAX));

-- Enabled jobs with not schedule
exec [IT.Macomb_DBA].dbo.usp_GetJobsnotscheduled @TXTOUT = @output output
if @output IS NOT NULL 
	BEGIN
		select @body = @body + @output;
	END 
select @output = cast('' AS NVARCHAR(MAX));

/*--What does the body look like?  Comment this out on production
select @body;
--*/

Select @subject = cast('Morning issue report - ' AS NVARCHAR(Max)) + @@SERVERNAME;


-- Finally, Send Email
EXEC msdb.dbo.sp_send_dbmail @profile_name = 'SMTP',
@recipients = 'Macomb DBA <macomb-dba@macombgov.org>',
@subject = @subject, 
@body = @body,
@body_format = 'HTML',
@from_address='<NOREPLY@macombgov.org>';
--*/
end