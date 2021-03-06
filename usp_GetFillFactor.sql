/****** Object:  StoredProcedure [dbo].[usp_GetFillFactor]    Script Date: 6/29/2020 2:48:17 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_GetFillFactor') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_GetFillFactor AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_GetFillFactor] (
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
