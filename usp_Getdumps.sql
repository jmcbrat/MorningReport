/****** Object:  StoredProcedure [dbo].[usp_Getdumps]    Script Date: 6/29/2020 2:48:07 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getdumps') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getdumps AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_Getdumps] (
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
