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
ALTER PROC [dbo].[usp_GetLinked] (
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
