/****** Object:  StoredProcedure [dbo].[usp_Getadmins]    Script Date: 6/29/2020 2:48:04 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getadmins') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getadmins AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_Getadmins] (
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
