/****** Object:  StoredProcedure [dbo].[usp_Getservices]    Script Date: 6/29/2020 2:48:36 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Getservices') IS NULL
  EXEC ('CREATE PROCEDURE dbo.usp_Getservices AS RETURN 0;');
GO

ALTER PROC [dbo].[usp_Getservices] (
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
