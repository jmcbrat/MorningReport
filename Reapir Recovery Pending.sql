ALTER DATABASE [SH.JailReimbursement] set single_user
GO

DBCC CHECKDB ([CCVetsTrtmnt], REPAIR_ALLOW_DATA_LOSS) WITH ALL_ERRORMSGS;
GO 

ALTER DATABASE [CCVetsTrtmnt] set multi_user
GO