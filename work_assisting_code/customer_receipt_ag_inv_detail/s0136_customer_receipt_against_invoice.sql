USE [SAPCONNECT]
GO
/****** Object:  StoredProcedure [dbo].[SAP_CUSTOMER_RECEIPT_AGAINST_INVOICE]    Script Date: 03-07-2024 13:45:28 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [dbo].[SAP_CUSTOMER_RECEIPT_AGAINST_INVOICE]
	(
	@COMPANY VARCHAR(10)='%'
	,@CUSTOMER VARCHAR(MAX)='%'
	 ,@FROM_DATE DATETIME
	,@TO_DATE DATETIME
	)
AS
--SAP_CUSTOMER_RECEIPT_AGAINST_INVOICE '23','4100008151'

DECLARE @SYDATE VARCHAR(10) = CONVERT(VARCHAR(10), GETDATE(),126)
EXEC('SET ''CDS_CLIENT''=''900''') AT SAP_GHP
EXEC('SET ''SAP_SYSTEM_DATE''='''+@SYDATE+'''') AT SAP_GHP 

IF @COMPANY in ('','0','%','''','All')
	BEGIN
		SET @COMPANY='23'
	END

IF @CUSTOMER IN ('ALL' ,' ')
		BEGIN 
		SET	@CUSTOMER ='%' 
		END
ELSE
		BEGIN 
			PRINT ('DATA FOUND')
		END
		
DECLARE @SQL AS VARCHAR(MAX)
SELECT @SQL = 
	'
SELECT 
	K.COMPANYCODE
	,K.PLANT
	,K.PLANTNAME
	,K.CUSTOMERCODE
	,K.CUSTOMERNAME 
	,TO_CHAR(K.POSTINGDATE,''DD/MM/YYYY'')  RECEIPTDATE
	,K.DOCUMENTNO RECEIPTDOCUMENTNO
	,K.CHEQUEAMOUNT RECEIPTAMOUNT
	,K.SDDOCNO SDINVOICENO
	,K.INVOICENO FIINVOICENO
	,K.BILLDATE INVOICEDATE
	,K.INVOICEPOSTINGDATE
	,IFNULL(K.AMOUNT,0.00) INVOICEAMOUNT
	,K.PAYMENTAMOUNT PAYMENTADJUSTAMT
	,IFNULL(DAYS_BETWEEN(K.DOCUMENTDATE,K.POSTINGDATE),0) NOOFDAYS
	
 FROM
	(
	SELECT
		 C.BUKRS COMPANYCODE
		,LEFT(B.PRCTR,4)PLANT
		,T.NAME1 PLANTNAME
		,C.BLART DOCUMENTTYPE
		,C.KUNNR CUSTOMERCODE
		,N.BPEXT NAVCODE
 		,K.NAME1 CUSTOMERNAME
		,IFNULL(K.STCD3,'''') CUSTOMERGSTNUMBER
		,K.REGIO CUSTOMERSTATECODE
		,T5.BEZEI  CUSTOMERSTATE_NAME
		,C.VBELN SDDOCNO
		,C.BELNR DOCUMENTNO
		,C.GJAHR FISCALYEAR
		,C.BUDAT POSTINGDATE
		,C.WRBTR PAYMENTAMOUNT
		,G.TXT50 BANKNAME
		,B.HKONT GLACC
		,TO_CHAR(C.BUDAT,''DD/MM/YYYY'') CHEQUEDATE
		,(CASE WHEN LENGTH(I.XBLNR)<=6 THEN I.XBLNR ELSE IFNULL(LEFT(I.BKTXT,6),LEFT(C.SGTXT,6)) END)  CHEQUENO
		,B.WRBTR CHEQUEAMOUNT
		,C.SGTXT REMARK
		,IFNULL(C.REBZG,'''') INVOICENO
		,IFNULL(C.VBELN,'''') BILLINGDOCNO
		,IFNULL(TO_CHAR(I.BLDAT,''DD/MM/YYYY''),'''') BILLDATE
		,(I.BLDAT) DOCUMENTDATE
		,IFNULL(TO_CHAR(I.BUDAT,''DD/MM/YYYY''),'''') INVOICEPOSTINGDATE
		,IFNULL(BL.AMOUNT,MN.AMOUNT)AMOUNT
		
	 FROM SAPABAP1.BSID C
		 LEFT JOIN (SELECT BUKRS,BELNR,REBZJ,BUZEI,SUM((CASE WHEN SHKZG=''S'' THEN IFNULL(WRBTR,0.00) ELSE IFNULL(WRBTR,0.00)*-1 END))AMOUNT FROM SAPABAP1.BSID  GROUP BY BUKRS,BELNR,REBZJ,BUZEI)BL ON C.BUKRS=BL.BUKRS AND C.GJAHR=BL.REBZJ AND C.REBZG=BL.BELNR AND C.REBZZ =BL.BUZEI
		 LEFT JOIN SAPABAP1.KNA1 K ON C.KUNNR=K.KUNNR
		 INNER JOIN SAPABAP1.BUT000 N ON K.MANDT=N.CLIENT AND K.KUNNR=N.PARTNER
		 LEFT JOIN (SELECT * FROM SAPABAP1.BSEG WHERE MANDT=900 AND  H_BLART IN (''DP'',''DZ'',''AB'',''DW'') AND PRCTR<>'''' AND MWSKZ='''') B ON 
			C.BUKRS=B.BUKRS AND C.GJAHR=B.GJAHR AND C.BELNR=B.BELNR
		LEFT JOIN (SELECT * FROM SAPABAP1.SKAT WHERE KTOPL=''LGIN'') G ON B.HKONT=G.SAKNR
		LEFT JOIN SAPABAP1.BKPF I ON C.BUKRS=I.BUKRS AND C.REBZJ=I.GJAHR  AND C.REBZG=I.BELNR
		LEFT JOIN  SAPABAP1.T005U T5 ON T5.BLAND=K.REGIO AND T5.SPRAS=''E'' AND T5.LAND1=''IN'' AND T5.MANDT=900
		LEFT JOIN SAPABAP1.T001W T ON T.WERKS=LEFT(B.PRCTR,4)
		LEFT JOIN (SELECT KUNNR,RBUKRS,BELNR,BUDAT,BLDAT,AWREF,GJAHR,MAX(LEFT(PRCTR,4)) PRCTR,SUM(HSL) AMOUNT FROM SAPABAP1.ACDOCA WHERE BELNR NOT LIKE ''B%''  AND KOART=''D''
			GROUP BY KUNNR ,RBUKRS,BUDAT ,BLDAT ,BELNR,AWREF,GJAHR)MN ON C.BUKRS=MN.RBUKRS  AND (CASE WHEN C.VBELN='''' THEN C.REBZG ELSE C.VBELN END )=MN.AWREF AND C.KUNNR=MN.KUNNR
	WHERE C.MANDT=900 
		AND C.BLART IN (''DZ'',''DP'',''DW'')
		AND B.HKONT NOT IN (''0000253230'',''0000130932'')
		AND C.BUDAT BETween '''+CONVERT(VARCHAR(10),@FROM_DATE,112)+'''  AND '''+CONVERT(VARCHAR(10),@TO_DATE,112)+'''
UNION ALL 
		SELECT
	
			 C.BUKRS COMPANYCODE
			,LEFT(B.PRCTR,4)PLANT
			,T.NAME1 PLANTNAME
			,C.BLART DOCUMENTTYPE
			,C.KUNNR CUSTOMERCODE
			,N.BPEXT NAVCODE
 			,K.NAME1 CUSTOMERNAME
			,IFNULL(K.STCD3,'''') CUSTOMERGSTNUMBER
			,K.REGIO CUSTOMERSTATECODE
			,T5.BEZEI  CUSTOMERSTATE_NAME
			,C.VBELN SDDOCNO
			,C.BELNR DOCUMENTNO
			,C.GJAHR FISCALYEAR
			,C.BUDAT POSTINGDATE
			,C.WRBTR PAYMENTAMOUNT
			,G.TXT50 BANKNAME
			,B.HKONT GLACC
			,TO_CHAR(C.BUDAT,''DD/MM/YYYY'') CHEQUEDATE
			,(CASE WHEN LENGTH(C.XBLNR)<=6 THEN C.XBLNR ELSE IFNULL(LEFT(I.BKTXT,6),LEFT(C.SGTXT,6)) END)  CHEQUENO
			,B.WRBTR CHEQUEAMOUNT
			,C.SGTXT REMARK
			,IFNULL(MN.BELNR,'''') INVOICENO
			,(CASE WHEN C.VBELN='''' THEN C.REBZG ELSE C.VBELN END ) BILLINGDOCNO
			,TO_CHAR(MN.BLDAT,''DD/MM/YYYY'') BILLDATE
			,IFNULL(I.BLDAT,MN.BLDAT) DOCUMENTDATE
			,TO_CHAR(MN.BUDAT,''DD/MM/YYYY'') INVOICEPOSTINGDATE
			,IFNULL(MN.AMOUNT,0.00) AMOUNT
	FROM SAPABAP1.BSAD C
		LEFT JOIN SAPABAP1.KNA1 K ON C.KUNNR=K.KUNNR
		INNER JOIN SAPABAP1.BUT000 N ON K.MANDT=N.CLIENT AND K.KUNNR=N.PARTNER
		LEFT JOIN (SELECT * FROM SAPABAP1.BSEG WHERE MANDT=900 AND  H_BLART IN (''DP'',''DZ'',''AB'',''DW'')  AND PRCTR<>'''' AND MWSKZ='''') B ON 
				C.BUKRS=B.BUKRS AND C.GJAHR=B.GJAHR AND C.BELNR=B.BELNR
		LEFT JOIN (SELECT KUNNR,RBUKRS,BELNR,BUDAT,BLDAT,AWREF,GJAHR,MAX(LEFT(PRCTR,4)) PRCTR,SUM(HSL) AMOUNT FROM SAPABAP1.ACDOCA WHERE BELNR NOT LIKE ''B%''  AND KOART=''D''
			GROUP BY KUNNR ,RBUKRS,BUDAT ,BLDAT ,BELNR,AWREF,GJAHR)MN ON C.BUKRS=MN.RBUKRS  AND (CASE WHEN C.VBELN='''' THEN C.REBZG ELSE C.VBELN END )=MN.AWREF AND C.KUNNR=MN.KUNNR
		LEFT JOIN (SELECT * FROM SAPABAP1.SKAT WHERE KTOPL=''LGIN'') G ON B.HKONT=G.SAKNR
		LEFT JOIN SAPABAP1.BKPF I ON C.BUKRS=I.BUKRS AND C.REBZJ=I.GJAHR  AND C.REBZG=I.BELNR
		LEFT JOIN  SAPABAP1.T005U T5 ON T5.BLAND=K.REGIO AND T5.SPRAS=''E'' AND T5.LAND1=''IN'' AND T5.MANDT=900
		LEFT JOIN SAPABAP1.T001W T ON T.WERKS=LEFT(B.PRCTR,4)
	WHERE 
			C.MANDT=900 
			AND C.BLART  IN (''DZ'',''DP'',''DW'')
			AND B.HKONT NOT IN (''0000253230'',''0000130932'')
			AND C.BUDAT BETween '''+CONVERT(VARCHAR(10),@FROM_DATE,112)+'''  AND '''+CONVERT(VARCHAR(10),@TO_DATE,112)+'''
			)K
	WHERE	K.COMPANYCODE LIKE '''+ @COMPANY +'''
			AND K.CUSTOMERCODE LIKE ''' + @CUSTOMER + '''
'
PRINT @SQL
EXEC(@SQL) AT SAP_GHP



