ALTER PROCEDURE [dbo].[Android_SAP_Request] (
    @Site_Code VARCHAR(10) = 'PA',
    @From_Date DATETIME,
    @To_Date DATETIME,
    @Dat_Type VARCHAR(100) = ''
) AS IF @Site_Code IN ('''', '', '%', 'All') BEGIN
SET
    @Site_Code = 'PA'
END IF @Dat_Type IN ('''', '', '%', 'All') BEGIN
SET
    @Dat_Type = 'CreateDate'
END DECLARE @Plant_Code VARCHAR(1000) = (
    SELECT
        Plant_Code
    FROM
        SAP_MM_Plant_Master
    WHERE
        Site_Code = @Site_Code
) DECLARE @WASTE_LIFT_PRIORITY AS TABLE (
    PLANT_CODE VARCHAR(10),
    CUSTOMER_CODE VARCHAR(10),
    WASTE_CODE VARCHAR(30),
    INDEX_SCORE NUMERIC(18, 2),
    LIFTING_PRIORITY INT
) DECLARE @SQL VARCHAR(MAX) EXEC('SET ''CDS_CLIENT''=''900''') AT SAP_GHP
SET
    @SQL = 'SELECT PLANT_CODE,CUSTOMER_CODE,WASTE_CODE,INDEX_SCORE,LIFTING_PRIORITY FROM SAPABAP1.ZCDS_CLNT_WASTE_REQ_LIFT (''Mandt=''''900''''AND(PLANT_CODE=''''' + @Plant_Code + ''''')'',2)' --INSERT INTO @WASTE_LIFT_PRIORITY EXEC(@SQL) AT SAP_GHP
    --PRINT(@SQL)
    DECLARE @WO_TABLE AS TABLE (
        ItemID INT IDENTITY(1, 1),
        Item_ID Int,
        SAP_PLANT_CODE VARCHAR(10),
        ZW_STATUS VARCHAR(2),
        WORK_ORDER_NO VARCHAR(20),
        ZWO_ERR_MSG VARCHAR(500),
        REGION_NAME VARCHAR(100)
    )
SET
    @SQL = '
SELECT ITEM_ID,SAP_PLANT_CODE,ZW_STATUS,WORK_ORDER_NO,ZWO_ERR_MSG,TVG.BEZEI FROM
(
SELECT ROW_NUMBER() OVER(PARTITION BY ITEM_ID ORDER BY ZRECNO DESC) R_NO
,ITEM_ID,SAP_PLANT_CODE,ZW_STATUS,WORK_ORDER_NO,ZWO_ERR_MSG FROM SAPABAP1.ZAUTOWO_LOG
WHERE SAP_PLANT_CODE = ''' + @PLANT_CODE + ''' 
)x 
LEFT JOIN SAPABAP1.VBAK as rb on rb.VBELN = x.WORK_ORDER_NO
LEFT JOIN SAPABAP1.KNVV as vv on vv.KUNNR = rb.KUNNR and vv.VKORG = rb.VKORG AND vv.VTWEG = rb.VTWEG and vv.SPART = rb.SPART
LEFT JOIN SAPABAP1.TVGRT tvg on tvg.VKGRP=vv.VKGRP AND tvg.SPRAS=''E'' AND tvg.MANDT=vv.MANDT
WHERE R_NO = 1
' PRINT(@SQL)
INSERT INTO
    @WO_TABLE EXEC(@SQL) AT SAP_GHP DECLARE @SAP_TABLE AS TABLE (
        Itemid INT,
        SHIPMENT_NO VARCHAR(20),
        MANIFEST_NO VARCHAR(20),
        VEHICLE_NO VARCHAR(40),
        TRANSPORTER_NO VARCHAR(100),
        TRANSPORTER_NAME VARCHAR(200),
        SITE_INWARD_DATE DATETIME,
        BILLING_BLOCK VARCHAR(10),
        BILLING_STATUS VARCHAR(10),
        INVOICE_NO VARCHAR(20),
        BILLINGQUANTITY NUMERIC(18, 3),
        PAYMENT_TYPE VARCHAR(20)
    )
SET
    @SQL = 'SELECT vb.BSTNK ITEMID,vt.TKNUM SHIPMENT_NO,vb.VBELN MANIFEST_NO,vk.EXTI1 VEH_NO,vk.TDLNR,but.NAME_ORG1 TRANSPORTER_NAME 
					,z.ZGTRDATE
					,FAKSK,FKSAK,bl.BILLINGDOCUMENT,bl.BILLINGQUANTITY
					,(CASE WHEN IFNULL(gg.VBELN,'''')='''' AND IFNULL(bl.BILLINGDOCUMENT,'''')<>''''  THEN ''Partial'' else ''Full'' End)Payment_Type
					FROM SAPABAP1.VBAK vb
			--	INNER JOIN SAPABAP1.VBKD vd on vd.VBELN = vb.VBELN AND LENGTH (vd.BSTKD)=9 AND vd.BSTKD LIKE ''0%''
				
				INNER JOIN SAPABAP1.TVAKT akt on akt.AUART=vb.AUART AND akt.SPRAS=''E'' AND akt.BEZEI LIKE ''%Mani%''
				LEFT JOIN SAPABAP1.LIPS lp on lp.VGBEL=vb.VBELN
				LEFT JOIN SAPABAP1.VTTP vt on vt.VBELN = lp.VBELN
				LEFT JOIN SAPABAP1.VTTK vk on vk.TKNUM = vt.TKNUM
				LEFT JOIN SAPABAP1.BUT000 but on but.PARTNER=vk.TDLNR 
				LEFT JOIN SAPABAP1.ZWASTEIN z on z.VBELN = vb.VBELN
				LEFT JOIN SAPABAP1.ZIMANIFESTBILL as bl on bl.MANIFEST_NO = vb.VBELN and bl.SALESORGANIZATION = vb.VKORG
				LEFT JOIN
					(
					SELECT VBELN FROM SAPABAP1.BSAD GROUP BY VBELN
					)gg on gg.VBELN = bl.BILLINGDOCUMENT
				--LEFT JOIN
				--	(
				--	SELECT bk.AWKEY,bk.GJAHR,bk.BUKRS FROM SAPABAP1.BKPF as bk
				--		INNER JOIN SAPABAP1.BSID as bd on bd.BUKRS = bk.BUKRS AND bd.REBZG = bk.AWKEY AND bd.REBZJ = bk.GJAHR
				--		WHERE bk.AWTYP = ''VBRK''
				--	GROUP BY bk.AWKEY,bk.GJAHR,bk.BUKRS
				--	)
			 WHERE vb.VKORG=''' + @PLANT_CODE + ''' 
			 AND LENGTH (vb.BSTNK)=9 AND vb.BSTNK LIKE ''0%''
				' --	Print @SQL
INSERT INTO
    @SAP_TABLE EXEC(@SQL) AT SAP_GHP DECLARE @SAP_MAST AS TABLE (
        CUSTOMER_CODE VARCHAR(100),
        REGION_NAME VARCHAR(100)
    )
SET
    @SQL = 'SELECT DISTINCT bt.PARTNER,TVG.BEZEI FROM SAPABAP1.BUT000 bt
				LEFT JOIN SAPABAP1.KNVV tvv on tvv.KUNNR=bt.PARTNER AND tvv.MANDT=bt.CLIENT AND tvv.VKORG = ''' + @PLANT_CODE + '''  
				LEFT JOIN SAPABAP1.TVGRT tvg on tvg.VKGRP=tvv.VKGRP AND tvg.SPRAS=''E'' AND tvg.MANDT=bt.CLIENT
			'
INSERT INTO
    @SAP_MAST EXEC(@SQL) AT SAP_GHP DECLARE @SAPCLNTWASTE AS TABLE (
        CUSTOMER_CODE VARCHAR(100),
        WASTE_CODE VARCHAR(100),
        WASTE_NAME VARCHAR(100),
        PROCSS_RATE NUMERIC(18, 2),
        PH NUMERIC(18, 3),
        CV NUMERIC(18, 2),
        CL NUMERIC(18, 2),
        SULPHUR NUMERIC(18, 2)
    )
SET
    @SQL = 'SELECT bt.PARTNER , zbp.ZWAST_TYPE,mr.MAKTX
,cd.PROCESS_RATE,cd.PH ,cd.CV , cd.CL , cd.SULPHUR
FROM SAPABAP1.BUT000 bt
INNER JOIN SAPABAP1.ZBP_INDUSTRY zbp on zbp.Change_Number=bt.PARTNER and zbp.VKORG = ''' + @PLANT_CODE + ''' ' + ' AND UPPER(zbp.zwast_Type) NOT LIKE ''FPA%''
INNER JOIN SAPABAP1.MAKT mr on mr.MATNR = UPPER(zbp.ZWAST_TYPE)
LEFT JOIN SAPABAP1.ZCDS_CUST_MAT as cd on cd.CUSTOMER = bt.PARTNER and zbp.ZWAST_TYPE = cd.MATERIAL_CODE AND zbp.VKORG = cd.VKORG
'
INSERT INTO
    @SAPCLNTWASTE EXEC(@SQL) AT SAP_GHP DECLARE @VENDOR_MST AS TABLE (
        VENDOR_CODE VARCHAR(20),
        VENDOR_NAME VARCHAR(200)
    )
SET
    @SQL = 'SELECT DISTINCT k.LIFNR,k.NAME1 FROM SAPABAP1.LFA1 k
				INNER JOIN SAPABAP1.ZVEHMASTER v on v.LIFNR=k.LIFNR
		'
INSERT INTO
    @VENDOR_MST EXEC(@SQL) AT SAP_GHP IF @Dat_Type = 'CreateDate' BEGIN
SELECT
    a.Item_Id Request_No,
    a.Emp_Id,
    a.Emp_Name,
    a.Customer_Code,
    a.Customer_Name,
    COALESCE(wt.REGION_NAME, a.Region) Region,
    a.Waste_Type,
    cw.Waste_Name,
    a.Waste_Physical_Status,
    a.Packaging [Cont.Type],
    Packaging,
    Texture,
    Trucktype,
    Remarks,
    CONVERT(VARCHAR(20), Request_dta, 106) Request_Date --,Request_dta Request_Date
,
    CONVERT(VARCHAR(5), Request_time, 108) Request_Time --,Request_time Request_Time
,
    Quentity Quantity,
    CONVERT(VARCHAR(20), Created_on, 106) Entry_Date --,Created_on Entry_Date
,
    CONVERT(VARCHAR(5), Created_on, 108) Entry_Time --,Created_on Entry_Time
,
    s.SHIPMENT_NO,
    s.MANIFEST_NO,
    s.TRANSPORTER_NAME,
    s.VEHICLE_NO,
    CONVERT(VARCHAR(20), s.SITE_INWARD_DATE, 106) SITE_INWARD_DATE --,s.SITE_INWARD_DATE
,
    v.VENDOR_NAME Gobolt_Transporter_Name,
    a.VehicleNo Gobolt_Vehicle_No,
    a.TMR Gobolt_TMR,
    cw.PROCSS_RATE,
    cw.PH,
    cw.CV,
    cw.CL,
    cw.SULPHUR --,ft.INDEX_SCORE , ft.LIFTING_PRIORITY
,
    0 INDEX_SCORE,
    0 LIFTING_PRIORITY,
    wt.ZW_STATUS,
    wt.WORK_ORDER_NO,
    wt.ZWO_ERR_MSG as Work_Order_Err_Message,
    s.BILLING_BLOCK,
    s.BILLING_STATUS,
    s.INVOICE_NO,
    s.BILLINGQUANTITY,
    s.PAYMENT_TYPE
FROM
    ANDROIDHR..Android_Waste_Request a
    LEFT JOIN @SAP_TABLE s on s.Itemid = a.Item_id --	LEFT JOIN @SAP_MAST sp on sp.CUSTOMER_CODE=a.Customer_Code
    LEFT JOIN @SAPCLNTWASTE cw on cw.CUSTOMER_CODE = a.Customer_Code
    and cw.WASTE_CODE = a.Waste_Type
    LEFT JOIN @VENDOR_MST v on v.VENDOR_CODE = a.VendorNo --LEFT JOIN @WASTE_LIFT_PRIORITY as ft on ft.PLANT_CODE = a.Sap_Plant_Code AND ft.CUSTOMER_CODE = a.Customer_Code AND ft.WASTE_CODE = a.Waste_Type
    LEFT JOIN @WO_TABLE as wt on wt.Item_ID = a.Item_Id
WHERE
    1 = 1
    AND a.Site_Code = @Site_Code
    AND CONVERT(DATE, Created_on) BETWEEN @From_Date
    AND @To_Date
END IF @Dat_Type = 'RequestDate' BEGIN
SELECT
    a.Item_Id Request_No,
    a.Emp_Id,
    a.Emp_Name,
    a.Customer_Code,
    a.Customer_Name,
    COALESCE(wt.REGION_NAME, a.Region) Region,
    a.Waste_Type,
    cw.Waste_Name,
    a.Waste_Physical_Status,
    a.Packaging [Cont.Type],
    Packaging,
    Texture,
    Trucktype,
    Remarks,
    CONVERT(VARCHAR(20), Request_dta, 106) Request_Date --,Request_dta Request_Date
,
    CONVERT(VARCHAR(5), Request_time, 108) Request_Time --,Request_time Request_Time
,
    Quentity Quantity,
    CONVERT(VARCHAR(20), Created_on, 106) Entry_Date --,Created_on Entry_Date
,
    CONVERT(VARCHAR(5), Created_on, 108) Entry_Time --,Created_on Entry_Time
,
    s.SHIPMENT_NO,
    s.MANIFEST_NO,
    s.TRANSPORTER_NAME,
    s.VEHICLE_NO,
    CONVERT(VARCHAR(20), s.SITE_INWARD_DATE, 106) SITE_INWARD_DATE --,s.SITE_INWARD_DATE
,
    v.VENDOR_NAME Gobolt_Transporter_Name,
    a.VehicleNo Gobolt_Vehicle_No,
    a.TMR Gobolt_TMR,
    cw.PROCSS_RATE,
    cw.PH,
    cw.CV,
    cw.CL,
    cw.SULPHUR --,ft.INDEX_SCORE , ft.LIFTING_PRIORITY
,
    0 INDEX_SCORE,
    0 LIFTING_PRIORITY,
    wt.ZW_STATUS,
    wt.WORK_ORDER_NO,
    wt.ZWO_ERR_MSG as Work_Order_Err_Message,
    s.BILLING_BLOCK,
    s.BILLING_STATUS,
    s.INVOICE_NO,
    s.BILLINGQUANTITY,
    s.PAYMENT_TYPE
FROM
    ANDROIDHR..Android_Waste_Request a
    LEFT JOIN @SAP_TABLE s on s.Itemid = a.Item_id --LEFT JOIN @SAP_MAST sp on sp.CUSTOMER_CODE=a.Customer_Code
    LEFT JOIN @SAPCLNTWASTE cw on cw.CUSTOMER_CODE = a.Customer_Code
    and cw.WASTE_CODE = a.Waste_Type
    LEFT JOIN @VENDOR_MST v on v.VENDOR_CODE = a.VendorNo
    LEFT JOIN @WASTE_LIFT_PRIORITY as ft on ft.PLANT_CODE = a.Sap_Plant_Code
    AND ft.CUSTOMER_CODE = a.Customer_Code
    AND ft.WASTE_CODE = a.Waste_Type
    LEFT JOIN @WO_TABLE as wt on wt.Item_ID = a.Item_Id
WHERE
    1 = 1
    AND a.Site_Code = @Site_Code
    AND CONVERT(DATE, Request_dta) BETWEEN @From_Date
    AND @To_Date
END