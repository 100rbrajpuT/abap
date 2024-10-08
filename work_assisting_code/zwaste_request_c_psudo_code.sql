ALTER PROCEDURE [dbo].[Android_SAP_Request] (
    @Site_Code VARCHAR(10) = 'PA',
    @From_Date DATETIME,
    @To_Date DATETIME,
    @Dat_Type VARCHAR(100) = ''
) AS 
BEGIN
    -- Default Site_Code
    IF @Site_Code IN ('''', '', '%', 'All') 
    BEGIN
        SET @Site_Code = 'PA';
    END 

    -- Default Dat_Type
    IF @Dat_Type IN ('''', '', '%', 'All') 
    BEGIN
        SET @Dat_Type = 'CreateDate';
    END 

    DECLARE @Plant_Code VARCHAR(1000) = (
        SELECT Plant_Code
        FROM SAP_MM_Plant_Master
        WHERE Site_Code = @Site_Code
    ); 

    DECLARE @WO_TABLE AS TABLE (
        ItemID INT IDENTITY(1, 1),
        Item_ID Int,
        SAP_PLANT_CODE VARCHAR(10),
        ZW_STATUS VARCHAR(2),
        WORK_ORDER_NO VARCHAR(20),
        ZWO_ERR_MSG VARCHAR(500),
        REGION_NAME VARCHAR(100)
    );

    -- Logic to retrieve Work Orders
    DECLARE @SQL VARCHAR(MAX) = '
    SELECT ITEM_ID, SAP_PLANT_CODE, ZW_STATUS, WORK_ORDER_NO, ZWO_ERR_MSG 
    FROM (
        SELECT ROW_NUMBER() OVER(PARTITION BY ITEM_ID ORDER BY ZRECNO DESC) AS R_NO,
               ITEM_ID, SAP_PLANT_CODE, ZW_STATUS, WORK_ORDER_NO, ZWO_ERR_MSG 
        FROM SAPABAP1.ZAUTOWO_LOG
        WHERE SAP_PLANT_CODE = ''' + @Plant_Code + '''
    ) x 
    WHERE R_NO = 1';
    
    INSERT INTO @WO_TABLE EXEC(@SQL) AT SAP_GHP;

    -- Additional Tables Setup...
    -- (Add your other declarations and insert logic here as needed)

    -- Handle CreateDate case
    IF @Dat_Type = 'CreateDate' 
    BEGIN
        SELECT
            a.Item_Id AS Request_No,
            a.Emp_Id,
            a.Emp_Name,
            a.Customer_Code,
            a.Customer_Name,
            COALESCE(wt.REGION_NAME, a.Region) AS Region,
            a.Waste_Type,
            cw.Waste_Name,
            a.Waste_Physical_Status,
            a.Packaging AS [Cont.Type],
            a.Texture,
            a.Trucktype,
            a.Remarks,
            CONVERT(VARCHAR(20), a.Request_dta, 106) AS Request_Date,
            CONVERT(VARCHAR(5), a.Request_time, 108) AS Request_Time,
            a.Quantity AS Quantity,
            CONVERT(VARCHAR(20), a.Created_on, 106) AS Entry_Date,
            CONVERT(VARCHAR(5), a.Created_on, 108) AS Entry_Time,
            s.SHIPMENT_NO,
            s.MANIFEST_NO,
            s.TRANSPORTER_NAME,
            s.VEHICLE_NO,
            CONVERT(VARCHAR(20), s.SITE_INWARD_DATE, 106) AS SITE_INWARD_DATE,
            v.VENDOR_NAME AS Gobolt_Transporter_Name,
            a.VehicleNo AS Gobolt_Vehicle_No,
            a.TMR AS Gobolt_TMR,
            cw.PROCSS_RATE,
            cw.PH,
            cw.CV,
            cw.CL,
            cw.SULPHUR,
            0 AS INDEX_SCORE,
            0 AS LIFTING_PRIORITY,
            wt.ZW_STATUS,
            wt.WORK_ORDER_NO,
            wt.ZWO_ERR_MSG AS Work_Order_Err_Message,
            s.BILLING_BLOCK,
            s.BILLING_STATUS,
            s.INVOICE_NO,
            s.BILLINGQUANTITY,
            s.PAYMENT_TYPE
        FROM ANDROIDHR..Android_Waste_Request a
        LEFT JOIN @SAP_TABLE s ON s.Itemid = a.Item_id
        LEFT JOIN @SAPCLNTWASTE cw ON cw.CUSTOMER_CODE = a.Customer_Code AND cw.WASTE_CODE = a.Waste_Type
        LEFT JOIN @VENDOR_MST v ON v.VENDOR_CODE = a.VendorNo
        LEFT JOIN @WO_TABLE wt ON wt.Item_ID = a.Item_Id
        WHERE a.Site_Code = @Site_Code
          AND CONVERT(DATE, a.Created_on) BETWEEN @From_Date AND @To_Date;
    END 

    -- Handle RequestDate case
    IF @Dat_Type = 'RequestDate' 
    BEGIN
        SELECT
            a.Item_Id AS Request_No,
            a.Emp_Id,
            a.Emp_Name,
            a.Customer_Code,
            a.Customer_Name,
            COALESCE(wt.REGION_NAME, a.Region) AS Region,
            a.Waste_Type,
            cw.Waste_Name,
            a.Waste_Physical_Status,
            a.Packaging AS [Cont.Type],
            a.Texture,
            a.Trucktype,
            a.Remarks,
            CONVERT(VARCHAR(20), a.Request_dta, 106) AS Request_Date,
            CONVERT(VARCHAR(5), a.Request_time, 108) AS Request_Time,
            a.Quantity AS Quantity,
            CONVERT(VARCHAR(20), a.Created_on, 106) AS Entry_Date,
            CONVERT(VARCHAR(5), a.Created_on, 108) AS Entry_Time,
            s.SHIPMENT_NO,
            s.MANIFEST_NO,
            s.TRANSPORTER_NAME,
            s.VEHICLE_NO,
            CONVERT(VARCHAR(20), s.SITE_INWARD_DATE, 106) AS SITE_INWARD_DATE,
            v.VENDOR_NAME AS Gobolt_Transporter_Name,
            a.VehicleNo AS Gobolt_Vehicle_No,
            a.TMR AS Gobolt_TMR,
            cw.PROCSS_RATE,
            cw.PH,
            cw.CV,
            cw.CL,
            cw.SULPHUR,
            0 AS INDEX_SCORE,
            0 AS LIFTING_PRIORITY,
            wt.ZW_STATUS,
            wt.WORK_ORDER_NO,
            wt.ZWO_ERR_MSG AS Work_Order_Err_Message,
            s.BILLING_BLOCK,
            s.BILLING_STATUS,
            s.INVOICE_NO,
            s.BILLINGQUANTITY,
            s.PAYMENT_TYPE
        FROM ANDROIDHR..Android_Waste_Request a
        LEFT JOIN @SAP_TABLE s ON s.Itemid = a.Item_id
        LEFT JOIN @SAPCLNTWASTE cw ON cw.CUSTOMER_CODE = a.Customer_Code AND cw.WASTE_CODE = a.Waste_Type
        LEFT JOIN @VENDOR_MST v ON v.VENDOR_CODE = a.VendorNo
        LEFT JOIN @WO_TABLE wt ON wt.Item_ID = a.Item_Id
        WHERE a.Site_Code = @Site_Code
          AND CONVERT(DATE, a.Request_dta) BETWEEN @From_Date AND @To_Date;
    END 
END
