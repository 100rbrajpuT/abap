DATA: lt_requests TYPE TABLE OF your_structure_type,
      lt_sap_table TYPE TABLE OF sap_table_structure,
      lt_sapclntwaste TYPE TABLE OF sapclntwaste_structure,
      lt_vendor_mst TYPE TABLE OF vendor_structure,
      lt_wo_table TYPE TABLE OF wo_table_structure.

" Join ZCUSTOMER_REF and ZCUSTOMER_REF_LOG to get Android_Waste_Request
SELECT a.Item_Id AS Request_No,
       a.Emp_Id,
       a.Emp_Name,
       b.Customer_Code,
       b.Customer_Name,
       COALESCE(wt.REGION_NAME, a.Region) AS Region, ******************
       a.Waste_Type,
       cw.Waste_Name, *********
       a.Waste_Physical_Status,
       a.Packaging AS Cont_Type,
       a.Texture,
       a.Trucktype,
       a.Remarks,
       CONVERT(VARCHAR(20), a.Request_dta, 106) AS Request_Date,
       CONVERT(VARCHAR(5), a.Request_time, 108) AS Request_Time,
       a.Quantity,
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
       cw.PROCSS_RATE,   *******
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
  INTO TABLE lt_requests
  FROM ZCUSTOMER_REF AS a
  INNER JOIN ZCUSTOMER_REF_LOG AS b ON a.Item_Id = b.Item_Id
  LEFT JOIN @SAP_TABLE AS s ON s.Itemid = a.Item_Id
  LEFT JOIN @SAPCLNTWASTE AS cw ON cw.CUSTOMER_CODE = a.Customer_Code AND cw.WASTE_CODE = a.Waste_Type
  LEFT JOIN @VENDOR_MST AS v ON v.VENDOR_CODE = a.VendorNo
  LEFT JOIN @WO_TABLE AS wt ON wt.Item_ID = a.Item_Id
  WHERE a.Site_Code = @Site_Code
    AND CONVERT(DATE, a.Created_on) BETWEEN @From_Date AND @To_Date.

" Process lt_requests as needed
