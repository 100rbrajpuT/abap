*&---------------------------------------------------------------------*
*& Report ZTEST_WAST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_wast.

TABLES : zcustomer_ref , zcust_ref_log, vttp , ZCDS_MANIFEST.
*DATA: lt_result1 TYPE TABLE OF ZCDS_WASTE_SUBQ2,
*      ls_result1 TYPE ZCDS_WASTE_SUBQ2.


TYPES: BEGIN OF ty_final,
         item_id               TYPE zcustomer_ref-item_id,    "sales orgnx
         emp_id                TYPE zcustomer_ref-emp_id,
         emp_name              TYPE zcustomer_ref-emp_name,
         customer_code         TYPE zcustomer_ref-customer_code,
         customer_name         TYPE zcustomer_ref-customer_name,
         region                TYPE zcustomer_ref-region,
         waste_type            TYPE zcustomer_ref-waste_type,
         waste_physical_status TYPE zcustomer_ref-waste_physical_status,
         packaging             TYPE zcustomer_ref-packaging,
         texture               TYPE zcustomer_ref-texture,
         trucktype             TYPE zcustomer_ref-trucktype,
         remarks               TYPE zcustomer_ref-remarks,
         request_dta           TYPE zcustomer_ref-request_dta,
         request_time          TYPE zcustomer_ref-request_time,
         quentity              TYPE zcustomer_ref-quentity,
         created_on            TYPE zcustomer_ref-created_on,
         CREATED_by            TYPE zcustomer_ref-CREATED_by,
         sap_plant_code        TYPE  zautowo_log-sap_plant_code,
         avg_ph                TYPE p LENGTH 16 DECIMALS 3,
         avg_cv                TYPE p LENGTH 16 DECIMALS 3,
         avg_cl                TYPE p LENGTH 16 DECIMALS 3,
         avg_voc               TYPE p LENGTH 16 DECIMALS 3,
         index_score           TYPE p LENGTH 16 DECIMALS 3,
         lifting_priority      TYPE p LENGTH 16 DECIMALS 3,
         zw_status             TYPE  zautowo_log-zw_status,
         work_order_no         TYPE  zautowo_log-work_order_no,
         zwo_err_msg           TYPE  zautowo_log-zwo_err_msg,
        manifest                 TYPE ZCDS_MANIFEST-manifest,
        shipment_no                TYPE ZCDS_MANIFEST-shipment_no,
        itemid                TYPE ZCDS_MANIFEST-itemid,
        vehicle_no                 TYPE ZCDS_MANIFEST-vehicle_no,
      transporter_no                TYPE ZCDS_MANIFEST-transporter_no,
      transporter_name                TYPE ZCDS_MANIFEST-transporter_name,
      billingdocument TYPE ZCDS_MANIFEST-billingdocument ,
      billingquantity TYPE ZCDS_MANIFEST-billingquantity ,
       payment_type TYPE zcds_manifest-payment_type ,
*         tknum                 TYPE vttp-tknum,
*         tpnum                 TYPE vBAK-bstnk,
*         exti1                 TYPE vttk-exti1,
*         tdlnr                 TYPE vttk-tdlnr,
*         name_org1             TYPE but000-name_org1,
       END OF ty_final.


DATA : l_where  TYPE string,
       l_where2 TYPE string.

DATA : l_VKORG  TYPE vkorg,
       lv_datum TYPE datum,
       lt_tab   TYPE STANDARD TABLE OF zcds_clnt_waste_req_lift,
       LS_tab   TYPE zcds_clnt_waste_req_lift,
       ls_tab2  TYPE zsdwlp_log,
       zpartype TYPE char10.

DATA: lt_result       TYPE TABLE OF ty_final,
      ls_result       TYPE ty_final, lv_request_date TYPE string.

SELECT-OPTIONS : s_VKORG FOR l_VKORG NO INTERVALS.
SELECT-OPTIONS : s_date FOR lv_datum.
PARAMETERS: p_option TYPE char10 AS LISTBOX VISIBLE LENGTH 20.

DATA : name  TYPE vrm_id,
       list  TYPE vrm_values,
       value LIKE LINE OF list.

DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      w_layout    TYPE slis_layout_alv.
w_layout-zebra = 'X'.
RANGES: r_field FOR vbak-kunnr.

DATA: ispfli TYPE TABLE OF spfli.
DATA: gr_table TYPE REF TO cl_salv_table.
DATA: gr_functions TYPE REF TO cl_salv_functions.
DATA: gr_display TYPE REF TO cl_salv_display_settings.
DATA: gr_columns TYPE REF TO cl_salv_columns_table.
DATA: gr_column TYPE REF TO cl_salv_column_table.
DATA: gr_layout TYPE REF TO cl_salv_layout.
DATA: key TYPE salv_s_layout_key.
DATA: color TYPE lvc_s_colo.

IF s_VKORG-low IS INITIAL.
  MESSAGE 'Please select sales organisation.' TYPE 'E' DISPLAY LIKE 'I'.
ENDIF.

AUTHORITY-CHECK OBJECT 'M_BEST_WRK' FOR USER sy-uname
            ID 'WERKS' FIELD s_VKORG-low
            ID 'ACTVT' FIELD '02'.
IF sy-subrc <> 0.
  MESSAGE e018(zmm) WITH s_VKORG-low.
ENDIF.

AT SELECTION-SCREEN OUTPUT.
  "Populating the dropdown for p_option
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  ls_value-key = 'CREATED'.
  ls_value-text = 'Created Date'.
  APPEND ls_value TO lt_values.

  ls_value-key = 'RELEASED'.
  ls_value-text = 'Release Date'.
  APPEND ls_value TO lt_values.

  ls_value-key = 'ALL'.
  ls_value-text = 'ALL Date'.
  APPEND ls_value TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_OPTION'
      values          = lt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    WRITE: 'Error populating dropdown'.
  ENDIF.


  name = 'ZPARTYPE'.

  value-key = '1'.
  value-text = 'FP BASE'.
  APPEND value TO list.

  value-key = '2'.
  value-text = 'QC BASE'.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.
  CLEAR list.

  zpartype = '2'. "Set default value for the variable ZPARTYPE

START-OF-SELECTION.
  CASE p_option.
    WHEN 'CREATED'.
      WRITE: 'Created Date selected'.
    WHEN 'RELEASED'.
      WRITE: 'Release Date selected'.
    WHEN 'ALL'.
      WRITE: 'ALL'.
  ENDCASE.

  l_where = cl_shdb_seltab=>combine_seltabs(
                                        EXPORTING it_named_seltabs =
                                              VALUE #( ( name = 'PLANT_CODE' dref = REF #( s_VKORG[] ) )

                                                       )
                                              iv_client_field = 'Mandt'
                                  ).


  CONDENSE l_where NO-GAPS.





  SELECT a~item_id,
         a~emp_id,
         a~emp_name,
         a~customer_code,
         a~CUSTOMER_Name,
         a~region ,
         a~waste_type,
         a~waste_physical_status,
         a~packaging AS cont_type,
         a~texture,
         a~trucktype ,
         a~remarks ,
         a~REQUEST_dta ,
         a~request_time,   "CONVERT(VARCHAR(20), a.Request_dta, 106) AS Request_Date,
         a~quentity ,
         a~created_on  AS Entry_Date ,
         a~created_by AS Entry_time ,
         s~sap_plant_code ,
         ft~avg_ph ,
         ft~avg_cv ,
         ft~avg_cl ,
         ft~avg_voc,
         ft~index_score,
         ft~lifting_priority ,
         s~zw_status AS work_order_status,
         s~work_order_no ,
         s~zwo_err_msg ,
         cds~manifest AS manifest_num ,
        cds~shipment_no AS shipment_no,
        cds~itemid AS itemid,
       cds~vehicle_no AS vehicle_no ,
       cds~transporter_no AS transporter_no ,
       cds~transporter_name AS transporter_name,
       cds~billingdocument ,
       cds~billingquantity ,
     cds~payment_type
*         vt~tknum AS shipment_num ,
*         vt~tpnum AS shipment_item ,
*   " vt~tpnum AS shipment_item,   " Shipment item from VTTP
*       vk~exti1 AS vehicle_num,     " Vehicle number from VTTK
*       vk~tdlnr AS transporter_num, " Transporter number from VTTK
*       but~name_org1 AS transporter_name " Transporter name from BUT000
  "INTO TABLE @lt_result
  FROM zcust_ref_log AS a
  LEFT OUTER JOIN zcustomer_ref AS b ON a~item_id = b~item_id
  "left outer JOIN zcust_ref_log AS b ON a~item_id = b~item_id
  "LEFT OUTER MANY TO ONE JOIN zautowo_log AS s  ON s~ITEM_ID =  a~item_id   "zrecno
*  LEFT OUTER JOIN zautowo_log AS s  ON s~item_id =  a~item_id OR  s~item_id =  b~item_id
*        AND s~sap_plant_code = a~sap_plant_code OR s~sap_plant_code = b~sap_plant_code
    LEFT OUTER JOIN zv_latest_log AS s ON s~item_id = a~item_id OR  s~item_id =  b~item_id
**   LEFT OUTER JOIN (
**    SELECT  item_id , MAX(created_on) AS max_created_on
**    FROM zautowo_log
**    GROUP BY item_id
**  ) AS last_log ON last_log~item_id = a~item_id
      LEFT OUTER JOIN ZCDS_MANIFEST AS cds ON cds~itemid = s~item_id
*  LEFT JOIN vbak AS rb ON rb~vbeln = s~work_order_no
*  LEFT JOIN lips AS lp ON lp~vgbel = rb~vbeln
*  LEFT JOIN vttp AS vt ON vt~vbeln = lp~vbeln
*  LEFT JOIN vttk AS vk ON vk~tknum = vt~tknum
*  LEFT JOIN but000 AS but ON but~partner = vk~tdlnr
  LEFT OUTER JOIN zfn_clntwaste_priority( sel_opt = @l_where , paratype = @zpartype ) AS ft
    ON ft~plant_code = s~sap_plant_code AND
       ft~customer_code = a~customer_code  AND
       ft~waste_code = a~waste_type
**  LEFT OUTER JOIN vbak AS rb ON rb~vbeln = s~work_order_no   "LIPS lp on lp.VGBEL=vb.VBELN
**  LEFT OUTER JOIN lips AS lp    ON lp~vgbel = rb~vbeln
**  LEFT OUTER JOIN vttp AS vt ON vt~vbeln =  lp~vbeln
**  LEFT OUTER JOIN vttk AS vk ON vk~tknum =  vt~tknum
** " LEFT OUTER JOIN likp AS li ON li~traid = rb~vbeln

*
  WHERE    a~sap_plant_code IN @s_vkorg " or b~SAP_PLANT_CODE IN @s_vkorg "or  "   s~sap_plant_code IN @s_vkorg " a~item_id = '783574'  "'707755' "'784287'   " '784287' " '10309922' " '10309922' "a~item_id = '783574' "
    AND s~work_order_no IS NOT NULL
    AND s~item_id = a~item_id
*    AND s~zw_status  = 'T'
    AND ( a~created_on IN @s_date   )"OR b~created_on IN @s_date )
 GROUP BY a~item_id,
         a~emp_id,
         a~emp_name,
         a~customer_code,
         a~customer_name,
         a~region ,
         a~waste_type,
         a~waste_physical_status,
         a~packaging,
         a~texture,
         a~trucktype,
         a~remarks,
         a~request_dta,
         a~request_time,
         a~quentity,
         a~created_on,
         a~created_by,
         s~sap_plant_code,
         s~zw_status,
        s~work_order_no,
         s~zwo_err_msg ,
          ft~avg_ph ,
         ft~avg_cv ,
         ft~avg_cl ,
         ft~avg_voc,
         ft~index_score,
         ft~lifting_priority ,
         cds~manifest ,
         cds~shipment_no ,
         cds~itemid ,
         cds~vehicle_no ,
         cds~transporter_no ,
         cds~transporter_name,
         cds~billingdocument,
         cds~billingquantity ,
cds~payment_type
*  vt~tknum ,
*  vk~exti1,
*   vk~tdlnr,
*   but~name_org1,
*   vt~tpnum
    INTO TABLE @lt_result.




*  LOOP AT lt_result INTO ls_result .
*    WRITE: / ls_result-item_id, ls_result-emp_id , ls_result-emp_name , ls_result-customer_code ,
*     ls_result-CUSTOMER_Name , ls_result-waste_type , ls_result-REQUEST_dta , ls_result-REQUEST_time , ls_result-sap_plant_code.
*
*  ENDLOOP.




  cl_salv_table=>factory( IMPORTING r_salv_table = gr_table
 CHANGING t_table = lt_result[] ).
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).
  gr_display = gr_table->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_layout = gr_table->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  gr_display->set_list_header( 'WASTE LIFTING PRIORITY REPORT' ).

  gr_columns = gr_table->get_columns( ).

  TRY.
      gr_column ?= gr_columns->get_column( 'ITEM_ID' ).
      gr_column->set_long_text( 'Item ID' ).
      gr_column->set_medium_text( 'Item ID' ).
      gr_column->set_short_text( 'ID' ).

      gr_column ?= gr_columns->get_column( 'EMP_ID' ).
      gr_column->set_long_text( 'EMPLYEE_ID ' ).
      gr_column->set_medium_text( 'EMP_ID' ).
      gr_column->set_short_text( 'EMP_ID' ).
      gr_column->set_output_length( 30 ).

      gr_column ?= gr_columns->get_column( 'EMP_NAME' ).
      gr_column->set_long_text( 'EMPLOYEE Name' ).
      gr_column->set_medium_text( 'EMPLOYEE' ).
      gr_column->set_short_text( 'EMY' ).
      gr_column->set_output_length( 30 ).


      gr_column ?= gr_columns->get_column( 'CUSTOMER_CODE' ).
      gr_column->set_long_text( 'Customer Code' ).
      gr_column->set_medium_text( 'Cust Code' ).
      gr_column->set_short_text( 'CCode' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'CUSTOMER_NAME' ).
      gr_column->set_long_text( 'Customer Name' ).
      gr_column->set_medium_text( 'Cust Name' ).
      gr_column->set_short_text( 'CName' ).
      gr_column->set_output_length( 30 ).

      gr_column ?= gr_columns->get_column( 'REGION' ).
      gr_column->set_long_text( 'Region' ).
      gr_column->set_medium_text( 'Region' ).
      gr_column->set_short_text( 'Reg' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'WASTE_TYPE' ).
      gr_column->set_long_text( 'Waste Type' ).
      gr_column->set_medium_text( 'Waste' ).
      gr_column->set_short_text( 'WType' ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'WASTE_PHYSICAL_STATUS' ).
      gr_column->set_long_text( 'Waste Physical Status' ).
      gr_column->set_medium_text( 'Physical Status' ).
      gr_column->set_short_text( 'Status' ).
      gr_column->set_output_length( 25 ).

      gr_column ?= gr_columns->get_column( 'PACKAGING' ).
      gr_column->set_long_text( 'Packaging' ).
      gr_column->set_medium_text( 'Packaging' ).
      gr_column->set_short_text( 'Pack' ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'TEXTURE' ).
      gr_column->set_long_text( 'Texture' ).
      gr_column->set_medium_text( 'Texture' ).
      gr_column->set_short_text( 'Text' ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'TRUCKTYPE' ).
      gr_column->set_long_text( 'Truck Type' ).
      gr_column->set_medium_text( 'Truck' ).
      gr_column->set_short_text( 'Truck' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'REMARKS' ).
      gr_column->set_long_text( 'Remarks' ).
      gr_column->set_medium_text( 'Remarks' ).
      gr_column->set_short_text( 'Rmk' ).
      gr_column->set_output_length( 40 ).

      gr_column ?= gr_columns->get_column( 'REQUEST_DTA' ).
      gr_column->set_long_text( 'Request Date' ).
      gr_column->set_medium_text( 'Req Date' ).
      gr_column->set_short_text( 'Date' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'REQUEST_TIME' ).
      gr_column->set_long_text( 'Request Time' ).
      gr_column->set_medium_text( 'Req Time' ).
      gr_column->set_short_text( 'Time' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'QUENTITY' ).
      gr_column->set_long_text( 'Quantity' ).
      gr_column->set_medium_text( 'Qty' ).
      gr_column->set_short_text( 'Qty' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'CREATED_ON' ).
      gr_column->set_long_text( 'Created On' ).
      gr_column->set_medium_text( 'Created' ).
      gr_column->set_short_text( 'Date' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'CREATED_BY' ).
      gr_column->set_long_text( 'Created By' ).
      gr_column->set_medium_text( 'Created By' ).
      gr_column->set_short_text( 'By' ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'SAP_PLANT_CODE' ).
      gr_column->set_long_text( 'Plant Code' ).
      gr_column->set_medium_text( 'Plant' ).
      gr_column->set_short_text( 'Code' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'AVG_PH' ).
      gr_column->set_long_text( 'Avg pH' ).
      gr_column->set_medium_text( 'Avg pH' ).
      gr_column->set_short_text( 'pH' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'AVG_CV' ).
      gr_column->set_long_text( 'Avg CV' ).
      gr_column->set_medium_text( 'Avg CV' ).
      gr_column->set_short_text( 'CV' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'AVG_CL' ).
      gr_column->set_long_text( 'Avg CL' ).
      gr_column->set_medium_text( 'Avg CL' ).
      gr_column->set_short_text( 'CL' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'AVG_VOC' ).
      gr_column->set_long_text( 'Avg VOC' ).
      gr_column->set_medium_text( 'Avg VOC' ).
      gr_column->set_short_text( 'VOC' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'INDEX_SCORE' ).
      gr_column->set_long_text( 'Index Score' ).
      gr_column->set_medium_text( 'Score' ).
      gr_column->set_short_text( 'Score' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'LIFTING_PRIORITY' ).
      gr_column->set_long_text( 'Lifting Priority' ).
      gr_column->set_medium_text( 'Priority' ).
      gr_column->set_short_text( 'Prio' ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'ZW_STATUS' ).
      gr_column->set_long_text( 'Work Order Status' ).
      gr_column->set_medium_text( 'Order Status' ).
      gr_column->set_short_text( 'Status' ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'WORK_ORDER_NO' ).
      gr_column->set_long_text( 'Work Order No' ).
      gr_column->set_medium_text( 'Order No' ).
      gr_column->set_short_text( 'WO' ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'ZWO_ERR_MSG' ).
      gr_column->set_long_text( 'Error Message' ).
      gr_column->set_medium_text( 'Error Msg' ).
      gr_column->set_short_text( 'Msg' ).
      gr_column->set_output_length( 40 ).

      gr_column ?= gr_columns->get_column( 'MANIFEST' ).
      gr_column->set_long_text( 'MANIFEST_NO' ).
      gr_column->set_medium_text( 'MANIFEST' ).
      gr_column->set_short_text( 'Manifest' ).
      gr_column->set_output_length( 15 ).

* Shipment Number
      gr_column ?= gr_columns->get_column( 'SHIPMENT_NO' ).
      gr_column->set_long_text( 'Shipment_Number' ).
      gr_column->set_medium_text( 'Shipment' ).
      gr_column->set_short_text( 'Ship' ).
      gr_column->set_output_length( 10 ).

* Shipment ID
      gr_column ?= gr_columns->get_column( 'ITEMID' ).
      gr_column->set_long_text( 'Shipment_ID' ).
      gr_column->set_medium_text( 'Shipment' ).
      gr_column->set_short_text( 'ShipID' ).
      gr_column->set_output_length( 10 ).

* Vehicle Number
      gr_column ?= gr_columns->get_column( 'VEHICLE_NO' ).
      gr_column->set_long_text( 'Vehicle_Number' ).
      gr_column->set_medium_text( 'Vehicle' ).
      gr_column->set_short_text( 'Veh No' ).
      gr_column->set_output_length( 10 ).

* Transporter Number
      gr_column ?= gr_columns->get_column( 'TRANSPORTER_NO' ).
      gr_column->set_long_text( 'Transporter Number' ).
      gr_column->set_medium_text( 'Transporter' ).
      gr_column->set_short_text( 'Trans No' ).
      gr_column->set_output_length( 20 ).
*
* cds~itemid AS shipment_item ,
*       cds~vehicle_no AS vehicle_num ,
*       cds~transporter_no AS transporter_num ,
*       cds~transporter_name AS transporter_name
* Transporter Name
      gr_column ?= gr_columns->get_column( 'TRANSPORTER_NAME' ).
      gr_column->set_long_text( 'Transporter Name' ).
      gr_column->set_medium_text( 'Transporter' ).
      gr_column->set_short_text( 'Trans Name' ).
      gr_column->set_output_length( 30 ).

          gr_column ?= gr_columns->get_column( 'BILLINGDOCUMENT' ).
      gr_column->set_long_text( 'INVOICE_NUMBER' ).
      gr_column->set_medium_text( 'INVOICE_NUM' ).
      gr_column->set_short_text( 'INVOICE' ).
      gr_column->set_output_length( 30 ).

          gr_column ?= gr_columns->get_column( 'BILLINGQUANTITY' ).
      gr_column->set_long_text( 'BillingQuantity' ).
      gr_column->set_medium_text( 'BillingQuantity' ).
      gr_column->set_short_text( 'Bill_Qty' ).
      gr_column->set_output_length( 30 ).

     gr_column ?= gr_columns->get_column( 'PAYMENT_TYPE' ).
      gr_column->set_long_text( 'Payment_Type' ).
      gr_column->set_medium_text( 'payment_type' ).
      gr_column->set_short_text( 'bill_qty' ).
      gr_column->set_output_length( 30 ).


    CATCH cx_salv_not_found.
*      MESSAGE  'Some fields yet to complete!' TYPE 'E'.
      WRITE : 'add missing fields'.
  ENDTRY.
  gr_table->display( ).



****************cds VIEW USED****************************
@AbapCatalog.sqlViewName: 'ZCDS_MANIFEST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Waste Pr sub Query2 shipment transport'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_Waste_subQ2 as select from vbak as vb
inner join tvakt as akt on vb.auart = akt.auart and akt.spras = 'E' and akt.bezei like '%Mani%'
  left outer join lips as lp on lp.vgbel = vb.vbeln
  left outer join vttp as vt on vt.vbeln = lp.vbeln
   left  outer join vttk as vk on vk.tknum = vt.tknum
   left outer join but000 as but on but.partner = vk.tdlnr
   left outer join zwastein as z on z.vbeln = vb.vbeln
   left outer join zimanifestbill as bl  on bl.manifest_no = vb.vbeln 
        and bl.salesorganization = vb.vkorg
    left outer join ZCDS_BSAD_Group as gg
    on gg.vbeln = bl.billingdocument
        
        
     
{

     key vb.vbeln as manifest ,
      vb.vkorg as plant_code ,
      vb.bstnk as itemid ,
      vt.tknum as shipment_no , 
      vk.exti1 as vehicle_no ,
      vk.tdlnr as transporter_no ,
      but.name_org1 as transporter_name ,
      z.zgtrdate as site_inward_date ,
      --bl.billlingdocument as invoice_no,
      bl.billingdocument as BILLINGDOCUMENT,
      bl.billingquantity as billingquantity  ,
      case
         when gg.vbeln is null and bl.billingdocument is not null then 'Partial'
         else 'Full'
     end as payment_type
}
***************************
@AbapCatalog.sqlViewName: 'ZCDS_BSAD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZCDS_BSAD_Group_sub2'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_BSAD_Group as select from bsad
{
  vbeln
}
group by vbeln;
**************************************
@AbapCatalog.sqlViewName: 'ZV_MAX_ZRECNO_SS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Max row no for zautolog table'
@Metadata.ignorePropagatedAnnotations: true
define view ZV_MAX_ZRECNO as select from zautowo_log
{
    item_id,
    max( zrecno ) as latest_zrecno
}
group by item_id
***********************************
@AbapCatalog.sqlViewName: 'ZV_LATEST_LOG_SS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'zautowo_log table'
@Metadata.ignorePropagatedAnnotations: true
   define view ZV_LATEST_LOG as select from zautowo_log as log
    inner join ZV_MAX_ZRECNO as maxlog
    on log.item_id = maxlog.item_id
    and log.zrecno = maxlog.latest_zrecno
{
    key log.zrecno as latest_zrecno,         // latest zrecno for the item_id
    log.item_id,                             // item_id
    log.sap_plant_code,                      // plant code
    log.zw_status,                           // status
    log.work_order_no,                       // work order number
    log.zwo_err_msg,                         // error message
    log.zcreate_date,                        // creation date
    log.zcreate_time                         // creation time
}
