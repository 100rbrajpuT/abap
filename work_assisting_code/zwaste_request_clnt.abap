*&---------------------------------------------------------------------*
*& Report ZWASTE_REQUEST_CLNT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT zwaste_request_clnt.

TABLES : tvgrt .
DATA : l_where  TYPE string,
       l_where2 TYPE string.

DATA : l_VKORG  TYPE vkorg,
       lv_datum TYPE datum,
       lt_tab   TYPE STANDARD TABLE OF zcds_clnt_waste_req_lift,
       LS_tab   TYPE zcds_clnt_waste_req_lift,
       ls_tab2  TYPE zsdwlp_log,
       zpartype TYPE char10. "Declare ZPARTYPE as a variable

TYPES : BEGIN OF lty_ZAUTOWO_LOG,
          zrecno         TYPE zitem_id,
          item_id        TYPE  zitem_id,
          sap_plant_code TYPE werks_d,
          zw_status      TYPE zw_status,
          work_order_no  TYPE vbeln,
          zwo_err_msg    TYPE zwo_err_msg,
          zcreate_date   TYPE zcreate_date,
          customer_code  TYPE vbeln,
          vkorg          TYPE vkorg,
          vkgrp          TYPE vkgrp,
          region_name    TYPE region_name,
        END OF lty_ZAUTOWO_LOG.


DATA : lt_zautowo_log TYPE TABLE OF lty_ZAUTOWO_LOG,
       ls_zautowo_log TYPE lty_ZAUTOWO_LOG.


TYPES: BEGIN OF ty_waste_report,
         zrecno         TYPE zitem_id,    " From ZAUTOWO_LOG table
         item_id        TYPE zitem_id,    " From ZAUTOWO_LOG table
         plant_code     TYPE werks_d,
         plant_name     TYPE t001w-name1,
         customer_code  TYPE kunnr,
         region_name    TYPE tvgrt-bezei,
         customer_name  TYPE kna1-name1,

         waste_code     TYPE matnr,
         waste_name     TYPE makt-maktx,
         avg_ph         TYPE p LENGTH 16 DECIMALS 3,
         avg_cv         TYPE p LENGTH 16 DECIMALS 3,
         avg_cl         TYPE p LENGTH 16 DECIMALS 3,
         avg_voc        TYPE p LENGTH 16 DECIMALS 3,

         qc_cl_range    TYPE char12,
*         material_easiness TYPE char15,
         physical_state TYPE char15,


*         sap_plant_code TYPE werks_d,     " From ZAUTOWO_LOG table
         zw_status      TYPE zw_status,   " From ZAUTOWO_LOG table
         work_order_no  TYPE vbeln,       " From ZAUTOWO_LOG table
         zwo_err_msg    TYPE zwo_err_msg, " From ZAUTOWO_LOG table
       END OF ty_waste_report.

DATA: lt_final_report TYPE TABLE OF ty_waste_report,
      ls_final_report TYPE ty_waste_report.



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

  IF sy-uname <> 'WF-BATCH'.

    "select * from ZCDS_CLNT_WASTE_REQ_LIFT( sel_opt = @l_where , paratype = @ZPARTYPE ) into table @lt_tab.
    SELECT plant_code , plant_name , customer_code , customer_name , waste_code  ,waste_name , avg_ph , avg_cv , avg_cl , avg_voc FROM zfn_clntwaste_priority( sel_opt = @l_where , paratype = @zpartype ) WHERE plant_code IN @s_VKORG
      INTO CORRESPONDING FIELDS OF TABLE @lt_tab .



    SELECT zrecno, item_id, sap_plant_code, zw_status, work_order_no, zwo_err_msg, zcreate_date
      INTO CORRESPONDING FIELDS OF TABLE @lt_zautowo_log
      FROM zautowo_log
      WHERE sap_plant_code IN @s_VKORG
        AND zcreate_date IN @s_date
      ORDER BY item_id, zrecno DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_zautowo_log COMPARING item_id.

    LOOP AT lt_zautowo_log INTO ls_zautowo_log.
      SELECT SINGLE rb~vbeln, rb~kunnr, vv~vkorg, vv~vkgrp, tvg~bezei
        INTO ( @ls_zautowo_log-work_order_no, @ls_zautowo_log-customer_code, @ls_zautowo_log-vkorg, @ls_zautowo_log-vkgrp, @ls_zautowo_log-region_name )
        FROM vbak AS rb
        LEFT JOIN knvv AS vv ON vv~kunnr = rb~kunnr
                            AND vv~vkorg = rb~vkorg
                            AND vv~vtweg = rb~vtweg
                            AND vv~spart = rb~spart
        LEFT JOIN tvgrt AS tvg ON tvg~vkgrp = vv~vkgrp
                               AND tvg~spras = 'E'

        WHERE rb~vbeln = @ls_zautowo_log-work_order_no.

      MODIFY lt_zautowo_log FROM ls_zautowo_log.
    ENDLOOP.

  ENDIF.





  CLEAR lt_final_report.

  LOOP AT lt_tab INTO ls_tab.
    CLEAR ls_final_report.

    " Fill data from the CDS view (ZFN_CLNTWASTE_PRIORITY)
    ls_final_report-plant_code        = ls_tab-plant_code.
    ls_final_report-plant_name        = ls_tab-plant_name.
    ls_final_report-customer_code     = ls_tab-customer_code.
    ls_final_report-customer_name     = ls_tab-customer_name.
    ls_final_report-waste_code        = ls_tab-waste_code.
    ls_final_report-waste_name        = ls_tab-waste_name.
    ls_final_report-avg_ph            = ls_tab-avg_ph.
    ls_final_report-avg_cv            = ls_tab-avg_cv.
    ls_final_report-avg_cl            = ls_tab-avg_cl.
    ls_final_report-avg_voc           = ls_tab-avg_voc.
*  ls_final_report-qc_cl_range       = ls_tab-qc_cl_range.
*  ls_final_report-material_easiness = ls_tab-material_easiness.
*  ls_final_report-physical_state    = ls_tab-physical_state.

    " Now, check if there is corresponding data from ZAUTOWO_LOG
    READ TABLE lt_zautowo_log INTO ls_zautowo_log WITH KEY  customer_code = ls_tab-customer_code . "sap_plant_code = ls_tab-plant_code AND
*    READ TABLE lt_zautowo_log INTO ls_zautowo_log WITH KEY sap_plant_code = ls_tab-plant_code and customer_code = 4100002429 .

    IF sy-subrc = 0.
      ls_final_report-zrecno         = ls_zautowo_log-zrecno.
      ls_final_report-item_id        = ls_zautowo_log-item_id.
      "   ls_final_report-sap_plant_code = ls_zautowo_log-sap_plant_code.
      ls_final_report-region_name    = ls_zautowo_log-region_name.
      ls_final_report-zw_status      = ls_zautowo_log-zw_status.
      ls_final_report-work_order_no  = ls_zautowo_log-work_order_no.
      ls_final_report-zwo_err_msg    = ls_zautowo_log-zwo_err_msg.


    ENDIF.

    " Append the filled structure to the final table
    APPEND ls_final_report TO lt_final_report.

  ENDLOOP.

*  " Fetch data from ZAUTOWO_LOG
*  SELECT zrecno ,
*         item_id ,
*         sap_plant_code ,
*         zw_status ,
*         work_order_no ,
*         zwo_err_msg
*    FROM zautowo_log
*    INTO CORRESPONDING FIELDS OF TABLE @lt_zautowo_log
*    WHERE sap_plant_code IN @s_VKORG .
  " AND zw_status = 'X'  .   " Adjust the status as per your requirement
  "  AND some_date BETWEEN @s_date-low AND @s_date-high. " Adjust date filter

  cl_salv_table=>factory( IMPORTING r_salv_table = gr_table
   CHANGING t_table = lt_final_report[] ).
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
  gr_column ?= gr_columns->get_column( 'ZRECNO' ).
  gr_column->set_long_text( 'Request_Number' ).
  gr_column->set_medium_text( 'Request_Num' ).
  gr_column->set_short_text( 'Request' ).

  gr_columns = gr_table->get_columns( ).
  gr_column ?= gr_columns->get_column( 'ITEM_ID' ).
  gr_column->set_long_text( 'Item_ID' ).
  gr_column->set_medium_text( 'Item_ID' ).
  gr_column->set_short_text( 'Item_ID' ).


  gr_columns = gr_table->get_columns( ).
  gr_column ?= gr_columns->get_column( 'PLANT_CODE' ).
  gr_column->set_long_text( 'PLANT_CODE' ).
  gr_column->set_medium_text( 'PLANT_CODE' ).
  gr_column->set_short_text( 'PLANT_CODE' ).


  gr_column ?= gr_columns->get_column( 'PLANT_NAME' ).
  gr_column->set_long_text( 'PLANT_NAME' ).
  gr_column->set_medium_text( 'PLANT_NAME' ).
  gr_column->set_short_text( 'PLANT_NAME' ).
  gr_column->set_output_length( 13 ).

  gr_column ?= gr_columns->get_column( 'CUSTOMER_CODE' ).
  gr_column->set_long_text( 'CUSTOMER_CODE' ).
  gr_column->set_medium_text( 'CUSTOMER_CODE' ).
  gr_column->set_short_text( 'CUST_CODE' ).
  gr_column->set_output_length( 13 ).

  gr_column ?= gr_columns->get_column( 'REGION_NAME' ).
  gr_column->set_long_text( 'Region_Name' ).
  gr_column->set_medium_text( 'Region' ).
  gr_column->set_short_text( 'Region' ).
  gr_column->set_output_length( 13 ).

  gr_column ?= gr_columns->get_column( 'CUSTOMER_NAME' ).
  gr_column->set_long_text( 'CUSTOMER_NAME' ).
  gr_column->set_medium_text( 'CUSTOMER_NAME' ).
  gr_column->set_short_text( 'CUST_NAME' ).
  gr_column->set_output_length( 30 ).

  gr_column ?= gr_columns->get_column( 'WASTE_CODE' ).
  gr_column->set_long_text( 'WASTE_CODE' ).
  gr_column->set_medium_text( 'WASTE_CODE' ).
  gr_column->set_short_text( 'WAST_CODE' ).
  gr_column->set_output_length( 20 ).

  gr_column ?= gr_columns->get_column( 'WASTE_NAME' ).
  gr_column->set_long_text( 'WASTE_NAME' ).
  gr_column->set_medium_text( 'WASTE_NAME' ).
  gr_column->set_short_text( 'WAST_NAME' ).
  gr_column->set_output_length( 30 ).

  gr_column ?= gr_columns->get_column( 'AVG_PH' ).
  gr_column->set_long_text( 'AVG_PH' ).
  gr_column->set_medium_text( 'AVG_PH' ).
  gr_column->set_short_text( 'AVG_PH' ).
  gr_column->set_output_length( 8 ).

  gr_column ?= gr_columns->get_column( 'AVG_CV' ).
  gr_column->set_long_text( 'AVG_CV' ).
  gr_column->set_medium_text( 'AVG_CV' ).
  gr_column->set_short_text( 'AVG_CV' ).
  gr_column->set_output_length( 8 ).

  gr_column ?= gr_columns->get_column( 'AVG_CL' ).
  gr_column->set_long_text( 'AVG_CL' ).
  gr_column->set_medium_text( 'AVG_CL' ).
  gr_column->set_short_text( 'AVG_CL' ).
  gr_column->set_output_length( 8 ).

  gr_column ?= gr_columns->get_column( 'AVG_VOC' ).
  gr_column->set_long_text( 'AVG_VOC' ).
  gr_column->set_medium_text( 'AVG_VOC' ).
  gr_column->set_short_text( 'AVG_VOC' ).
  gr_column->set_output_length( 8 ).
*
*  gr_column ?= gr_columns->get_column( 'QC_CL_RANGE' ).
*  gr_column->set_long_text( 'QC_CL_RANGE' ).
*  gr_column->set_medium_text( 'QC_CL_RANGE' ).
*  gr_column->set_short_text( 'QC_CLR' ).
*  gr_column->set_output_length( 12 ).
*
*
*  gr_column ?= gr_columns->get_column( 'MATERIAL_EASINESS' ).
*  gr_column->set_long_text( 'MATERIAL_EASINESS' ).
*  gr_column->set_medium_text( 'MAT_EASINESS' ).
*  gr_column->set_short_text( 'MAT_EASY' ).
*  gr_column->set_output_length( 15 ).
*
*  gr_column ?= gr_columns->get_column( 'PHYSICAL_STATE' ).
*  gr_column->set_long_text( 'PHYSICAL_STATE' ).
*  gr_column->set_medium_text( 'PHYSICAL_STATE' ).
*  gr_column->set_short_text( 'PHYSTATE' ).
*  gr_column->set_output_length( 15 ).

*  gr_column ?= gr_columns->get_column( 'SERVICE_CODE' ).
*  gr_column->set_long_text( 'SERVICE_CODE' ).
*  gr_column->set_medium_text( 'SERVICE_CODE' ).
*  gr_column->set_short_text( 'SERCODE' ).
*  gr_column->set_output_length( 10 ).
*
*  gr_column ?= gr_columns->get_column( 'COMPETITION_AVAILABLE' ).
*  gr_column->set_long_text( 'COMPETITION_AVAILABLE' ).
*  gr_column->set_medium_text( 'COMPET_AVAIL' ).
*  gr_column->set_short_text( 'COMPAVAL' ).
*  gr_column->set_output_length( 20 ).
*
*  gr_column ?= gr_columns->get_column( 'ABC_XYZ' ).
*  gr_column->set_long_text( 'ABC_XYZ' ).
*  gr_column->set_medium_text( 'ABC_XYZ' ).
*  gr_column->set_short_text( 'ABC_XYZ' ).
*  gr_column->set_output_length( 7 ).
*
*  gr_column ?= gr_columns->get_column( 'FP_PHYSTATE' ).
*  gr_column->set_long_text( 'FP_PHYSTATE' ).
*  gr_column->set_medium_text( 'FP_PHYSTATE' ).
*  gr_column->set_short_text( 'FP_PHY' ).
*  gr_column->set_output_length( 12 ).
*
*  gr_column ?= gr_columns->get_column( 'FP_CL' ).
*  gr_column->set_long_text( 'FP_CL' ).
*  gr_column->set_medium_text( 'FP_CL' ).
*  gr_column->set_short_text( 'FP_CL' ).
*  gr_column->set_output_length( 8 ).
*
*  gr_column ?= gr_columns->get_column( 'FP_CL_RANGE' ).
*  gr_column->set_long_text( 'FP_CL_RANGE' ).
*  gr_column->set_medium_text( 'FP_CL_RANGE' ).
*  gr_column->set_short_text( 'FP_CLR' ).
*  gr_column->set_output_length( 12 ).

*  gr_column ?= gr_columns->get_column( 'WEIGHT_AVG_RATE' ).
*  gr_column->set_long_text( 'WEIGHT_AVG_RATE' ).
*  gr_column->set_medium_text( 'WEIGHT_AVG_RATE' ).
*  gr_column->set_short_text( 'WT_AVG' ).
*  gr_column->set_output_length( 15 ).
*
*  gr_column ?= gr_columns->get_column( 'MST_PROC_RATE' ).
*  gr_column->set_long_text( 'MST_PROC_RATE' ).
*  gr_column->set_medium_text( 'MST_PROC_RATE' ).
*  gr_column->set_short_text( 'MST_RATE' ).
*  gr_column->set_output_length( 13 ).

*  gr_column ?= gr_columns->get_column( 'SAFTY_SCORE' ).
*  gr_column->set_long_text( 'SAFTY_SCORE' ).
*  gr_column->set_medium_text( 'SAFTY_SCORE' ).
*  gr_column->set_short_text( 'SFT_SCORE' ).
*  gr_column->set_output_length( 11 ).

*  gr_column ?= gr_columns->get_column( 'MATERIAL_EASINESS_SCORE' ).
*  gr_column->set_long_text( 'MATERIAL_EASINESS_SCORE' ).
*  gr_column->set_medium_text( 'MAT_SCORE' ).
*  gr_column->set_short_text( 'MAT_SCORE' ).
*  gr_column->set_output_length( 21 ).

*  gr_column ?= gr_columns->get_column( 'QUALITY_SCORE' ).
*  gr_column->set_long_text( 'QUALITY_SCORE' ).
*  gr_column->set_medium_text( 'QUALITY_SCORE' ).
*  gr_column->set_short_text( 'QUL_SCORE' ).
*  gr_column->set_output_length( 12 ).
*
*  gr_column ?= gr_columns->get_column( 'PRICE_SCORE' ).
*  gr_column->set_long_text( 'PRICE_SCORE' ).
*  gr_column->set_medium_text( 'PRICE_SCORE' ).
*  gr_column->set_short_text( 'PRC_SCORE' ).
*  gr_column->set_output_length( 11 ).

*  gr_column ?= gr_columns->get_column( 'CUSTOMER_SCORE' ).
*  gr_column->set_long_text( 'CUSTOMER_SCORE' ).
*  gr_column->set_medium_text( 'CUSTOMER_SCORE' ).
*  gr_column->set_short_text( 'CUS_SCORE' ).
*  gr_column->set_output_length( 13 ).
*
*  gr_column ?= gr_columns->get_column( 'COMPETITION_SCORE' ).
*  gr_column->set_long_text( 'COMPETITION_SCORE' ).
*  gr_column->set_medium_text( 'COMPETITION_SCORE' ).
*  gr_column->set_short_text( 'COM_SCORE' ).
*  gr_column->set_output_length( 17 ).

*  gr_column ?= gr_columns->get_column( 'INDEX_SCORE' ).
*  gr_column->set_long_text( 'INDEX_SCORE' ).
*  gr_column->set_medium_text( 'INDEX_SCORE' ).
*  gr_column->set_short_text( 'IDX_SCORE' ).
*  gr_column->set_output_length( 11 ).
*
*  gr_column ?= gr_columns->get_column( 'LIFTING_PRIORITY' ).
*  gr_column->set_long_text( 'LIFTING_PRIORITY' ).
*  gr_column->set_medium_text( 'LIFT_PRIORITY' ).
*  gr_column->set_short_text( 'LFT_PRIOR' ).
*  gr_column->set_output_length( 14 ).

  gr_table->display( ).
