*&---------------------------------------------------------------------*
*& Report ZCUST_CNT_MASTER_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcust_cnt_master_report.

TABLES: knvv, but000, kna1, adrc, adr6, t005u, tzont, t001w, tvgrt, t151t, tvkbt, tbrct, but051, but050, adr2, tb905t, tb913, tb911  .



*&---------------------------------------------------------------------*
*& Include          ZDT_DECLAIRATION_TOP
*&---------------------------------------------------------------------*




TYPES: BEGIN OF ty_final,
         vkorg         TYPE vkorg,    "sales orgnx
         name1         TYPE t001w-name1,
         partner       TYPE bu_partner,
         bpext         TYPE but000-bpext,
         name_org1     TYPE but000-name_org1,
         name_org2     TYPE but000-name_org2,
*         customer_name TYPE string,
         street        TYPE adrc-street,       " Address 1 (ADDRESS1)
         str_suppl1    TYPE adrc-str_suppl1,   " Address 2 (ADDRESS2)
         str_suppl2    TYPE adrc-str_suppl2,   " Address 3 (ADDRESS3)
         str_suppl3    TYPE adrc-str_suppl3,   " Address 4 (ADDRESS4)
         location      TYPE   adrc-location,
         city2         TYPE   adrc-city2,
         post_code1    TYPE  adrc-post_code1,
         city1         TYPE  adrc-city1,
         addrnumber    TYPE adrc-ADDRNUMBER,   "for testing
         regio         TYPE kna1-regio,
         bezeis        TYPE t005u-bezei,  "state name
         smtp_addr     TYPE adr6-smtp_addr,
         lzone         TYPE kna1-lzone,      "TRANSPORTATION_ZONE_CODE
         vtext         TYPE tzont-vtext ,    "TRANSPORTATION_ZONE_NAME
         Locco         TYPE kna1-Locco,
         vkgrp         TYPE vkgrp,   "Sales Group - REGION_CODE
         bezeiii       TYPE tvgrt-bezei,   "REGION_NAME
         kdgrp         TYPE kdgrp,  "Customer Group
         ktext         TYPE t151t-ktext  , " as INDUSTRY_SCALE_NAME,
         vkbur         TYPE vkbur,   "Sales Office code
         bezei         TYPE    tvkbt-bezei , "SALES_OFFICE_NAME
         niels         TYPE kna1-niels , "n.NIELS GRADE
         bran1         TYPE kna1-bran1 , "BRAN1 INDUSTRY_TYPE_CODE
         vtext_ins     TYPE tbrct-vtext , "  INDUSTRY_TYPE_NAME,
         xdele         TYPE but000-xdele    ,  "
         partner2      TYPE   but051-partner2 ,  " CONT_PERSON_CODE
*         smtp_address  TYPE  but051-smtp_address ,                  "cont person email
*         tel_number    TYPE but051-tel_number,                     "telephone num

         name_first    TYPE but000-name_first ,   " CONT_NAME1
         name_last     TYPE but000-name_last ,   " CONT_NAME2
         tel_number    TYPE adr2-tel_number ,  " as CONT_TEL_NUMBER1
         smtp_addre    TYPE adr6-smtp_addr,
**  persnumber    TYPE but000-persnumber,
         customer_name TYPE string,
*         kunnr      TYPE kunnr,
*         land1      TYPE kna1-land1,
         helper_field  TYPE string,
       END OF ty_final.




*DATA : lt_result TYPE TABLE OF ty_final,
*       ls_result TYPE ty_final.

DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv,
      ls_layout   TYPE slis_layout_alv,    "here i created work area and slis_fieldcat_alv i got it by double tap on slis_t_fieldcat_alv, and will use for creating maually fieldcatlog
      lt_result   TYPE TABLE OF ty_final,
      ls_result   TYPE ty_final,
*      lt_list     TYPE slis_t_listheader,
*      ls_list     TYPE slis_listheader,
      lv_input    TYPE string.

INCLUDE zdt_declairation_top.
DATA : lv_partnr TYPE bu_partner,
*DATA: lt_knvv_temp TYPE TABLE OF knvv.
       lt_emails TYPE TABLE OF adr6-smtp_addr,          " Table to store multiple emails temporarily
       lv_emails TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_vkorg TYPE vkorg.
  SELECT-OPTIONS : s_partnr FOR lv_partnr.

SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

**  SELECT knvv~vkorg,
**         knvv~kunnr,
**         MAX( knvv~kdgrp ) AS kdgrp,
**         MAX( knvv~vkgrp ) AS vkgrp,
**         MAX( knvv~vkbur ) AS vkbur
**    INTO TABLE @lt_knvv_temp
**    FROM knvv
**    WHERE NOT EXISTS ( SELECT 1
**                         FROM vbak
**                        WHERE vbak~kunnr = knvv~kunnr
**                          AND vbak~vkorg = knvv~vkorg )
**   GROUP BY knvv~vkorg, knvv~kunnr, knvv~mandt.

  SELECT knvv~vkorg,
         t001w~name1,
       but000~partner,
       but000~bpext ,
       but000~name_org1,
       but000~name_org2,
*        CONCATENATE ( but000~name_org1  but000~name_org2 )
*            INTO name_org_full SEPARATED BY space ,
*       CONCAT but000~name_org1
*       CONCAT ( but000~name_org1, '' , but000~name_org2 ) AS name_org_full,
       adrc~street,
       adrc~str_suppl1,
       adrc~str_suppl2,
       adrc~str_suppl3,
       adrc~location,
       adrc~city2,
       adrc~post_code1,
       adrc~city1,
       adrc~addrnumber, """for testing
       kna1~regio AS State_Code,
       t005u~bezei AS state_name,
      adr6~smtp_addr,
*    STRING_AGG ( adr6~smtp_addr, ' , ') AS smtp_addr,  " Concatenate email addresses
       kna1~lzone   AS transportation_zone_code ,
       tzont~vtext AS transportation_zone_name,   "kn.LOCCO MASTER_KMBPd
       kna1~Locco  AS master_km_bp,
       knvv~vkgrp AS region_code,
       tvgrt~bezei AS region_name,
       knvv~kdgrp AS industry_scale_code,
       t151t~ktext  AS industry_scale_name,
       knvv~vkbur AS sales_office_code,
       tvkbt~bezei AS     bezei_sales_name  ,    "TYPE    tvkbt-bezei , "SALES_OFFICE_NAME
       kna1~niels AS grADE ,
      kna1~bran1 AS industry_type_code,
      tbrct~vtext AS  industry_type_name,
*      but000~xdele ,
       CASE
         WHEN but000~xdele = 'X' THEN 'INACTIVE'
         ELSE 'ACTIVE'
       END AS xdele_status,
       but051~partner2 AS cont_person_code ,  " CONT_PERSON_CODE
*       but051~SMTP_ADDRESS ,                  "cont person email
*       but051~TEL_NUMBER,                     "telephone num

       b0~name_first ,   " CONT_NAME1
       b0~name_last  ,  " CONT_NAME2
       adr2~tel_number  , " as CONT_TEL_NUMBER1
      adrs~smtp_addr
*       b0~PERSNUMBER
*         knvv~kunnr,

*         but000~name_org2,
*         but000~name_org3,
*         but000~name_org4,

*         kna1~land1

    INTO TABLE @lt_result
    FROM knvv
    LEFT JOIN but000 ON knvv~kunnr = but000~partner
    LEFT JOIN kna1 ON knvv~kunnr = kna1~kunnr
    LEFT JOIN adrc ON kna1~adrnr = adrc~addrnumber
    LEFT JOIN adr6 ON adrc~addrnumber = adr6~addrnumber
    LEFT JOIN t001w ON t001w~werks =  knvv~vkorg
    LEFT JOIN t005u ON t005u~bland = kna1~regio AND t005u~spras = 'E' AND t005u~land1 = 'IN'
    LEFT JOIN tzont ON tzont~zone1 = kna1~lzone AND t005u~spras = 'E' AND t005u~land1 = 'IN'
    LEFT JOIN tvgrt ON tvgrt~vkgrp = knvv~vkgrp AND tvgrt~spras = 'E'
    LEFT JOIN t151t ON t151t~kdgrp = knvv~kdgrp AND t151t~spras = 'E'
    LEFT JOIN tvkbt  ON tvkbt~vkbur = knvv~vkbur AND tvkbt~spras = 'E'
*     LEFT JOIN lt_knvv_temp AS sub_knvv ON sub_knvv~kunnr = but000~partner
*                                      AND sub_knvv~vkorg = knvv~vkorg
     LEFT JOIN tbrct ON tbrct~braco = kna1~bran1 AND tbrct~spras = 'E'
     LEFT JOIN but051 ON but051~partner1 = but000~partner
*     left outer  JOIN BUT050  on BUT050~PARTNER2 = but051~PARTNER2 and but051~PARTNER1 = BUT050~PARTNER1
     LEFT JOIN but000 AS b0 ON b0~partner = but051~partner2
     LEFT JOIN adr2  ON adr2~persnumber = b0~persnumber " AND adr2~consnumber = '001'
     LEFT JOIN adr6 AS adrs ON adrs~persnumber = b0~persnumber "AND adr2~consnumber = '001'
   WHERE knvv~vkorg = @p_vkorg
     AND but000~partner IN @s_partnr.



end-of-SELECTION.

  DATA: lv_flag TYPE string.
  DATA : ls_previous TYPE string.

  LOOP AT lt_result INTO ls_result.
    CONCATENATE ls_result-name_org1 ls_result-name_org2 INTO ls_result-customer_name SEPARATED BY ' ' .

    " Fetch the email addresses for the current address number (kna1~adrnr)
    CLEAR: lt_emails, lv_emails.
    SELECT smtp_addr
      INTO TABLE lt_emails
      FROM adr6
      WHERE addrnumber = ls_result-addrnumber.

    LOOP AT lt_emails INTO DATA(ls_email).
      IF lv_emails IS INITIAL.
        lv_emails = ls_email.
      ELSE.
        CONCATENATE lv_emails ls_email INTO lv_emails SEPARATED BY ', '.
      ENDIF.
    ENDLOOP.

    " Update the result structure with concatenated emails
    ls_result-smtp_addr = lv_emails.



    IF ls_result-partner2 IS INITIAL.
      lv_flag = 'X' .
    ENDIF.

    MODIFY lt_result FROM ls_result .
  ENDLOOP.
  SORT lt_result BY partner partner2.
  "IF lv_flag IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_result COMPARING partner   partner2. "smtp_addr
  "ENDIF.


  IF sy-subrc = 0.
*    PERFORM display_data1.
    PERFORM display_data.
  ELSE.
    MESSAGE 'No data found for the given selection criteria.' TYPE 'I'.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.




**  DATA: lo_alv         TYPE REF TO cl_salv_table,
**        lt_fcat        TYPE lvc_t_fcat,   " Field catalog table
**        ls_fcat        TYPE lvc_s_fcat,   " Field catalog line
**        lx_msg         TYPE REF TO cx_salv_msg,
**        lv_salv_column TYPE REF TO cl_salv_columns_table,
**        lo_functions   TYPE REF TO cl_salv_functions_list.
**
**  " Define field catalog entries
**  CLEAR: lt_fcat, ls_fcat.
**
**  " 1. Field: VKORG (Sales Org)
**  ls_fcat-fieldname = 'VKORG'.       " Field name in the internal table lt_result
**  ls_fcat-seltext = 'Sales Org'.   " Column heading
**  ls_fcat-outputlen = 4.             " Output length for the field
**  ls_fcat-datatype  = 'CHAR'.        " Data type
**  APPEND ls_fcat TO lt_fcat.
**
**  " 2. Field: NAME1 (Plant Name)
**  ls_fcat-fieldname = 'NAME165'.
**  ls_fcat-seltext = 'Plant saurah Name'.
**  ls_fcat-outputlen = 20.
**  ls_fcat-datatype  = 'CHAR'.
**  APPEND ls_fcat TO lt_fcat.
**
**  " 3. Field: PARTNER (Partner)
**  ls_fcat-fieldname = 'PARTNER'.
**  ls_fcat-seltext = 'Partner'.
**  ls_fcat-outputlen = 10.
**  ls_fcat-datatype  = 'CHAR'.
**  APPEND ls_fcat TO lt_fcat.
**
**  " Add more fields as needed for your report
**  " ... add other fields following the same structure above
**
**  TRY.
**      " Generate ALV table
**      CALL METHOD cl_salv_table=>factory
**        IMPORTING
**          r_salv_table = lo_alv
**        CHANGING
**          t_table      = lt_result.
**
**      " Apply the field catalog (layout settings)
**      lo_alv->get_columns( )->set_optimize( abap_true ).
**
**      " Enable ALV functions like filter and sorting
**      lo_functions = lo_alv->get_functions( ).
**      lo_functions->set_filter( abap_true ).
**      lo_functions->set_default( abap_true ).
**
**      " Display the ALV grid
**      lo_alv->display( ).
**
**    CATCH cx_salv_msg INTO lx_msg.
**      MESSAGE lx_msg->get_text( ) TYPE 'S'.
**    CATCH cx_salv_object_not_found INTO DATA(lx_obj_not_found).
**      MESSAGE lx_obj_not_found->get_text( ) TYPE 'S'.
**  ENDTRY.
**

*using function module


  " PLANT_CODE
  ls_fieldcat-col_pos = '0'.
  ls_fieldcat-fieldname = 'VKORG'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Plant Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " PLANT_NAME
  ls_fieldcat-col_pos = '1'.
  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Plant Name'.
  ls_fieldcat-outputlen =  15 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " SAP_CUSTOMER_CODE
  ls_fieldcat-col_pos = '2'.
  ls_fieldcat-fieldname = 'PARTNER'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'SAP Customer Code'.
  ls_fieldcat-outputlen =  15 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " CUSTOMER_NAME
  ls_fieldcat-col_pos = '3'.
  ls_fieldcat-fieldname = 'CUSTOMER_NAME'.  " Use the new field
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Customer Name'.
  ls_fieldcat-outputlen =  40 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


*" CUSTOMER_NAME
*ls_fieldcat-col_pos = '5'.
*ls_fieldcat-fieldname = 'name_org2'.
*ls_fieldcat-tabname = 'lt_result'.
*ls_fieldcat-seltext_l = 'Customer Name'.
*APPEND ls_fieldcat TO lt_fieldcat.
*CLEAR ls_fieldcat.

  " ADDRESS1
  ls_fieldcat-col_pos = '4'.
  ls_fieldcat-fieldname = 'STREET'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Address 1'.
  ls_fieldcat-outputlen =  35 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " ADDRESS2
  ls_fieldcat-col_pos = '5'.
  ls_fieldcat-fieldname = 'STR_SUPPLL'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Address 2'.
  ls_fieldcat-outputlen =  25 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " ADDRESS3
  ls_fieldcat-col_pos = '6'.
  ls_fieldcat-fieldname = 'STR_SUPPL2'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Address 3'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " ADDRESS4
  ls_fieldcat-col_pos = '7'.
  ls_fieldcat-fieldname = 'STR_SUPPL3'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Address 4'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " DISTRICT
  ls_fieldcat-col_pos = '8'.
  ls_fieldcat-fieldname = 'CITY2'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'District'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " POST_CODE
  ls_fieldcat-col_pos = '9'.
  ls_fieldcat-fieldname = 'POST_CODE1'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Post Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " CITY
  ls_fieldcat-col_pos = '10'.
  ls_fieldcat-fieldname = 'CITY1'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'City'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " STATE_CODE
  ls_fieldcat-col_pos = '11'.
  ls_fieldcat-fieldname = 'REGIO'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'State Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " STATE_NAME
  ls_fieldcat-col_pos = '12'.
  ls_fieldcat-fieldname = 'BEZEIS'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'State Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " SMTP_ADDR
  ls_fieldcat-col_pos = '13'.
  ls_fieldcat-fieldname = 'SMTP_ADDR'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'SMTP Address'.
  ls_fieldcat-outputlen =  30.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " TRANSPORTATION_ZONE_CODE
  ls_fieldcat-col_pos = '14'.
  ls_fieldcat-fieldname = 'LZONE'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Transportation Zone Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " TRANSPORTATION_ZONE_NAME
  ls_fieldcat-col_pos = '15'.
  ls_fieldcat-fieldname = 'VTEXT'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Transportation Zone Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " MASTER_KM_BP
  ls_fieldcat-col_pos = '16'.
  ls_fieldcat-fieldname = 'LOCCO'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Master KM BP'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " REGION_CODE
  ls_fieldcat-col_pos = '17'.
  ls_fieldcat-fieldname = 'VKGRP'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Region Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " REGION_NAME
  ls_fieldcat-col_pos = '18'.
  ls_fieldcat-fieldname = 'BEZEIII'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Region Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " INDUSTRY_SCALE_CODE
  ls_fieldcat-col_pos = '19'.
  ls_fieldcat-fieldname = 'KDGRP'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Industry Scale Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " INDUSTRY_SCALE_NAME
  ls_fieldcat-col_pos = '20'.
  ls_fieldcat-fieldname = 'KTEXT'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Industry Scale Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " SALES_OFFICE_CODE
  ls_fieldcat-col_pos = '21'.
  ls_fieldcat-fieldname = 'VKBUR'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Sales Office Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " SALES_OFFICE_NAME
  ls_fieldcat-col_pos = '22'.
  ls_fieldcat-fieldname = 'BEZEI'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Sales Office Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " GRADE
  ls_fieldcat-col_pos = '23'.
  ls_fieldcat-fieldname = 'NEILS'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Grade'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " INDUSTRY_TYPE_CODE
  ls_fieldcat-col_pos = '24'.
  ls_fieldcat-fieldname = 'BRANL'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Industry Type Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " INDUSTRY_TYPE_NAME
  ls_fieldcat-col_pos = '25'.
  ls_fieldcat-fieldname = 'VTEXT_INS'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Industry Type Name'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " XDELE
  ls_fieldcat-col_pos = '26'.
  ls_fieldcat-fieldname = 'XDELE'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'XDELE'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " CONT_PERSON_CODE
  ls_fieldcat-col_pos = '27'.
  ls_fieldcat-fieldname = 'PARTNER2'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Cont Person Code'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ""  but051~SMTP_ADDRESS ,                  "cont person email
*       but051~TEL_NUMBER,                     "telephone num

*ls_fieldcat-col_pos = '29'.
*ls_fieldcat-fieldname = 'SMTP_ADDRESS'.
*ls_fieldcat-tabname = 'lt_result'.
*ls_fieldcat-seltext_l = 'Email Addres'.
*APPEND ls_fieldcat TO lt_fieldcat.
*CLEAR ls_fieldcat.
*


  " CONT_NAME1
  ls_fieldcat-col_pos = '28'.
  ls_fieldcat-fieldname = 'NAME_FIRST'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Cont Name 1'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  " CONT_NAME2
  ls_fieldcat-col_pos = '29'.
  ls_fieldcat-fieldname = 'NAME_LAST'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Cont Name 2'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-col_pos = '30'.
  ls_fieldcat-fieldname = 'TEL_NUMBER'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Tele. Number'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '31'.
  ls_fieldcat-fieldname = 'SMTP_ADDRE'.
  ls_fieldcat-tabname = 'lt_result'.
  ls_fieldcat-seltext_l = 'Email Address'.
  ls_fieldcat-outputlen =  30 .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     i_callback_top_of_page = 'TOP_PAGE'
*     I_CALLBACK_USER_COMMAND        = ' '
      i_structure_name   = 'ty_final'
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat[]
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IT_EXCEPT_QINFO    =
*     I_SUPPRESS_EMPTY_DATA  = ABAP_FALSE
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER =
    TABLES
      t_outtab           = lt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here

*    IF sy-subrc = 0.
*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename         = 'D:\temp\output.xls'
*          filetype         = 'ASC'
*        TABLES
*          data_tab         = lt_result
*        EXCEPTIONS
*          file_write_error = 1
*          OTHERS           = 2.
*    ENDIF.
  ENDIF.


ENDFORM.

INCLUDE zt_cnt_master_report_displaf01.
