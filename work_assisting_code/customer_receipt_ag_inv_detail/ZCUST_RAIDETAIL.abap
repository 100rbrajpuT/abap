*&---------------------------------------------------------------------*
*& Report ZCUST_RAIDETAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcust_raidetail.


TYPES: BEGIN OF ty_bsid,
         bukrs TYPE bukrs,
         prctr TYPE prctr,
         kunnr TYPE kunnr,
         budat TYPE budat,
         gjahr TYPE gjahr,
         blart TYPE blart,
         hkont TYPE hkont,
         belnr TYPE belnr_d,
         wrbtr TYPE wrbtr,
         vbeln TYPE vbeln_vf,
         rebzg TYPE rebzg,     "REBZG
       END OF ty_bsid.

DATA: it_bsid TYPE TABLE OF ty_bsid,
      wa_bsid TYPE ty_bsid.

TYPES: BEGIN OF ty_acdoca,
         rbukrs TYPE bukrs,
         werks  TYPE werks_d,
         prctr  TYPE prctr,
         belnr  TYPE belnr_d,
*         kunnr TYPE kunnr,
*         budat TYPE budat,
         bldat  TYPE bldat,    "INVOICEDATE
         budat TYPE budat ,    "invoice posting date
*         gjahr TYPE gjahr,
*         blart TYPE blart,
*         hkont TYPE hkont,
*         belnr TYPE belnr_d,
*         wrbtr TYPE wrbtr,
*         vbeln TYPE vbeln_vf,
*         rebzg TYPE rebzg,
       END OF ty_acdoca.

DATA: it_acdoca TYPE TABLE OF ty_acdoca,
      wa_acdoca TYPE ty_acdoca.


TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         regio TYPE regio,
         " KUNNR TYPE KUNNR,
       END OF ty_kna1.

DATA: it_kna1 TYPE TABLE OF ty_kna1,
      wa_kna1 TYPE ty_kna1.

TYPES: BEGIN OF ty_bseg,    "bukrs, belnr,gjahr, wrbtr ,hkont, prctr, kunnr, sgtxt
         bukrs TYPE bukrs,
*         prctr TYPE prctr,
*         kunnr TYPE kunnr,
*         budat TYPE budat,
*         gjahr TYPE gjahr,
*         belnr TYPE belnr_d,
         wrbtr TYPE wrbtr,
         kunnr TYPE kunnr,
         belnr TYPE belnr_d,
*         vbeln TYPE vbeln_vf,
*         rebzg TYPE rebzg,
       END OF ty_bseg.

DATA: it_bseg TYPE TABLE OF ty_bseg,
      wa_bseg TYPE ty_bseg.

TYPES: BEGIN OF ty_report,
         companycode        TYPE bukrs,
         plant              TYPE werks_d,
         plantname          TYPE name1,
         customercode       TYPE kunnr,
         customername       TYPE name1,
         receiptdate        TYPE char10,
         receiptno          TYPE belnr_d ,  "belnr,
         receiptamount      TYPE wrbtr,
         sd_invoice_no      TYPE vbeln_vf, "vbeln,
         fi_invoice_no      TYPE   rebzg,  "belnr,
         invoicedate        TYPE bldat,    "INVOICEDATE   char10,
         invoicepostingdate TYPE   budat ,  " invoice posting date"     "char10,
         invoiceamount      TYPE wrbtr,
         paymentadjustamt   TYPE wrbtr,
         noofdays           TYPE dats,
       END OF ty_report.

DATA: it_report TYPE TABLE OF ty_report,
      wa_report TYPE ty_report.

DATA : bukrs TYPE bukrs,
       kunnr TYPE kunnr,
       budat TYPE budat.

*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      w_layout    TYPE slis_layout_alv.
w_layout-zebra = 'X'.
*RANGES: r_field FOR vbak-kunnr.

DATA: ispfli TYPE TABLE OF spfli.
DATA: gr_table TYPE REF TO cl_salv_table.
DATA: gr_functions TYPE REF TO cl_salv_functions.
DATA: gr_display TYPE REF TO cl_salv_display_settings.
DATA: gr_columns TYPE REF TO cl_salv_columns_table.
DATA: gr_column TYPE REF TO cl_salv_column_table.
DATA: gr_layout TYPE REF TO cl_salv_layout.
DATA: key TYPE salv_s_layout_key.
DATA: color TYPE lvc_s_colo.

" Step 2: Define the selection screen
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
    s_comny FOR bukrs OBLIGATORY,   " Company Code
    s_cust FOR kunnr,             " Customer Code
    s_date FOR budat .          " Date

SELECTION-SCREEN END OF BLOCK blk1.

START-OF-SELECTION.


  SELECT bukrs ,prctr , kunnr , budat , gjahr ,blart, hkont , belnr , wrbtr , vbeln ,  rebzg "com code , profit center , customer code , RECEIPTDATE ,
                                                        "belnr - RECEIPTDOCUMENTNO ,WRBTR- RECEIPTAMOUNT , VBELN -  SDINVOICENO , REBZG - FIINVOICENO
    FROM bsid_view
    INTO TABLE  @it_bsid
    WHERE blart IN ('DP','DW','AB','DZ')
    AND hkont NOT IN  ('0000253230','0000130932')
    AND  bukrs IN   @s_comny
    AND   kunnr IN @s_cust
    AND budat IN @s_date .


  IF  it_bsid IS NOT INITIAL.
    SELECT rbukrs ,werks,  prctr , belnr ,  bldat , budat
    FROM acdoca
    INTO TABLE @it_acdoca
     FOR ALL ENTRIES IN @it_bsid
      WHERE  belnr   = @it_bsid-belnr
      AND gjahr  =   @it_bsid-gjahr
      AND rbukrs IN   @s_comny
      AND   kunnr IN @s_cust
      AND budat IN @s_date .

  ENDIF.

  IF it_bsid IS NOT INITIAL.
    SELECT  kunnr , name1  , regio    "customer name .  regio in not required
      FROM kna1
      INTO TABLE @it_kna1
      FOR ALL ENTRIES IN @it_bsid
      WHERE kunnr  = @it_bsid-kunnr .
  ENDIF.

  IF it_bsid IS NOT INITIAL.
*    SELECT  name1  , regio    "customer name .  regio in not required
*      FROM bseg
*      INTO TABLE @it_kna1
*      FOR ALL ENTRIES IN @it_bsid
*      WHERE kunnr  = @it_bsid-kunnr .
*      SELECT a~bukrs, a~belnr, a~gjahr, a~wrbtr, a~hkont, a~prctr, a~kunnr, a~sgtxt
**    INTO TABLE @DATA(it_bseg)
*    FROM bsid_view AS c
*    LEFT JOIN bseg AS a
*      ON a~bukrs = c~bukrs
*     AND a~gjahr = c~gjahr
*     AND a~belnr = c~belnr
*     WHERE a~h_blart IN ('DP', 'DZ', 'AB', 'DW')  " Filter condition for document type
*      AND a~prctr <> ''                          " Profit center must not be blank
*      AND a~mwskz = ''
*          INTO TABLE @DATA(it_bseg) .

    SELECT bukrs, wrbtr , kunnr , belnr " belnr,gjahr, wrbtr ,hkont, prctr, kunnr, sgtxt
     INTO TABLE @it_bseg
    FROM bseg
      FOR ALL ENTRIES IN @it_bsid
    WHERE bukrs = @it_bsid-bukrs
    "  and kunnr  = @it_bsid-kunnr
    AND gjahr = @it_bsid-gjahr
    AND belnr = @it_bsid-belnr
    AND h_blart IN ('DP', 'DZ', 'AB', 'DW')  " Filter condition for document type
    AND prctr <> ''                          " Profit center must not be blank
    AND mwskz = '' .
  ENDIF.

END-OF-SELECTION.




  LOOP AT it_bsid INTO wa_bsid.

    CLEAR wa_report.

    wa_report-companycode = wa_bsid-bukrs.
    wa_report-plant = wa_bsid-prctr.
    wa_report-customercode = wa_bsid-kunnr.
    wa_report-receiptdate = wa_bsid-budat.
    wa_report-receiptno = wa_bsid-belnr.
    wa_report-receiptamount = wa_bsid-wrbtr.
    wa_report-sd_invoice_no = wa_bsid-vbeln.
    wa_report-fi_invoice_no = wa_bsid-rebzg.

    IF wa_bsid-prctr IS NOT INITIAL.
      SELECT SINGLE werks  name1
        FROM t001w
        INTO  (wa_report-plant ,  wa_report-plantname  )
        WHERE werks = wa_bsid-prctr(4).
    ELSE.
      READ TABLE it_acdoca INTO wa_acdoca WITH KEY belnr   = wa_bsid-belnr.
      IF sy-subrc = 0.
        wa_report-plant =  wa_acdoca-werks.
        SELECT SINGLE werks  name1
        FROM t001w
        INTO  (wa_report-plant ,  wa_report-plantname )
        WHERE werks =  wa_acdoca-prctr(4).
      ENDIF.
    ENDIF.


    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr.
    IF sy-subrc = 0.
      wa_report-customername = wa_kna1-name1.
    ELSE.
      wa_report-customername = 'Unknown'.
    ENDIF.

    " Retrieve invoice date (bldat) from it_acdoca and append to the report
    READ TABLE it_acdoca INTO wa_acdoca WITH KEY belnr = wa_bsid-belnr.
    IF sy-subrc = 0.
      wa_report-invoicedate = wa_acdoca-bldat. " Appending bldat (invoice date)
      wa_report-invoicepostingdate =  wa_acdoca-budat.
    ELSE.
      wa_report-invoicedate = 'Unknown'. " In case the invoice date is not found
      wa_report-invoicepostingdate =  '00.00.0000'.
    ENDIF.

    " * Calculate Number of Days (e.g., between receipt and invoice posting date)
    wa_report-noofdays = wa_report-receiptdate - wa_report-invoicedate .

    "* Append to Report Table
    APPEND wa_report TO it_report.

  ENDLOOP.

*  cl_demo_output=>display( it_report ).
*  cl_demo_output=>display( it_bsid ).
*  cl_demo_output=>display( it_kna1 ).



  PERFORM alv_fieldcat.

  INCLUDE zts_raidetail_alv_fieldcatf01.

  *----------------------------------------------------------------------*
***INCLUDE ZTS_RAIDETAIL_ALV_FIELDCATF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form alv_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_fieldcat .


  cl_salv_table=>factory( IMPORTING r_salv_table = gr_table
  CHANGING t_table = it_report[] ).
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).
  gr_display = gr_table->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_layout = gr_table->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  gr_display->set_list_header( 'Customer Receipt Against Invoice Detail' ).

  gr_columns = gr_table->get_columns( ).

  "TRY.
  gr_column ?= gr_columns->get_column( 'COMPANYCODE' ).
  gr_column->set_long_text( 'Company_Code' ).
  gr_column->set_medium_text( 'Company_Co' ).
  gr_column->set_short_text( 'Comp_Co' ).
  gr_column->set_output_length( 10 ).


  gr_column ?= gr_columns->get_column( 'PLANT' ).
  gr_column->set_long_text( 'plant' ).
  gr_column->set_medium_text( 'plant' ).
  gr_column->set_short_text( 'PLANT' ).
  gr_column->set_output_length( 6 ).

  gr_column ?= gr_columns->get_column( 'PLANTNAME' ).
  gr_column->set_long_text( 'PLANT_NAME' ).
  gr_column->set_medium_text( 'plant' ).
  gr_column->set_short_text( 'PLANT' ).
  gr_column->set_output_length( 6 ).

  gr_column ?= gr_columns->get_column( 'CUSTOMERCODE' ).
  gr_column->set_long_text( 'CUSTOMER_CODE' ).
  gr_column->set_medium_text( 'CUSTOMER C' ).
  gr_column->set_short_text( 'CUSTOMER' ).
  gr_column->set_output_length( 10 ).

  gr_column ?= gr_columns->get_column( 'CUSTOMERNAME' ).
  gr_column->set_long_text( 'CUSTOMER_NAME' ).
  gr_column->set_medium_text( 'CUSTOMER_NAME' ).
  gr_column->set_short_text( 'CUSTOMER' ).
  gr_column->set_output_length( 18 ).


*
  gr_column ?= gr_columns->get_column( 'RECEIPTDATE' ).   "  receiptdate
  gr_column->set_long_text( 'Receipt_Date' ).
  gr_column->set_medium_text( 'Receipt_Dt' ).
  gr_column->set_short_text( 'Rcpt_Dt' ).
  gr_column->set_output_length( 12 ).
*
  gr_column ?= gr_columns->get_column( 'RECEIPTNO' ).      "receiptno
  gr_column->set_long_text( 'Receipt_No' ).
  gr_column->set_medium_text( 'Receipt_No' ).
  gr_column->set_short_text( 'Rcpt_No' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'RECEIPTAMOUNT' ).     " receiptamount
  gr_column->set_long_text( 'Receipt_Amount' ).
  gr_column->set_medium_text( 'Receipt_Amt' ).
  gr_column->set_short_text( 'Rcpt_Amt' ).
  gr_column->set_output_length( 15 ).

  gr_column ?= gr_columns->get_column( 'SD_INVOICE_NO' ).    "  sd_invoice_no      TYPE vbeln,
  gr_column->set_long_text( 'SD_Invoice_No' ).
  gr_column->set_medium_text( 'SD_Inv_No' ).
  gr_column->set_short_text( 'SD_Inv' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'FI_INVOICE_NO' ).
  gr_column->set_long_text( 'FI_Invoice_No' ).
  gr_column->set_medium_text( 'FI_InvNo' ).
  gr_column->set_short_text( 'FI_Inv' ).
  gr_column->set_output_length( 12 ).
**
  gr_column ?= gr_columns->get_column( 'INVOICEDATE' ).    "acdoca-bldat
  gr_column->set_long_text( 'Invoice Date' ).
  gr_column->set_medium_text( 'Invoice Dt' ).
  gr_column->set_short_text( 'Inv_Dt' ).
  gr_column->set_output_length( 12 ).
*
  gr_column ?= gr_columns->get_column( 'INVOICEPOSTINGDATE' ).
  gr_column->set_long_text( 'Invoice Posting Date' ).
  gr_column->set_medium_text( 'Post Date' ).
  gr_column->set_short_text( 'Post_Dt' ).
  gr_column->set_output_length( 12 ).
*
*  gr_column ?= gr_columns->get_column( 'INVOICEAMOUNT' ).
*  gr_column->set_long_text( 'Invoice Amount' ).
*  gr_column->set_medium_text( 'Invoice Amt' ).
*  gr_column->set_short_text( 'Inv_Amt' ).
*  gr_column->set_output_length( 15 ).
*
*  gr_column ?= gr_columns->get_column( 'PAYMENTADJUSTAMT' ).
*  gr_column->set_long_text( 'Payment Adjustment Amount' ).
*  gr_column->set_medium_text( 'Adjust Amt' ).
*  gr_column->set_short_text( 'Adj_Amt' ).
*  gr_column->set_output_length( 15 ).
*
*  gr_column ?= gr_columns->get_column( 'NOOFDAYS' ).
*  gr_column->set_long_text( 'Number of Days' ).
*  gr_column->set_medium_text( 'No. of Days' ).
*  gr_column->set_short_text( 'Days' ).
*  gr_column->set_output_length( 6 ).



*    CATCH cx_salv_not_found.
**      MESSAGE  'Some fields yet to complete!' TYPE 'E'.
*      WRITE : 'add missing fields'.
  " ENDTRY.

*  companycode        TYPE bukrs,
*         plant              TYPE werks_d,
*         plantname          TYPE name1,
*         customercode       TYPE kunnr,
*         customername       TYPE name1,
*         receiptdate        TYPE char10,
*         receiptno          TYPE belnr,
*         receiptamount      TYPE wrbtr,
*         sd_invoice_no      TYPE vbeln,
*         fi_invoice_no      TYPE belnr,
*         invoicedate        TYPE char10,
*         invoicepostingdate TYPE char10,
*         invoiceamount      TYPE wrbtr,
*         paymentadjustamt   TYPE wrbtr,
*         noofdays           TYPE int4,
*
  gr_table->display( ).




ENDFORM.