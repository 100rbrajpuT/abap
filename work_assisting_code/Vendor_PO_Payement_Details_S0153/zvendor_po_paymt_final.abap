*&---------------------------------------------------------------------*
*& Report ZVENDOR_PO_PAYMT_RPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvendor_po_paymt_rpt.


INCLUDE zvendor_top .

DATA :bukrs TYPE bukrs,
      werks TYPE werks,
      bedat TYPE ebdat,
      ekgrp TYPE ekgrp.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_comp  FOR bukrs ,"OBLIGATORY, " Company Code
                  s_plant FOR werks ,"OBLIGATORY. " Plant
                  s_date FOR bedat OBLIGATORY, " From/To Date Range
                  s_pgrp FOR ekgrp .               " Purchase Group
SELECTION-SCREEN END OF BLOCK blk1.


*         bukrs     TYPE bukrs,           " Company Code
*         ekgrp     TYPE ekgrp,           " Purchasing Group
*         lifnr     TYPE lifnr,           " Vendor
*         ebeln     TYPE ebeln,           " Purchasing Document Number
*         bedat     TYPE dats,            " Document Date
*         rlwrt     TYPE rlwrt,           " Invoice Total Value
START-OF-SELECTION.

  SELECT bukrs ekgrp lifnr  ebeln bedat rlwrt lponr
   FROM ekko
     INTO TABLE it_ekko
   WHERE bukrs IN s_comp       " Filter by Company Code   BUKRS
     AND ekgrp IN s_pgrp       " Filter by Purchase Group
     AND bedat IN s_date.      " Filter by Date Range

  IF it_ekko IS NOT INITIAL.
    SELECT  ekgrp eknam           "Description of purchasing group - name
    FROM t024
    INTO TABLE it_t024
    FOR ALL ENTRIES IN it_ekko
    WHERE ekgrp =  it_ekko-ekgrp.
  ENDIF.


** bukrs TYPE bukrs,           " Company Code
**         belnr TYPE BELNR_d,         "for ledger invoice no
**         gjahr TYPE gjahr,
**         ebeln TYPE ebeln,
**         awkey TYPE awkey,            "starting 10 digit will ne invoice no
**         mwskz TYPE mwskz,

  IF it_ekko IS NOT INITIAL.
    SELECT bukrs ,  belnr ,   gjahr  , ebeln  , awkey ,  mwskz , WRBTR
      FROM bseg
      INTO TABLE @it_bseg
       FOR ALL ENTRIES IN @it_ekko
      WHERE ebeln  = @it_ekko-ebeln
      AND BUKRS  = @it_ekko-BUKRS .

*      AND mwskz <> ''  "IS NOT INITIAL .
*      AND KTOSL = 'WIT' .

  ENDIF.



  IF it_ekko IS NOT INITIAL.
    SELECT  ebeln ,belnr,  ebelp , werks , bewtp  , gjahr    " ebeln -   purchanging num  , werks - plant code ,  BELNR-  invoiceno
    FROM ekbe
    INTO TABLE @it_ekbe
    FOR ALL ENTRIES IN @it_ekko
    WHERE  ebeln =  @it_ekko-ebeln
    AND bewtp IN ('Q', 'T' ).
*     AND  werks(0)+2 = @it_ekko-bukrs .


*         belnr TYPE belnr_d,           " INVOICENO
*         bldat TYPE  bldat,            "BILL  date
*         xblnr TYPE xblnr1,           " BILL_NO
*         gjahr TYPE gjahr,            "INVOICEFISCALYEAR
*         bktxt TYPE bktxt,            " HEADERTEXT
*         cpudt TYPE cpudt,            "ENTERYDATE
*         ppdat TYPE ppdate,                                 "PARKEDDATE
*         RMWWR TYPE RMWWR,     "INVOICE_GROSS_AMOUNT
*         WMWST1 TYPE FWSTEV,   "GSTAMOUNT

    IF it_bseg IS NOT INITIAL.
      SELECT belnr , bldat , xblnr ,  gjahr,  bktxt , cpudt ,   rmwwr ,  wmwst1
      FROM rbkp
      INTO TABLE @it_bkpf
      FOR ALL ENTRIES IN @it_bseg
      WHERE belnr  = @it_bseg-awkey(10)
      AND gjahr  = @it_bseg-gjahr .
    ENDIF.


*TYPES: BEGIN OF ty_ekpo,
*         ebeln TYPE ebeln,           " purchanging num
*         ebelp TYPE  ebelp,           "item no
*         txz01 TYPE txz01,           " short text
*         "    bewtp TYPE bewtp,
*       END OF ty_ekpo.
*    ENDIF.
    IF it_ekbe IS NOT INITIAL.
      SELECT ebeln , ebelp , txz01
      FROM ekpo
      INTO TABLE @it_ekpo
      FOR ALL ENTRIES IN @it_ekbe
      WHERE ebeln  = @it_ekbe-ebeln
      AND ebelp  = @it_ekbe-ebelp.

    ENDIF.

    IF it_ekbe IS NOT INITIAL.
*      SELECT ebeln , ebelp , txz01
*      FROM ekpo
*      INTO TABLE @it_ekpo
*      FOR ALL ENTRIES IN @it_ekbe
*      WHERE ebeln  = @it_ekbe-ebeln
*      AND ebelp  = @it_ekbe-ebelp.
    ENDIF.


*         name1     TYPE name1_gp ,           " vendor name
*         name2     TYPE name2_gp,
*         telf1     TYPE telf1,                 telephone
*          konzs                                 VENDORCATEGORY
*         j_1ipanno TYPE j_1ipanno,          "Permanent Account Number  - vendor PAN
    SELECT lifnr , name1 AS nam , name2 ,konzs, telf1 ,  j_1ipanno , adrnr
    FROM lfa1
    INTO TABLE @it_lfa1
    FOR ALL ENTRIES IN @it_ekko
    WHERE lifnr  =  @it_ekko-lifnr.

*          street     TYPE ad_street,
*         str_suppl1 TYPE ad_strspp1 ,
*         str_suppl2 TYPE AD_STRSPP2,
*         city1      TYPE ad_city1,
*         post_code1 TYPE ad_pstcd1,
    IF it_lfa1 IS NOT INITIAL .
      SELECT addrnumber ,street ,str_suppl1 , str_suppl2 , city1 ,  post_code1
      FROM adrc
      INTO TABLE @it_adrc
      FOR ALL ENTRIES IN  @it_lfa1
      WHERE addrnumber =  @it_lfa1-adrnr.
    ENDIF.

    IF it_lfa1 IS NOT INITIAL.
      SELECT addrnumber , smtp_addr
        FROM adr6
        INTO TABLE @it_adr6
        FOR ALL ENTRIES IN @it_lfa1
        WHERE addrnumber =  @it_lfa1-adrnr.
    ENDIF.


    IF it_lfa1 IS NOT INITIAL.
      SELECT partner  , taxnum
        FROM dfkkbptaxnum
        INTO TABLE @it_DFKKBPTAXNUM
        FOR ALL ENTRIES IN @it_lfa1
        WHERE partner =  @it_lfa1-lifnr
        AND  taxtype ='IN3' .
    ENDIF.
*    TYPES: BEGIN OF ty_DFKKBPTAXNUM,
*         PARTNER TYPE BU_PARTNER,
*         TAXNUM   TYPE BPTAXNUM,
*       END OF ty_DFKKBPTAXNUM.


  ENDIF.

end-OF-SELECTION.



*logic to populate the final table
*  LOOP AT it_ekko INTO wa_ekko.
*    CLEAR wa_final.
*    wa_final-bukrs = wa_ekko-bukrs.
*    wa_final-ekgrp = wa_ekko-ekgrp.
*    wa_final-lifnr = wa_ekko-lifnr.
*    wa_final-ebeln = wa_ekko-ebeln.
*    wa_final-bedat = wa_ekko-bedat.
*    wa_final-rlwrt = wa_ekko-rlwrt.
*    wa_final-lponr = wa_ekko-lponr.
*
*    READ TABLE it_t024 INTO wa_T024 WITH KEY ekgrp = wa_ekko-ekgrp.
*    IF sy-subrc = 0.
*      wa_final-eknam = wa_T024-eknam.
*    ENDIF.
*
*    READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_ekko-ebeln.
*    IF sy-subrc = 0.
*      wa_final-werks = wa_ekbe-werks.
*      IF  wa_ekbe-werks IS NOT INITIAL.
*        SELECT SINGLE name1
*        INTO wa_final-name1
*        FROM t001w
*        WHERE werks = wa_ekbe-werks.
*      ENDIF.
*
*      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln ebelp = wa_ekbe-ebelp.
*      IF sy-subrc = 0.
*        wa_final-txz01 = wa_ekpo-txz01.
*      ENDIF.
*    ENDIF.
*
*    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr  = wa_ekko-lifnr .
*    IF sy-subrc = 0.
*      wa_final-name11 =  wa_lfa1-name11.
*      wa_final-name2 =  wa_lfa1-name2.
*      wa_final-konzs =  wa_lfa1-konzs.                  "group key.
*      wa_final-telf1 =  wa_lfa1-telf1.
*      wa_final-j_1ipanno =  wa_lfa1-j_1ipanno.
*
*      READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber =  wa_lfa1-adrnr .
*      IF sy-subrc = 0.
*        "wa_final-address =
*        CONCATENATE wa_adrc-street wa_adrc-str_suppl1 wa_adrc-str_suppl2 INTO wa_final-address SEPARATED BY ', '.
*        wa_final-city1 =  wa_adrc-city1 .
*        wa_final-post_code1 =  wa_adrc-post_code1.
*      ENDIF.
*
*      READ TABLE it_adr6 INTO wa_adr6 WITH KEY addrnumber =  wa_lfa1-adrnr .
*      IF sy-subrc = 0.
*        wa_final-smtp_addr =  wa_adr6-smtp_addr.
*      ENDIF.
*
*      READ TABLE it_DFKKBPTAXNUM INTO wa_DFKKBPTAXNUM WITH KEY partner =  wa_lfa1-lifnr .
*      IF sy-subrc = 0.
*        wa_final-taxnum =  wa_DFKKBPTAXNUM-taxnum.
*      ENDIF.
*
*
*
*    ENDIF.
*
*
*    LOOP AT it_bseg INTO wa_bseg WHERE ebeln = wa_ekko-ebeln.
*    CLEAR wa_final.
*    wa_final-bukrs = wa_bseg-bukrs.
*    wa_final-belnr = wa_bseg-belnr.
**    wa_final-gjahr = wa_bseg-gjahr.
*    wa_final-awkey = wa_bseg-awkey.
**    wa_final-mwskz = wa_bseg-mwskz.
*
*    " Map other required fields from `EKKO` to the final table
**    wa_final-ekgrp = wa_ekko-ekgrp.
**    wa_final-lifnr = wa_ekko-lifnr.
**    wa_final-ebeln = wa_ekko-ebeln.
**    wa_final-bedat = wa_ekko-bedat.
**    wa_final-rlwrt = wa_ekko-rlwrt.
**    wa_final-lponr = wa_ekko-lponr.
*
*    APPEND wa_final TO it_final.
*  ENDLOOP.
*
*
*
*
*
*    APPEND wa_final TO it_final.
*  ENDLOOP.




  LOOP AT it_ekko INTO wa_ekko.
    CLEAR wa_final.

    " Populate fields from EKKO
    wa_final-bukrs = wa_ekko-bukrs.
    wa_final-ekgrp = wa_ekko-ekgrp.
    wa_final-lifnr = wa_ekko-lifnr.
    wa_final-ebeln = wa_ekko-ebeln.
    wa_final-bedat = wa_ekko-bedat.
    wa_final-rlwrt = wa_ekko-rlwrt.
    wa_final-lponr = wa_ekko-lponr.

    " Populate purchasing group name from T024
    READ TABLE it_t024 INTO wa_T024 WITH KEY ekgrp = wa_ekko-ekgrp.
    IF sy-subrc = 0.
      wa_final-eknam = wa_T024-eknam.
    ENDIF.

    " Populate fields from EKBE
    READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_ekko-ebeln.
    IF sy-subrc = 0.
      wa_final-werks = wa_ekbe-werks.
      IF wa_ekbe-werks IS NOT INITIAL.
        SELECT SINGLE name1
          INTO wa_final-name1
          FROM t001w
          WHERE werks = wa_ekbe-werks.
      ENDIF.

      " Populate item details from EKPO
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln ebelp = wa_ekbe-ebelp.
      IF sy-subrc = 0.
        wa_final-txz01 = wa_ekpo-txz01.
      ENDIF.
    ENDIF.

    " Populate vendor details from LFA1
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekko-lifnr.
    IF sy-subrc = 0.
      wa_final-name11 = wa_lfa1-name11.
      wa_final-name2 = wa_lfa1-name2.
      wa_final-konzs = wa_lfa1-konzs. " Group key
      wa_final-telf1 = wa_lfa1-telf1.
      wa_final-j_1ipanno = wa_lfa1-j_1ipanno.

      " Populate address details from ADRC
      READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber = wa_lfa1-adrnr.
      IF sy-subrc = 0.
        CONCATENATE wa_adrc-street wa_adrc-str_suppl1 wa_adrc-str_suppl2 INTO wa_final-address SEPARATED BY ', '.
        wa_final-city1 = wa_adrc-city1.
        wa_final-post_code1 = wa_adrc-post_code1.
      ENDIF.

      " Populate email address from ADR6
      READ TABLE it_adr6 INTO wa_adr6 WITH KEY addrnumber = wa_lfa1-adrnr.
      IF sy-subrc = 0.
        wa_final-smtp_addr = wa_adr6-smtp_addr.
      ENDIF.

      " Populate tax number from DFKKBPTAXNUM
      READ TABLE it_DFKKBPTAXNUM INTO wa_DFKKBPTAXNUM WITH KEY partner = wa_lfa1-lifnr.
      IF sy-subrc = 0.
        wa_final-taxnum = wa_DFKKBPTAXNUM-taxnum.
      ENDIF.
    ENDIF.

    " Populate fields from BSEG (handling multiple BELNR for single EBELN)
    LOOP AT it_bseg INTO wa_bseg WHERE ebeln = wa_ekko-ebeln.
      wa_final-belnr = wa_bseg-belnr.
      wa_final-awkey = wa_bseg-awkey(10).
      wa_final-WRBTR = wa_bseg-WRBTR.

      " Append the final record for each BELNR entry
      "taking data from rbkp , rbkp data in it_bkpf
      READ TABLE it_bkpf INTO wa_bkpf WITH  KEY belnr =  wa_bseg-awkey(10).
      IF sy-subrc = 0.
        wa_final-bldat =  wa_bkpf-bldat.
        wa_final-xblnr =  wa_bkpf-xblnr.
        wa_final-gjahr =  wa_bkpf-gjahr.
        wa_final-bktxt =  wa_bkpf-bktxt.
        wa_final-cpudt =  wa_bkpf-cpudt.
        wa_final-rmwwr =  wa_bkpf-rmwwr.
        wa_final-wmwst1 =  wa_bkpf-wmwst1.
          wa_final-inv_gross_amt = wa_bkpf-rmwwr - wa_bkpf-wmwst1. " Calculate INV_GROSS_AMT
      ENDIF.


      APPEND wa_final TO it_final.
    ENDLOOP.

  ENDLOOP.

  " Display final results
*  cl_demo_output=>display( it_ekko ).
*  cl_demo_output=>display( it_final ).


  PERFORM alv_fieldcat.

  INCLUDE zven_po_paymt_rpt_alv_fieldf01.
  ***************************************************************************
  *&---------------------------------------------------------------------*
*& Include          ZVENDOR_TOP
*&---------------------------------------------------------------------*



TYPES: BEGIN OF ty_ekko,
         bukrs TYPE bukrs,           " Company Code
         ekgrp TYPE ekgrp,           " Purchasing Group
         lifnr TYPE lifnr,           " Vendor
         ebeln TYPE ebeln,           " Purchasing Document Number
         bedat TYPE dats,            " Document Date
         rlwrt TYPE rlwrt,           " Invoice Total Value
         lponr TYPE lponr,           "last item number
       END OF ty_ekko.

DATA : it_ekko TYPE TABLE OF ty_ekko,
       wa_ekko TYPE ty_ekko.


* ebeln , BELNR , AWKEY , MWSKZ from bseg
TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,           " Company Code
         belnr TYPE BELNR_d,         "for ledger invoice no
         gjahr TYPE gjahr,
         ebeln TYPE ebeln,
         awkey TYPE awkey,            "starting 10 digit will ne invoice no
         mwskz TYPE mwskz,
        WRBTR TYPE WRBTR,
       END OF ty_bseg.

DATA : it_bseg TYPE TABLE OF ty_bseg,
       wa_bseg TYPE ty_bseg.

TYPES: BEGIN OF ty_T024,
         ekgrp TYPE ekgrp,           " Purchasing Group
         eknam TYPE eknam,

       END OF ty_T024.

DATA : it_T024 TYPE TABLE OF ty_T024,
       wa_T024 TYPE ty_T024.

TYPES: BEGIN OF ty_ekbe,
         ebeln TYPE ebeln,           " purchanging num
         belnr TYPE mblnr,
         ebelp TYPE  ebelp,           "item no
         werks TYPE WERKS_d,           " plant code
         bewtp TYPE bewtp,
        gjahr TYPE gjahr,
       END OF ty_ekbe.

DATA : it_ekbe TYPE TABLE OF ty_ekbe,
       wa_ekbe TYPE ty_ekbe.

TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,           " purchanging num
         ebelp TYPE  ebelp,           "item no
         txz01 TYPE txz01,           " short text
         "    bewtp TYPE bewtp,
       END OF ty_ekpo.

DATA : it_ekpo TYPE TABLE OF ty_ekpo,
       wa_ekpo TYPE ty_ekpo.


* belnr , bldat , xblnr ,  gjahr,  bktxt , cpudt ,   RMWWR ,  WMWST1
TYPES: BEGIN OF ty_bkpf,
         belnr TYPE belnr_d,           " INVOICENO
         bldat TYPE  bldat,            "BILL  date
         xblnr TYPE xblnr1,           " BILL_NO
         gjahr TYPE gjahr,            "INVOICEFISCALYEAR
         bktxt TYPE bktxt,            " HEADERTEXT
         cpudt TYPE cpudt,            "ENTERYDATE
       "  ppdat TYPE ppdate,                                 "PARKEDDATE
         RMWWR TYPE RMWWR,     "INVOICE_GROSS_AMOUNT
         WMWST1 TYPE FWSTEV,   "GSTAMOUNT
       END OF ty_bkpf.

DATA : it_bkpf TYPE TABLE OF ty_bkpf,
       wa_bkpf TYPE ty_bkpf.


TYPES: BEGIN OF ty_T001W,
         werks TYPE WERKS_d,           " plant code
         name1 TYPE name1,
       END OF ty_T001W.

DATA : it_T001W TYPE TABLE OF ty_T001W,
       wa_T001W TYPE ty_T001W.


"for networking wbs NSDM_V_MSEG
TYPES: BEGIN OF ty_NSDM_V_MSEG,
         nplnr TYPE nplnr,           " plant code
         "    name1 TYPE name1,
       END OF ty_NSDM_V_MSEG.

DATA : it_network TYPE TABLE OF ty_NSDM_V_MSEG,
       wa_network TYPE ty_NSDM_V_MSEG.


"LFA1  vendor regarding data
TYPES: BEGIN OF ty_lfa1,
         lifnr     TYPE lifnr,
         name11    TYPE name1_gp ,           " vendor name
         name2     TYPE name2_gp,
         konzs     TYPE konzs,                    "group key
         telf1     TYPE telf1,
         j_1ipanno TYPE j_1ipanno,          "Permanent Account Number
         adrnr     TYPE adrnr,
       END OF ty_lfa1.

DATA : it_lfa1 TYPE TABLE OF ty_lfa1,
       wa_lfa1 TYPE ty_lfa1.

TYPES: BEGIN OF ty_ADRC,
         addrnumber TYPE ad_addrnum,
         street     TYPE ad_street,
         str_suppl1 TYPE ad_strspp1,
         str_suppl2 TYPE ad_strspp2,
         city1      TYPE ad_city1,
         post_code1 TYPE ad_pstcd1,

       END OF ty_ADRC.

DATA : it_ADRC TYPE TABLE OF ty_ADRC,
       wa_ADRC TYPE ty_ADRC.


TYPES: BEGIN OF ty_ADR6,
         addrnumber TYPE ad_addrnum,
         smtp_addr  TYPE ad_smtpadr,
       END OF ty_ADR6.

DATA : it_ADR6 TYPE TABLE OF ty_ADR6,
       wa_ADR6 TYPE ty_ADR6.

*DFKKBPTAXNUM
TYPES: BEGIN OF ty_DFKKBPTAXNUM,
         partner TYPE bu_partner,
         taxnum  TYPE bptaxnum,
       END OF ty_DFKKBPTAXNUM.

DATA : it_DFKKBPTAXNUM TYPE TABLE OF ty_DFKKBPTAXNUM,
       wa_DFKKBPTAXNUM TYPE ty_DFKKBPTAXNUM.


TYPES: BEGIN OF ty_final,
         bukrs      TYPE bukrs,           " Company Code
         werks      TYPE WERKS_d,           " plant code
         name1      TYPE name1,          "plant name
         ekgrp      TYPE ekgrp,           " Purchasing Group
         eknam      TYPE eknam,           " Purchasing Group Name
         lifnr      TYPE lifnr,           " Vendor
         ebeln      TYPE ebeln,           " Purchasing Document Number
         bedat      TYPE dats,            " Document Date
         rlwrt      TYPE rlwrt,           " Invoice Total Value
         lponr      TYPE lponr,           " Last Item Number
         ebelp      TYPE  ebelp,           "item no
         "   LIFNR TYPE LIFNR,
         name11     TYPE name1_gp ,           " vendor name
         name2      TYPE name2_gp,
         konzs      TYPE konzs,                    "group key
         telf1      TYPE telf1,
         j_1ipanno  TYPE j_1ipanno,          "Permanent Account Number
         address    TYPE string,
         city1      TYPE ad_city1,
         post_code1 TYPE ad_pstcd1,
         smtp_addr  TYPE ad_smtpadr,
         taxnum     TYPE bptaxnum,
         txz01      TYPE txz01,           " short text
         belnr      TYPE BELNR_d,         "for ledger invoice no   from bseg
         awkey      TYPE awkey,            "starting 10 digit will ne invoice no

         bldat TYPE  bldat,            "BILL  date
         xblnr TYPE xblnr1,           " BILL_NO
          RMWWR TYPE RMWWR,     "INVOICE_GROSS_AMOUNT
         WMWST1 TYPE FWSTEV,   "GSTAMOUNT
          inv_gross_amt   TYPE p DECIMALS 2, " Add INV_GROSS_AMT here
          WRBTR TYPE WRBTR,
         gjahr TYPE gjahr,            "INVOICEFISCALYEAR
         bktxt TYPE bktxt,            " HEADERTEXT
         cpudt TYPE cpudt,            "ENTERYDATE
     "    ppdat TYPE ppdate,                                 "PARKEDDATE



       END OF ty_final.

DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.



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
*****************************************************************************************

*----------------------------------------------------------------------*
***INCLUDE ZVEN_PO_PAYMT_RPT_ALV_FIELDF01.
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
 CHANGING t_table = it_final[] ).
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).
  gr_display = gr_table->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_layout = gr_table->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  gr_display->set_list_header( 'Vendor PO Payment Details' ).

  gr_columns = gr_table->get_columns( ).


  "TRY.
  gr_column ?= gr_columns->get_column( 'BUKRS' ).
  gr_column->set_long_text( 'Company_Code' ).
  gr_column->set_medium_text( 'Company_Co' ).
  gr_column->set_short_text( 'Comp_Co' ).
  gr_column->set_output_length( 8 ).

  gr_column ?= gr_columns->get_column( 'NAME1' ).
  gr_column->set_long_text( 'Plant_Name' ).
  gr_column->set_medium_text( 'Plant_Name' ).
  gr_column->set_short_text( 'Plant_Name' ).
  gr_column->set_output_length( 20 ).

  gr_column ?= gr_columns->get_column( 'EKGRP' ).
  gr_column->set_long_text( 'Purchasing Group' ).
  gr_column->set_medium_text( 'Purchasing Group' ).
  gr_column->set_short_text( 'Purchs_Grp' ).
  gr_column->set_output_length( 15 ).

  gr_column ?= gr_columns->get_column( 'EKNAM' ).
  gr_column->set_long_text( 'Purchasing Group Name' ).
  gr_column->set_medium_text( 'Purchasing Grp Name' ).
  gr_column->set_short_text( 'Purchs Nm' ).
  gr_column->set_output_length( 18 ).

  gr_column ?= gr_columns->get_column( 'LIFNR' ).
  gr_column->set_long_text( 'Vendor' ).
  gr_column->set_medium_text( 'Vendor' ).
  gr_column->set_short_text( 'Vendor' ).
  gr_column->set_output_length( 11 ).




  gr_column ?= gr_columns->get_column( 'EBELN' ).
  gr_column->set_long_text( 'Purchasing Document Number' ).
  gr_column->set_medium_text( 'Purchs Doc Number' ).
  gr_column->set_short_text( 'PO Num' ).
  gr_column->set_output_length( 12 ).


  gr_column ?= gr_columns->get_column( 'BEDAT' ).
  gr_column->set_long_text( 'Document Date' ).
  gr_column->set_medium_text( 'Document Date' ).
  gr_column->set_short_text( 'Doc Date' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'NAME2' ).
  gr_column->set_output_length( 5 ).

*  wa_final-belnr = wa_bseg-belnr.
*  wa_final-awkey = wa_bseg-awkey(10).
  "smtp_addr

  gr_column ?= gr_columns->get_column( 'SMTP_ADDR' ).
  gr_column->set_output_length( 26 ).

  gr_column ?= gr_columns->get_column( 'BELNR' ).
  gr_column->set_long_text( 'LEDGER_INVOICE_NO' ).
  gr_column->set_medium_text( 'LEDGER_INV_NO' ).
  gr_column->set_short_text( 'LED_INV_NO' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'AWKEY' ).
  gr_column->set_long_text( 'INVOICE_NO' ).
  gr_column->set_medium_text( 'INVOICE_NO' ).
  gr_column->set_short_text( 'INV_NO' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'AWKEY' ).
  gr_column->set_long_text( 'INVOICE_NO' ).
  gr_column->set_medium_text( 'INVOICE_NO' ).
  gr_column->set_short_text( 'INV_NO' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'BLDAT' ).
  gr_column->set_long_text( 'Bill Date' ).
  gr_column->set_medium_text( 'Bill Date' ).
  gr_column->set_short_text( 'Bill Date' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'XBLNR' ).
  gr_column->set_long_text( 'Bill Number' ).
  gr_column->set_medium_text( 'Bill No' ).
  gr_column->set_short_text( 'Bill No' ).
  gr_column->set_output_length( 18 ).


  gr_column ?= gr_columns->get_column( 'GJAHR' ).
  gr_column->set_long_text( 'INVOICEFISCALYEAR' ).
  gr_column->set_medium_text( 'INVOICEFISCALYEAR' ).
  gr_column->set_short_text( 'FISCALYEAR' ).
  gr_column->set_output_length( 10 ).

  gr_column ?= gr_columns->get_column( 'CPUDT' ).
  gr_column->set_long_text( 'Entry Date' ).
  gr_column->set_medium_text( 'Entry Date' ).
  gr_column->set_short_text( 'Entry Date' ).
  gr_column->set_output_length( 12 ).

  gr_column ?= gr_columns->get_column( 'BKTXT' ).
  gr_column->set_long_text( 'Header Text' ).
  gr_column->set_medium_text( 'Header Text' ).
  gr_column->set_short_text( 'Header Txt' ).
  gr_column->set_output_length( 15 ).

  "invoice amt
  gr_column ?= gr_columns->get_column( 'RMWWR' ).
  gr_column->set_long_text( 'Invoice Amount' ).
  gr_column->set_medium_text( 'Invoice Amt' ).
  gr_column->set_short_text( 'Inv AMT' ).
  gr_column->set_output_length( 15 ).

  "inv_gross_amt
  gr_column ?= gr_columns->get_column( 'WMWST1' ).
  gr_column->set_long_text( 'GST Amount' ).
  gr_column->set_medium_text( 'GST Amount' ).
  gr_column->set_short_text( 'GST AMT' ).
  gr_column->set_output_length( 15 ).

  "inv_gross_amt
  gr_column ?= gr_columns->get_column( 'INV_GROSS_AMT' ).
  gr_column->set_long_text( 'Invoice Gross Amount' ).
  gr_column->set_medium_text( 'Invoice Gross Amt' ).
  gr_column->set_short_text( 'I G AMT' ).
  gr_column->set_output_length( 15 ).


  gr_column ?= gr_columns->get_column( 'WRBTR' ).
  gr_column->set_long_text( 'Invoice Gross Amount2' ).
  gr_column->set_medium_text( 'Invoice Gross Amt2' ).
  gr_column->set_short_text( 'I G AMT222' ).
  gr_column->set_output_length( 15 ).



  gr_table->display( ).


ENDFORM.