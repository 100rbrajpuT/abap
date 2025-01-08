*&---------------------------------------------------------------------*
*& Report ZVENDOR_PO_PAYMENT_RPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZVENDOR_PO_PAYMENT_RPT_CP.

INCLUDE ZVENDOR_PO_TOP_SS.
*INCLUDE zvendor_po_top .

INCLUDE ZVENDOR_PO_SS_SS.
*INCLUDE zvendor_po_ss .

INCLUDE ZVENDOR_PO_SUB_SS.
*include zvendor_po_sub .


 IF it_final[]  IS NOT INITIAL.
 PERFORM zvendor_alv_fieldcat.

 ELSE.
   MESSAGE : 'Data not Found' TYPE 'E' .
 ENDIF.

INCLUDE ZVENR_PO_PMT_RPT_ZVENF01_SS.
*INCLUDE zvendor_po_payment_rpt_zvenf01.




*******************************************



*&---------------------------------------------------------------------*
*& Include          ZVENDOR_PO_TOP
*&---------------------------------------------------------------------*


"for selection screen
DATA :bukrs TYPE bukrs,
      werks TYPE werks,
      bedat TYPE ebdat,
      ekgrp TYPE ekgrp,
      lv_ebeln TYPE ebeln.

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
         wrbtr TYPE wrbtr,
         KTOSL TYPE KTOSL,
       END OF ty_bseg.

DATA : it_bseg TYPE TABLE OF ty_bseg,
       wa_bseg TYPE ty_bseg.


DATA : it_bsegf TYPE TABLE OF ty_bseg,
       wa_bsegf TYPE ty_bseg.

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
         belnr  TYPE belnr_d,           " INVOICENO
         bldat  TYPE  bldat,            "BILL  date
         xblnr  TYPE xblnr1,           " BILL_NO
         gjahr  TYPE gjahr,            "INVOICEFISCALYEAR
         bktxt  TYPE bktxt,            " HEADERTEXT
         cpudt  TYPE cpudt,            "ENTERYDATE
         "  ppdat TYPE ppdate,                                 "PARKEDDATE
         rmwwr  TYPE rmwwr,     "INVOICE_GROSS_AMOUNT
         wmwst1 TYPE fwstev,   "GSTAMOUNT
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
         bukrs         TYPE bukrs,           " Company Code
         werks         TYPE WERKS_d,           " plant code
         name1         TYPE name1,          "plant name
         ekgrp         TYPE ekgrp,           " Purchasing Group
         eknam         TYPE eknam,           " Purchasing Group Name
         lifnr         TYPE lifnr,           " Vendor
         ebeln         TYPE ebeln,           " Purchasing Document Number
         bedat         TYPE dats,            " Document Date
         rlwrt         TYPE rlwrt,           " Invoice Total Value
         lponr         TYPE lponr,           " Last Item Number
         ebelp         TYPE  ebelp,           "item no
         "   LIFNR TYPE LIFNR,
         name11        TYPE name1_gp ,           " vendor name
         name2         TYPE name2_gp,
         konzs         TYPE konzs,                    "group key
         telf1         TYPE telf1,
         j_1ipanno     TYPE j_1ipanno,          "Permanent Account Number
         address       TYPE string,
         city1         TYPE ad_city1,
         post_code1    TYPE ad_pstcd1,
         smtp_addr     TYPE ad_smtpadr,
         taxnum        TYPE bptaxnum,
         txz01         TYPE txz01,           " short text
         belnr         TYPE BELNR_d,         "for ledger invoice no   from bseg
         awkey         TYPE awkey,            "starting 10 digit will ne invoice no

         bldat         TYPE  bldat,            "BILL  date
         xblnr         TYPE xblnr1,           " BILL_NO
         rmwwr         TYPE rmwwr,     "INVOICE_GROSS_AMOUNT
         wmwst1        TYPE fwstev,   "GSTAMOUNT
         tds_amont        TYPE wrbtr,   "tds abount
         inv_gross_amt TYPE p DECIMALS 2, " Add INV_GROSS_AMT here
         wrbtr         TYPE wrbtr,
         gjahr         TYPE gjahr,            "INVOICEFISCALYEAR
         bktxt         TYPE bktxt,            " HEADERTEXT
         cpudt         TYPE cpudt,            "ENTERYDATE
         "    ppdat TYPE ppdate,                                 "PARKEDDATE
       END OF ty_final.

DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.



DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      w_layout    TYPE slis_layout_alv.
w_layout-zebra = 'X'.


DATA: ispfli TYPE TABLE OF spfli.
DATA: gr_table TYPE REF TO cl_salv_table.
DATA: gr_functions TYPE REF TO cl_salv_functions.
DATA: gr_display TYPE REF TO cl_salv_display_settings.
DATA: gr_columns TYPE REF TO cl_salv_columns_table.
DATA: gr_column TYPE REF TO cl_salv_column_table.
DATA: gr_layout TYPE REF TO cl_salv_layout.
DATA: key TYPE salv_s_layout_key.
DATA: color TYPE lvc_s_colo.






*&---------------------------------------------------------------------*
*& Include          ZVENDOR_PO_SS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_comp  FOR bukrs ,"OBLIGATORY, " Company Code
                  s_plant FOR werks ,"OBLIGATORY. " Plant
                  s_date FOR bedat OBLIGATORY, " From/To Date Range
                  s_pgrp FOR ekgrp ,               " Purchase Group
                   s_ebeln for  lv_ebeln.


SELECTION-SCREEN END OF BLOCK blk1.





*&---------------------------------------------------------------------*
*& Include          ZVENDOR_PO_SUB
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  SELECT BUKRS EKGRP LIFNR  EBELN BEDAT RLWRT LPONR
   FROM EKKO
     INTO TABLE IT_EKKO
   WHERE BUKRS IN S_COMP       " Filter by Company Code   BUKRS
     AND EKGRP IN S_PGRP       " Filter by Purchase Group
     AND BEDAT IN S_DATE      " Filter by Date Range
     AND ebeln IN s_ebeln.     " Filter by Ebeln


*DELETE ADJACENT DUPLICATES FROM it_ekko COMPARING ebeln bukrs.  "ss :06-01-2024
*DELETE it_ekko WHERE ebeln IS INITIAL OR bukrs IS INITIAL.

  IF IT_EKKO IS NOT INITIAL.
    SELECT  EKGRP EKNAM           "Description of purchasing group - name
    FROM T024
    INTO TABLE IT_T024
    FOR ALL ENTRIES IN IT_EKKO
    WHERE EKGRP =  IT_EKKO-EKGRP.
  ENDIF.


** bukrs TYPE bukrs,           " Company Code
**         belnr TYPE BELNR_d,         "for ledger invoice no
**         gjahr TYPE gjahr,
**         ebeln TYPE ebeln,
**         awkey TYPE awkey,            "starting 10 digit will ne invoice no
**         mwskz TYPE mwskz,

  IF IT_EKKO IS NOT INITIAL.

*      SELECT bukrs ,  belnr ,   gjahr  , ebeln  , awkey ,  mwskz , wrbtr
*      FROM bseg
*      INTO TABLE @it_bseg
*       FOR ALL ENTRIES IN @it_ekko
*      WHERE ebeln  = @it_ekko-ebeln
*      AND bukrs  = @it_ekko-bukrs .

*      AND mwskz <> ''  "IS NOT INITIAL .
*      AND KTOSL = 'WIT' .
    "06-01-20
    LOOP AT IT_EKKO INTO WA_EKKO.
      SELECT BUKRS, BELNR, GJAHR, EBELN, AWKEY, MWSKZ, WRBTR , KTOSL
        FROM BSEG
        INTO TABLE @IT_BSEG

        WHERE EBELN = @WA_EKKO-EBELN
          AND BUKRS = @WA_EKKO-BUKRS.
      APPEND LINES OF IT_BSEG TO IT_BSEGF.
      CLEAR IT_BSEG.
    ENDLOOP.

  ENDIF.



  IF IT_EKKO IS NOT INITIAL.
    SELECT  EBELN ,BELNR,  EBELP , WERKS , BEWTP  , GJAHR    " ebeln -   purchanging num  , werks - plant code ,  BELNR-  invoiceno
    FROM EKBE
    INTO TABLE @IT_EKBE
    FOR ALL ENTRIES IN @IT_EKKO
    WHERE  EBELN =  @IT_EKKO-EBELN
    AND BEWTP IN ('Q', 'T' ).
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

    IF IT_BSEGF IS NOT INITIAL.
      SELECT BELNR , BLDAT , XBLNR ,  GJAHR,  BKTXT , CPUDT ,   RMWWR ,  WMWST1
      FROM RBKP
      INTO TABLE @IT_BKPF
      FOR ALL ENTRIES IN @IT_BSEGF
      WHERE BELNR  = @IT_BSEGF-AWKEY(10)
      AND GJAHR  = @IT_BSEGF-GJAHR .
    ENDIF.


*TYPES: BEGIN OF ty_ekpo,
*         ebeln TYPE ebeln,           " purchanging num
*         ebelp TYPE  ebelp,           "item no
*         txz01 TYPE txz01,           " short text
*         "    bewtp TYPE bewtp,
*       END OF ty_ekpo.
*    ENDIF.
    IF IT_EKBE IS NOT INITIAL.
      SELECT EBELN , EBELP , TXZ01
      FROM EKPO
      INTO TABLE @IT_EKPO
      FOR ALL ENTRIES IN @IT_EKBE
      WHERE EBELN  = @IT_EKBE-EBELN
      AND EBELP  = @IT_EKBE-EBELP.

    ENDIF.

    IF IT_EKBE IS NOT INITIAL.
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
    SELECT LIFNR , NAME1 AS NAM , NAME2 ,KONZS, TELF1 ,  J_1IPANNO , ADRNR
    FROM LFA1
    INTO TABLE @IT_LFA1
    FOR ALL ENTRIES IN @IT_EKKO
    WHERE LIFNR  =  @IT_EKKO-LIFNR.

*          street     TYPE ad_street,
*         str_suppl1 TYPE ad_strspp1 ,
*         str_suppl2 TYPE AD_STRSPP2,
*         city1      TYPE ad_city1,
*         post_code1 TYPE ad_pstcd1,
    IF IT_LFA1 IS NOT INITIAL .
      SELECT ADDRNUMBER ,STREET ,STR_SUPPL1 , STR_SUPPL2 , CITY1 ,  POST_CODE1
      FROM ADRC
      INTO TABLE @IT_ADRC
      FOR ALL ENTRIES IN  @IT_LFA1
      WHERE ADDRNUMBER =  @IT_LFA1-ADRNR.
    ENDIF.

    IF IT_LFA1 IS NOT INITIAL.
      SELECT ADDRNUMBER , SMTP_ADDR
        FROM ADR6
        INTO TABLE @IT_ADR6
        FOR ALL ENTRIES IN @IT_LFA1
        WHERE ADDRNUMBER =  @IT_LFA1-ADRNR.
    ENDIF.


    IF IT_LFA1 IS NOT INITIAL.
      SELECT PARTNER  , TAXNUM
        FROM DFKKBPTAXNUM
        INTO TABLE @it_DFKKBPTAXNUM
        FOR ALL ENTRIES IN @IT_LFA1
        WHERE PARTNER =  @IT_LFA1-LIFNR
        AND  TAXTYPE ='IN3' .
    ENDIF.
*    TYPES: BEGIN OF ty_DFKKBPTAXNUM,
*         PARTNER TYPE BU_PARTNER,
*         TAXNUM   TYPE BPTAXNUM,
*       END OF ty_DFKKBPTAXNUM.


  ENDIF.

end-OF-SELECTION.


  DATA: LT_PROCESSED_BELNR TYPE TABLE OF BSEG-BELNR WITH EMPTY KEY.
*DATA: lt_tds_amounts TYPE TABLE OF bseg WITH EMPTY KEY, " Hash table for BELNR and TDS Amount
*      wa_tds_amounts TYPE bseg.


  LOOP AT IT_EKKO INTO WA_EKKO.
    CLEAR WA_FINAL.

    " Populate fields from EKKO
    WA_FINAL-BUKRS = WA_EKKO-BUKRS.
    WA_FINAL-EKGRP = WA_EKKO-EKGRP.
    WA_FINAL-LIFNR = WA_EKKO-LIFNR.
    WA_FINAL-EBELN = WA_EKKO-EBELN.
    WA_FINAL-BEDAT = WA_EKKO-BEDAT.
    WA_FINAL-RLWRT = WA_EKKO-RLWRT.
    WA_FINAL-LPONR = WA_EKKO-LPONR.

    " Populate purchasing group name from T024
    READ TABLE IT_T024 INTO wa_T024 WITH KEY EKGRP = WA_EKKO-EKGRP.
    IF SY-SUBRC = 0.
      WA_FINAL-EKNAM = wa_T024-EKNAM.
    ENDIF.

    " Populate fields from EKBE
    READ TABLE IT_EKBE INTO WA_EKBE WITH KEY EBELN = WA_EKKO-EBELN.
    IF SY-SUBRC = 0.
      WA_FINAL-WERKS = WA_EKBE-WERKS.
      IF WA_EKBE-WERKS IS NOT INITIAL.
        SELECT SINGLE NAME1
          INTO WA_FINAL-NAME1
          FROM T001W
          WHERE WERKS = WA_EKBE-WERKS.
      ENDIF.

      " Populate item details from EKPO
      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_EKBE-EBELN EBELP = WA_EKBE-EBELP.
      IF SY-SUBRC = 0.
        WA_FINAL-TXZ01 = WA_EKPO-TXZ01.
      ENDIF.
    ENDIF.




    " Populate vendor details from LFA1
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR.
    IF SY-SUBRC = 0.
      WA_FINAL-NAME11 = WA_LFA1-NAME11.
      WA_FINAL-NAME2 = WA_LFA1-NAME2.
      WA_FINAL-KONZS = WA_LFA1-KONZS. " Group key
      WA_FINAL-TELF1 = WA_LFA1-TELF1.
      WA_FINAL-J_1IPANNO = WA_LFA1-J_1IPANNO.

      " Populate address details from ADRC
      READ TABLE IT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
      IF SY-SUBRC = 0.
        CONCATENATE WA_ADRC-STREET WA_ADRC-STR_SUPPL1 WA_ADRC-STR_SUPPL2 INTO WA_FINAL-ADDRESS SEPARATED BY ', '.
        WA_FINAL-CITY1 = WA_ADRC-CITY1.
        WA_FINAL-POST_CODE1 = WA_ADRC-POST_CODE1.
      ENDIF.

      " Populate email address from ADR6
      READ TABLE IT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
      IF SY-SUBRC = 0.
        WA_FINAL-SMTP_ADDR = WA_ADR6-SMTP_ADDR.
      ENDIF.

      " Populate tax number from DFKKBPTAXNUM
      READ TABLE it_DFKKBPTAXNUM INTO wa_DFKKBPTAXNUM WITH KEY PARTNER = WA_LFA1-LIFNR.
      IF SY-SUBRC = 0.
        WA_FINAL-TAXNUM = wa_DFKKBPTAXNUM-TAXNUM.
      ENDIF.
    ENDIF.

    " Populate fields from BSEG (handling multiple BELNR for single EBELN)
    LOOP AT IT_BSEGF INTO WA_BSEGF WHERE EBELN = WA_EKKO-EBELN.


      WA_FINAL-BELNR = WA_BSEGF-BELNR.
      WA_FINAL-AWKEY = WA_BSEGF-AWKEY(10).
      WA_FINAL-WRBTR = WA_BSEGF-WRBTR.

      " Fetch TDS Amount for each BELNR (with KTSOL = 'WIT')
      READ TABLE LT_PROCESSED_BELNR WITH KEY TABLE_LINE = WA_BSEGF-BELNR TRANSPORTING NO FIELDS.
      IF SY-SUBRC <> 0. " BELNR not yet processed
        SELECT SINGLE WRBTR
          INTO WA_FINAL-TDS_AMONT
          FROM BSEG
          WHERE BELNR = WA_BSEGF-BELNR
            AND BUKRS = WA_EKKO-BUKRS
            AND KTOSL = 'WIT'.

          SELECT SINGLE WRBTR
          INTO WA_FINAL-RMWWR
          FROM BSEG
          WHERE BELNR = WA_BSEGF-BELNR
            AND BUKRS = WA_EKKO-BUKRS
            AND KTOSL = 'KBS'.


        APPEND WA_BSEGF-BELNR TO LT_PROCESSED_BELNR. " Mark BELNR as processed
        ELSE.
          WA_FINAL-TDS_AMONT = '00.0'.
          WA_FINAL-RMWWR = '00.0'.
      ENDIF.




      " Append the final record for each BELNR entry
      "taking data from rbkp , rbkp data in it_bkpf
      READ TABLE IT_BKPF INTO WA_BKPF WITH  KEY BELNR =  WA_BSEGF-AWKEY(10).
      IF SY-SUBRC = 0.
        WA_FINAL-BLDAT =  WA_BKPF-BLDAT.
        WA_FINAL-XBLNR =  WA_BKPF-XBLNR.
        WA_FINAL-GJAHR =  WA_BKPF-GJAHR.
        WA_FINAL-BKTXT =  WA_BKPF-BKTXT.
        WA_FINAL-CPUDT =  WA_BKPF-CPUDT.
      "  WA_FINAL-RMWWR =  WA_BKPF-RMWWR.
        WA_FINAL-WMWST1 =  WA_BKPF-WMWST1.
*        wa_final-inv_gross_amt = wa_bkpf-rmwwr - wa_bkpf-wmwst1. " Calculate INV_GROSS_AMT
        WA_FINAL-INV_GROSS_AMT =  '00000'. "wa_bkpf-rmwwr - wa_bkpf-wmwst1. " Calculate INV_GROSS_AMT
      ENDIF.


      APPEND WA_FINAL TO IT_FINAL.
    ENDLOOP.

  ENDLOOP.

  " Display final results
*  cl_demo_output=>display( it_ekko ).
*  cl_demo_output=>display( it_final ).