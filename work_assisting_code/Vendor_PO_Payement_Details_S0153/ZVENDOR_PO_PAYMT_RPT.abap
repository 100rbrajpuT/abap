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
