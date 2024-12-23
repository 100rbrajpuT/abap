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



  IF it_ekko IS NOT INITIAL.
    SELECT  ebeln , ebelp , werks , bewtp     " ebeln -   purchanging num  , werks - plant code
    FROM ekbe
    INTO TABLE @it_ekbe
    FOR ALL ENTRIES IN @it_ekko
    WHERE  ebeln =  @it_ekko-ebeln
    AND bewtp IN ('Q', 'T' ).
*     AND  werks(0)+2 = @it_ekko-bukrs .

*    SELECT ekeb~ebeln, ekeb~werks, ekeb~bewtp
*  FROM ekbe AS ekbe
*  INNER JOIN ekko AS ekko ON ekbe~ebeln = ekko~ebeln
* INTO TABLE @DATA(it_ekbe)
* WHERE ekbe~bewtp IN ('Q', 'T')
*   AND ekko~bukrs = substring( ekbe~werks, 1, 2 ).

*TYPES: BEGIN OF ty_ekpo,
*         ebeln TYPE ebeln,           " purchanging num
*         ebelp TYPE  ebelp,           "item no
*         txz01 TYPE txz01,           " short text
*         "    bewtp TYPE bewtp,
*       END OF ty_ekpo.
    IF it_ekbe IS NOT INITIAL.
      SELECT ebeln , ebelp , txz01
      FROM ekpo
      INTO TABLE @it_ekpo
      FOR ALL ENTRIES IN @it_ekbe
      WHERE ebeln  = @it_ekbe-ebeln
      AND ebelp  = @it_ekbe-ebelp.

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
  LOOP AT it_ekko INTO wa_ekko.
    CLEAR wa_final.
    wa_final-bukrs = wa_ekko-bukrs.
    wa_final-ekgrp = wa_ekko-ekgrp.
    wa_final-lifnr = wa_ekko-lifnr.
    wa_final-ebeln = wa_ekko-ebeln.
    wa_final-bedat = wa_ekko-bedat.
    wa_final-rlwrt = wa_ekko-rlwrt.
    wa_final-lponr = wa_ekko-lponr.

    READ TABLE it_t024 INTO wa_T024 WITH KEY ekgrp = wa_ekko-ekgrp.
    IF sy-subrc = 0.
      wa_final-eknam = wa_T024-eknam.
    ENDIF.

    READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_ekko-ebeln.
    IF sy-subrc = 0.
      wa_final-werks = wa_ekbe-werks.
      IF  wa_ekbe-werks IS NOT INITIAL.
        SELECT SINGLE name1
        INTO wa_final-name1
        FROM t001w
        WHERE werks = wa_ekbe-werks.
      ENDIF.

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln ebelp = wa_ekbe-ebelp.
      IF sy-subrc = 0.
        wa_final-txz01 = wa_ekpo-txz01.
      ENDIF.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr  = wa_ekko-lifnr .
    IF sy-subrc = 0.
      wa_final-name11 =  wa_lfa1-name11.
      wa_final-name2 =  wa_lfa1-name2.
      wa_final-konzs =  wa_lfa1-konzs.                  "group key.
      wa_final-telf1 =  wa_lfa1-telf1.
      wa_final-j_1ipanno =  wa_lfa1-j_1ipanno.

      READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber =  wa_lfa1-adrnr .
      IF sy-subrc = 0.
        "wa_final-address =
        CONCATENATE wa_adrc-street wa_adrc-str_suppl1 wa_adrc-str_suppl2 INTO wa_final-address SEPARATED BY ', '.
        wa_final-city1 =  wa_adrc-city1 .
        wa_final-post_code1 =  wa_adrc-post_code1.
      ENDIF.

      READ TABLE it_adr6 INTO wa_adr6 WITH KEY addrnumber =  wa_lfa1-adrnr .
      IF sy-subrc = 0.
        wa_final-smtp_addr =  wa_adr6-smtp_addr.
      ENDIF.

      READ TABLE it_DFKKBPTAXNUM INTO wa_DFKKBPTAXNUM WITH KEY partner =  wa_lfa1-lifnr .
      IF sy-subrc = 0.
        wa_final-taxnum =  wa_DFKKBPTAXNUM-taxnum.
      ENDIF.



    ENDIF.





    APPEND wa_final TO it_final.
  ENDLOOP.

  cl_demo_output=>display( it_ekko ).
  cl_demo_output=>display( it_final ).


  *****************************************************************************

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

TYPES: BEGIN OF ty_T024,
         ekgrp TYPE ekgrp,           " Purchasing Group
         eknam TYPE eknam,

       END OF ty_T024.

DATA : it_T024 TYPE TABLE OF ty_T024,
       wa_T024 TYPE ty_T024.

TYPES: BEGIN OF ty_ekbe,
         ebeln TYPE ebeln,           " purchanging num
         ebelp TYPE  ebelp,           "item no
         werks TYPE WERKS_d,           " plant code
         bewtp TYPE bewtp,
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


       END OF ty_final.

DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.