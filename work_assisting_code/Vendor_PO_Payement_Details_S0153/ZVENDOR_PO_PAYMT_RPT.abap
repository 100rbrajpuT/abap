*&---------------------------------------------------------------------*
*& Report ZVENDOR_PO_PAYMT_RPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvendor_po_paymt_rpt.


INCLUDE zvendor_top .   "take this code from last in report (below)

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
    SELECT  ebeln werks bewtp     " ebeln -   purchanging num  , werks - plant code
    FROM ekbe
    INTO TABLE it_ekbe
    FOR ALL ENTRIES IN it_ekko
    WHERE ebeln =  it_ekko-ebeln
      AND bewtp IN ('Q', 'T' ).

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
    ENDIF.





    APPEND wa_final TO it_final.
  ENDLOOP.

  cl_demo_output=>display( it_ekko ).
  cl_demo_output=>display( it_final ).











**********************************************************************************************************
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
         EKNAM TYPE EKNAM,

       END OF ty_T024.

DATA : it_T024 TYPE TABLE OF ty_T024,
       wa_T024 TYPE ty_T024.

TYPES: BEGIN OF ty_ekbe,
         EBELN TYPE EBELN,           " purchanging num
         WERKS TYPE WERKS_d,           " plant code
         BEWTP TYPE BEWTP,
       END OF ty_ekbe.

DATA : it_ekbe TYPE TABLE OF ty_ekbe,
       wa_ekbe TYPE ty_ekbe.


TYPES: BEGIN OF ty_T001W,
         WERKS TYPE WERKS_d,           " plant code
        NAME1 TYPE NAME1,
       END OF ty_T001W.

DATA : it_T001W TYPE TABLE OF ty_T001W,
       wa_T001W TYPE ty_T001W.



TYPES: BEGIN OF ty_final,
         bukrs TYPE bukrs,           " Company Code
         WERKS TYPE WERKS_d,           " plant code
         NAME1 TYPE NAME1,          "plant name
         ekgrp TYPE ekgrp,           " Purchasing Group
         eknam TYPE eknam,           " Purchasing Group Name
         lifnr TYPE lifnr,           " Vendor
         ebeln TYPE ebeln,           " Purchasing Document Number
         bedat TYPE dats,            " Document Date
         rlwrt TYPE rlwrt,           " Invoice Total Value
         lponr TYPE lponr,           " Last Item Number

       END OF ty_final.

DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.