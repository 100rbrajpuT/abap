*&---------------------------------------------------------------------*
*& Report Z0607
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z0607.


TABLES: t001w, mara, mseg, mbew, mkpf, makt, bsim.

TYPES: BEGIN OF ty_final,
*       matnr TYPE mara-matnr,
*       maktx TYPE makt-maktx,
*       lgort TYPE mseg-lgort,
*       werks  TYPE mseg-werks,
*       budat TYPE bsim-budat,
*       anfmenge(09)  TYPE p DECIMALS 3,     "opening stock
*       anfmeins  TYPE mara-meins,
*       recqty(09)   TYPE p DECIMALS 3,
*       issqty(09)   TYPE p DECIMALS 3,
*       endmenge(09)  TYPE p DECIMALS 3,      "closing stock
*       endmeins   TYPE mara-meins,

        matnr TYPE bsim-matnr,
        bwkey TYPE bsim-bwkey,
        blart TYPE bsim-blart,
        budat TYPE bsim-budat,
        anfmenge(09)  TYPE p DECIMALS 3,
        anfmeins TYPE bsim-meins,
        lv_receipts TYPE  bsim-menge,
        lv_issues   TYPE  bsim-menge,
        endmenge(09)  TYPE p DECIMALS 3,
        endmeins TYPE bsim-meins,
       END OF ty_final.


TYPES: BEGIN OF ty_makt,
       matnr TYPE makt-matnr,
       maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_mkpf,
       mblnr TYPE mkpf-mblnr,
       blart TYPE mkpf-blart,
       budat TYPE mkpf-budat,
       END OF ty_mkpf.

TYPES: BEGIN OF ty_bsim,
       matnr TYPE bsim-matnr,
       bwkey TYPE bsim-bwkey,
       blart TYPE bsim-blart,
       meins TYPE bsim-meins,
       budat TYPE bsim-budat,
       menge TYPE bsim-menge,

       END OF ty_bsim.

TYPES: BEGIN OF ty_bsim1,
        matnr TYPE bsim-matnr,
        bwkey TYPE bsim-bwkey,
        blart TYPE bsim-blart,
        budat TYPE bsim-budat,
        anfmenge(09)  TYPE p DECIMALS 3,
        anfmeins TYPE bsim-meins,
        lv_receipts TYPE  bsim-menge,
        lv_issues   TYPE  bsim-menge,
        endmenge(09)  TYPE p DECIMALS 3,
        endmeins TYPE bsim-meins,
       END OF ty_bsim1.

DATA: it_final TYPE STANDARD TABLE OF ty_final,
      wa_final TYPE ty_final.
DATA: lt_bsim   TYPE STANDARD TABLE OF ty_bsim,
      ls_bsim  TYPE ty_bsim,
      lt_bsim1 TYPE STANDARD TABLE OF ty_bsim1,
      ls_bsim1 TYPE ty_bsim1,
      it_mseg   TYPE STANDARD TABLE OF mseg,
      wa_mseg  TYPE mseg.
DATA: it_makt   TYPE STANDARD TABLE OF ty_makt,
      wa_makt  TYPE ty_makt,
      it_mkpf   TYPE STANDARD TABLE OF ty_mkpf,
      wa_mkpf  TYPE ty_mkpf,
      it_mbew TYPE STANDARD TABLE OF mbew,
      wa_mbew TYPE mbew.




SELECTION-SCREEN: BEGIN OF BLOCK R1 WITH FRAME TITLE text-001.

  SELECT-OPTIONS: s_plant FOR t001w-werks NO INTERVALS no-EXTENSION,
                  s_mat   FOR mara-matnr ,
                  s_pdate FOR bsim-budat.

SELECTION-SCREEN: END OF BLOCK R1.


START-OF-SELECTION.

SELECT matnr maktx FROM makt
INTO TABLE it_makt
WHERE matnr IN s_mat.

SELECT matnr bwkey blart meins budat menge FROM bsim INTO TABLE lt_bsim

WHERE matnr IN s_mat AND
      bwkey IN s_plant AND
      budat BETWEEN s_pdate-low AND sy-datum.

*DELETE lt_bsim WHERE blart NE 'WA' .
*DELETE  lt_bsim WHERE blart NE 'WE' .

SELECT * FROM mseg INTO TABLE it_mseg

WHERE matnr      IN s_mat AND
     werks     IN s_plant AND
     budat_mkpf BETWEEN s_pdate-low AND sy-datum.

SELECT mblnr blart budat FROM mkpf
INTO TABLE it_mkpf FOR ALL ENTRIES IN it_mseg
WHERE mblnr = it_mseg-mblnr.


SORT lt_bsim by budat DESCENDING.



PERFORM stock_cal.
LOOP AT lt_bsim1 into ls_bsim1 .
IF ls_bsim-budat LE s_pdate-high .

APPEND ls_bsim1 to it_final.

ENDIF.
ENDLOOP.


DATA : lo_alv TYPE REF TO cl_salv_table.
TRY.
    CALL METHOD cl_salv_table=>factory
*      EXPORTING
*        list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*        r_container    =
*        container_name =
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = it_final.
    lo_alv->display( ).
*      CATCH cx_salv_msg.
  CATCH cx_salv_msg INTO DATA(lx_msg).
    MESSAGE lx_msg->get_text( ) TYPE 'S'.
ENDTRY.

*&---------------------------------------------------------------------*
*& Form stock_cal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM stock_cal .

DATA: lv_receipts TYPE  bsim-menge,
      lv_issues   TYPE  bsim-menge,
      lv_clos_st TYPE mbew-lbkum,
      lv_open_st TYPE mbew-lbkum,
      lv_total_receipts TYPE bsim-menge,
      lv_total_issues TYPE bsim-menge.


SELECT SINGLE lbkum INTO lv_clos_st FROM mbew
  WHERE matnr IN s_mat
  AND bwkey IN s_plant.

  IF lv_clos_st IS NOT INITIAL.

  LOOP AT lt_bsim INTO ls_bsim
      WHERE budat <= sy-datum AND budat >= s_pdate-low.


      AT END OF budat.
        IF ls_bsim-blart = 'WE'.  "Receipts
          SUM.
         lv_receipts =  ls_bsim-menge.
        ELSEIF ls_bsim-blart = 'WA'. "issues
         SUM.
        lv_issues =  ls_bsim-menge.

        ENDIF.
      ENDAT.

IF lv_receipts NE '' or lv_issues NE ''.

  ls_bsim1-matnr = ls_bsim-matnr.
  ls_bsim1-bwkey = ls_bsim-bwkey.
  ls_bsim1-blart = ls_bsim-blart.
  ls_bsim1-budat = ls_bsim-budat.
  ls_bsim1-anfmenge = lv_open_st.
  ls_bsim1-anfmeins = ls_bsim-meins.
  ls_bsim1-lv_receipts = lv_receipts.
  ls_bsim1-lv_issues = lv_issues.
  ls_bsim1-endmeins = ls_bsim-meins.



  APPEND ls_bsim1 to lt_bsim1.
  CLEAR  :ls_bsim1 , lv_receipts, lv_issues .

ENDIF.


ENDLOOP.

IF sy-subrc = 0.
  SORT lt_bsim1 by budat DESCENDING.

READ TABLE lt_bsim1 INTO ls_bsim1 INDEX 1.
IF sy-subrc = 0.
  ls_bsim1-endmenge = lv_clos_st.
  MODIFY lt_bsim1 FROM ls_bsim1 INDEX 1.
ENDIF.

READ TABLE lt_bsim1 INTO ls_bsim1 INDEX 1.
IF sy-subrc = 0.
  ls_bsim1-anfmenge = ls_bsim1-endmenge - ls_bsim1-lv_receipts + ls_bsim1-lv_issues.
  MODIFY lt_bsim1 FROM ls_bsim1 INDEX 1.
ENDIF.

ENDIF.

LOOP AT lt_bsim1 INTO ls_bsim1 .
IF sy-tabix > 1.
  ls_bsim1-endmenge = lt_bsim1[ sy-tabix - 1 ]-anfmenge.
  ls_bsim1-anfmenge = ls_bsim1-endmenge - ls_bsim1-lv_receipts + ls_bsim1-lv_issues.
   MODIFY lt_bsim1 FROM ls_bsim1.

ENDIF.

*    " Use the previous day's endmenge as the current day's anfmenge
*    ls_bsim1-anfmenge = ls_prev_day-endmenge.
*  ENDIF.
*
*  " Now calculate the endmenge for the current index using receipts and issues
*  ls_bsim1-endmenge = ls_bsim1-anfmenge + ls_bsim1-lv_receipts - ls_bsim1-lv_issues.
*
*  " Modify the current entry in lt_bsim1
*  MODIFY lt_bsim1 FROM ls_bsim1.

ENDLOOP.

SORT lt_bsim1 by budat anfmenge.


BREAK v014rahul.
ENDIF.


ENDFORM.
