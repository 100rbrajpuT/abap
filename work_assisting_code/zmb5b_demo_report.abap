*&---------------------------------------------------------------------*
*& Report ZPRG_MB5B_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprg_mb5b_1.

INCLUDE zdeclaration_top.

lv_curr_date =  SY-DATUM.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR mara-matnr,   " Material
                  s_bukrs FOR mseg-bukrs,
                  s_werks FOR mseg-werks,   " Plant

                  s_budat FOR mseg-budat_mkpf.   "Posting Date in the Document

SELECTION-SCREEN END OF BLOCK b1.

**INITIALIZATION.
**  s_budat-high = lv_curr_date.
**  s_budat-sign = 'I'.
**  s_budat-option = 'BT'.
**  MODIFY s_budat.




SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_valstk AS CHECKBOX.   " Valued Stock Checkbox
SELECTION-SCREEN END OF BLOCK b2.


SELECT mblnr mjahr zeile bwart matnr werks lgort dmbtr menge meins erfmg erfme gjahr budat_mkpf
  FROM mseg
  INTO TABLE lt_mseg
  WHERE matnr IN s_matnr
  AND bukrs IN s_bukrs
  AND werks IN s_werks
  AND budat_mkpf IN s_budat.

IF sy-subrc = 0.
  SELECT matnr ,   werks ,  lgort ,  labst
INTO CORRESPONDING FIELDS OF TABLE @lt_mard
FROM mard
WHERE matnr IN @s_matnr
AND werks IN @s_werks.
*    and lgort in Lt_mseg-lgort.

    LOOP AT lt_mseg INTO ls_mseg .
    READ TABLE lt_mard INTO ls_mard
      WITH KEY matnr = ls_mseg-matnr
               werks = ls_mseg-werks
               lgort = ls_mseg-lgort.

    IF sy-subrc = 0.
      CLEAR ls_final.
      MOVE-CORRESPONDING ls_mseg TO ls_final.
      MOVE-CORRESPONDING ls_mard TO ls_final.
      ls_final-lv_curr_date = sy-datum.
      APPEND ls_final TO lt_final.
    ENDIF.
  ENDLOOP.


  SORT lt_final BY budat_mkpf DESCENDING.


ENDIF.

*SELECT *
*  FROM mkpf
*  INTO TABLE lt_mkpf
*  WHERE MBLNR IN s_matnr   "MBLNR  matnr
**  AND werks IN s_werks
*  AND budat IN s_budat.



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
        t_table      = lt_final.
    lo_alv->display( ).
*      CATCH cx_salv_msg.

  CATCH cx_salv_msg INTO DATA(lx_msg).
    MESSAGE lx_msg->get_text( ) TYPE 'S'.

ENDTRY.
