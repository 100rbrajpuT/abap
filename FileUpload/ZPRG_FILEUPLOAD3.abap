*&---------------------------------------------------------------------*
*& Report ZPRG_FILEUPLOAD3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPRG_FILEUPLOAD3.

TYPES : BEGIN OF LTY_VBAK,
          VBELN TYPE VBELN_VA,
          ERDAT TYPE ERDAT,
          ERNAM TYPE ERNAM,
          VBTYP TYPE VBTYPL,
        END OF LTY_VBAK .

DATA : LT_DATA TYPE TABLE OF LTY_VBAK,
       LS_DATA TYPE  LTY_VBAK.
" lv_VBELN TYPE VBELN_VA.

DATA  :  LV_FILENAME(20)  TYPE C VALUE '/tmp/orderss.txt'.
DATA : LV_STRING TYPE STRING .

OPEN DATASET LV_FILENAME FOR INPUT IN TEXT MODE ENCODING DEFAULT .
IF SY-SUBRC = 0.
  DO .
    READ DATASET LV_FILENAME INTO LV_STRING .    " this will read the file one by one
    IF SY-SUBRC = 0.
      SPLIT  LV_STRING AT '_'  INTO LS_DATA-VBELN LS_DATA-ERDAT LS_DATA-ERNAM  LS_DATA-VBTYP .
      APPEND LS_DATA TO LT_DATA .
      CLEAR : LS_DATA .
    ELSE.
      EXIT.

    ENDIF.

  ENDDO.
  CLOSE DATASET LV_FILENAME.
ENDIF.

LOOP AT LT_DATA INTO LS_DATA.
  WRITE : / LS_DATA-VBELN , LS_DATA-ERDAT , LS_DATA-ERNAM , LS_DATA-VBTYP .
ENDLOOP.