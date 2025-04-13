*&---------------------------------------------------------------------*
*& Report ZPRG_FILEUPLOAD2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPRG_FILEUPLOAD2.

TYPES : BEGIN OF LTY_VBAK,
          VBELN TYPE VBELN_VA,
          ERDAT TYPE ERDAT,
          ERNAM TYPE ERNAM,
          VBTYP TYPE VBTYPL,
        END OF LTY_VBAK .

DATA : LT_DATA  TYPE TABLE OF LTY_VBAK,
       LS_DATA  TYPE  LTY_VBAK,
       lv_VBELN TYPE VBELN_VA.

DATA  : LV_FILEPATH(20) TYPE C VALUE '/tmp/orderss.txt'.
DATA  :  lv_string TYPE string .

SELECT-OPTIONS : S_VBELN FOR lv_VBELN.

START-OF-SELECTION .
  SELECT VBELN ERDAT  ERNAM  VBTYP
    FROM VBAK INTO TABLE LT_DATA
    WHERE VBELN IN S_VBELN .

END-of-SELECTION .


*write a file  at applaictaion layer  (AL11)
OPEN DATASET LV_FILEPATH for OUTPUT in TEXT MODE ENCODING DEFAULT .  "to write at the file in Application Layer  - need to open it
IF sy-subrc = 0.
   LOOP AT  LT_DATA INTO ls_data .
     CONCATENATE LS_DATA-vbeln LS_DATA-erdat  LS_DATA-ernam  LS_DATA-vbtyp  INTO lv_string SEPARATED BY '_' .
      TRANSFER lv_string to lV_FILEPATH .
   ENDLOOP.
   CLOSE DATASET
ENDIF.



















***"D:\saurabh  - presesntion layer
***CALL FUNCTION 'GUI_DOWNLOAD'
***  EXPORTING
****   BIN_FILESIZE                    =
***    FILENAME                        = 'D:\saurabh\write_doc.txt'
****   FILETYPE                        = 'ASC'
****   APPEND                          = ' '
***   WRITE_FIELD_SEPARATOR           = 'X'
****   HEADER                          = '00'
****   TRUNC_TRAILING_BLANKS           = ' '
****   WRITE_LF                        = 'X'
****   COL_SELECT                      = ' '
****   COL_SELECT_MASK                 = ' '
****   DAT_MODE                        = ' '
****   CONFIRM_OVERWRITE               = ' '
****   NO_AUTH_CHECK                   = ' '
****   CODEPAGE                        = ' '
****   IGNORE_CERR                     = ABAP_TRUE
****   REPLACEMENT                     = '#'
****   WRITE_BOM                       = ' '
****   TRUNC_TRAILING_BLANKS_EOL       = 'X'
****   WK1_N_FORMAT                    = ' '
****   WK1_N_SIZE                      = ' '
****   WK1_T_FORMAT                    = ' '
****   WK1_T_SIZE                      = ' '
****   WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
****   SHOW_TRANSFER_STATUS            = ABAP_TRUE
****   VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
**** IMPORTING
****   FILELENGTH                      =
***  TABLES
***    DATA_TAB                        = lt_data
****   FIELDNAMES                      =
*** EXCEPTIONS
***   FILE_WRITE_ERROR                = 1
***   NO_BATCH                        = 2
***   GUI_REFUSE_FILETRANSFER         = 3
***   INVALID_TYPE                    = 4
***   NO_AUTHORITY                    = 5
***   UNKNOWN_ERROR                   = 6
***   HEADER_NOT_ALLOWED              = 7
***   SEPARATOR_NOT_ALLOWED           = 8
***   FILESIZE_NOT_ALLOWED            = 9
***   HEADER_TOO_LONG                 = 10
***   DP_ERROR_CREATE                 = 11
***   DP_ERROR_SEND                   = 12
***   DP_ERROR_WRITE                  = 13
***   UNKNOWN_DP_ERROR                = 14
***   ACCESS_DENIED                   = 15
***   DP_OUT_OF_MEMORY                = 16
***   DISK_FULL                       = 17
***   DP_TIMEOUT                      = 18
***   FILE_NOT_FOUND                  = 19
***   DATAPROVIDER_EXCEPTION          = 20
***   CONTROL_FLUSH_ERROR             = 21
***   OTHERS                          = 22
***          .
***IF SY-SUBRC <> 0.
**** Implement suitable error handling here
***ENDIF.