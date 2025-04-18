*&---------------------------------------------------------------------*
*& Report ZPRG_FILEUPLOAD1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPRG_FILEUPLOAD1.

TYPES : BEGIN OF lty_data,
        emp_id(2) TYPE n,
        emp_name(30) TYPE c,
      END OF lty_data.

data : lt_result TYPE TABLE of lty_data,
        ls_result TYPE  lty_data.
 DATA :   lv_file   TYPE string .

PARAMETERS : p_file TYPE localfile .

at SELECTION-SCREEN on VALUE-REQUEST FOR p_file .

CALL FUNCTION 'F4_FILENAME'
 EXPORTING
   PROGRAM_NAME        = SYST-CPROG
   DYNPRO_NUMBER       = SYST-DYNNR
   FIELD_NAME          = ' '
 IMPORTING
   FILE_NAME           = p_file
          .

START-OF-SELECTION .
 lv_file = p_file .  " this is explicit type casting bcz --> FILENAME & p_file() are STRING & char125 simultanouly

CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = lv_file
*   FILETYPE                      = 'ASC'
   HAS_FIELD_SEPARATOR           = 'X'
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
*   CODEPAGE                      = ' '
*   IGNORE_CERR                   = ABAP_TRUE
*   REPLACEMENT                   = '#'
*   CHECK_BOM                     = ' '
*   VIRUS_SCAN_PROFILE            =
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   FILELENGTH                    =
*   HEADER                        =
  TABLES
    DATA_TAB                      = lt_result
* CHANGING
*   ISSCANPERFORMED               = ' '
 EXCEPTIONS
   FILE_OPEN_ERROR               = 1
   FILE_READ_ERROR               = 2
   NO_BATCH                      = 3
   GUI_REFUSE_FILETRANSFER       = 4
   INVALID_TYPE                  = 5
   NO_AUTHORITY                  = 6
   UNKNOWN_ERROR                 = 7
   BAD_DATA_FORMAT               = 8
   HEADER_NOT_ALLOWED            = 9
   SEPARATOR_NOT_ALLOWED         = 10
   HEADER_TOO_LONG               = 11
   UNKNOWN_DP_ERROR              = 12
   ACCESS_DENIED                 = 13
   DP_OUT_OF_MEMORY              = 14
   DISK_FULL                     = 15
   DP_TIMEOUT                    = 16
   OTHERS                        = 17
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.

*LOOP AT lt_result fiel.
*
*ENDLOOP.
LOOP AT LT_RESULT ASSIGNING FIELD-SYMBOL(<FS>).
  WRITE: / <FS>-emp_id, <FS>-emp_name . " <FS>-MATNR, <FS>-CHARG, <FS>-LIFNR , <FS>-VCODE, <FS>-CODE_DESCRIPTION.
ENDLOOP.




*************************
inspection lot - Inspection lot 1204 890000087860 was created

Inspection lot 1204 890000087862 was created


Inspection lot 1204 890000087863 was created


*************************
CALL FUNCTION 'CO_TA_TCOKT_READ'
      EXPORTING
        autyp    = t490-autyp
      IMPORTING
        struct   = tcokt
      EXCEPTIONS
        no_entry = 01.
    IF sy-subrc NE 0.
      MESSAGE e758.
    ENDIF.