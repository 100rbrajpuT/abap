*&---------------------------------------------------------------------*
*& Report Z_SEND_EMAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SEND_EMAIL.
**********************************************************************************************


DATA: lv_subject        TYPE SO_OBJ_DES,   "string
      lv_email_body     TYPE string,
      lt_text_lines     TYPE TABLE OF tline,
      lv_email_receiver TYPE adr6-smtp_addr,
      lt_email_content  TYPE bcsy_text.

DATA: lo_send_request   TYPE REF TO cl_bcs,
      lo_document       TYPE REF TO cl_document_bcs,
      lo_recipient      TYPE REF TO if_recipient_bcs.


****CALL FUNCTION 'READ_TEXT'
****  EXPORTING
****   CLIENT                        = SY-MANDT
****    id                            =
****    language                      =
****    NAME                          =
****    OBJECT                        =
****   ARCHIVE_HANDLE                = 0
****   LOCAL_CAT                     = ' '
**** IMPORTING
****   HEADER                        =
****   OLD_LINE_COUNTER              =
****  TABLES
****    lines                         =
**** EXCEPTIONS
****   ID                            = 1
****   LANGUAGE                      = 2
****   NAME                          = 3
****   NOT_FOUND                     = 4
****   OBJECT                        = 5
****   REFERENCE_CHECK               = 6
****   WRONG_ACCESS_TO_ARCHIVE       = 7
****   OTHERS                        = 8
****          .
****IF sy-subrc <> 0.
**** Implement suitable error handling here
****ENDIF.


" Step 1: Retrieve Standard Text from SO10
CALL FUNCTION 'READ_TEXT'
  EXPORTING
    id                      =  'ST' "'MAIL'  " Text ID (adjust based on your setup)
    name                    = 'Z_OVERDUE_REMINDER'   "Z_OVERDUE_REMINDER
    language                = sy-langu
    object                  = 'TEXT'
  TABLES
    lines                   = lt_text_lines
  EXCEPTIONS
    id                      = 1
    language                = 2
    name                    = 3
    not_found               = 4
    object                  = 5
    reference_check         = 6
    wrong_access_to_archive = 7
    OTHERS                  = 8.

IF sy-subrc <> 0.
  WRITE: / 'Error: Could not read the standard text.'.
  STOP.
ENDIF.

" Step 2: Concatenate the lines into a single string
LOOP AT lt_text_lines INTO DATA(ls_line).
  CONCATENATE lv_email_body ls_line-tdline INTO lv_email_body SEPARATED BY cl_abap_char_utilities=>newline.
ENDLOOP.

"lv_email_body  = 'his name is ss'.

" Replace placeholders in the text dynamically
REPLACE '{RefNo}' WITH '12345' INTO lv_email_body.
REPLACE '{Member Unit Name}' WITH 'ABC Ltd.' INTO lv_email_body.
REPLACE '{Address}' WITH '123 Street, City' INTO lv_email_body.
REPLACE '{Date}' WITH sy-datum INTO lv_email_body.

" Step 3: Split the email body into lines for sending
SPLIT lv_email_body AT cl_abap_char_utilities=>newline INTO TABLE lt_email_content.

" Step 4: Create email request
lo_send_request = cl_bcs=>create_persistent( ).

" Create email content
lv_subject = 'Overdue Payment Reminder'.
lo_document = cl_document_bcs=>create_document(
                i_type    = 'RAW'
                i_text    = lt_email_content
                i_subject = lv_subject ).

lo_send_request->set_document( lo_document ).

" Add recipient
lv_email_receiver = 'customer@example.com'. " Replace dynamically
*lo_send_request->add_recipient( lv_email_receiver ).
lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_receiver ). " Create recipient object
lo_send_request->add_recipient( lo_recipient ). " Add recipient object
*lo_send_request->add_recipient(
*  EXPORTING
*    i_address_type = 'U'
*    i_address      = lv_email_receiver
*    i_express      = 'X' ).

" Step 5: Send the email
CALL METHOD lo_send_request->send(
  i_with_error_screen = 'X' ).

COMMIT WORK.
WRITE: / 'Email sent successfully.'.







**********************************************************************************************
***
***
***DATA: lv_subject        TYPE SO_OBJ_DES,   "string
***      lv_email_body     TYPE string,
***      lt_text_lines     TYPE TABLE OF tline,
***      lv_email_receiver TYPE string,
***      lt_email_content  TYPE bcsy_text.
***
***DATA: lo_send_request   TYPE REF TO cl_bcs,
***      lo_document       TYPE REF TO cl_document_bcs,
***      lo_recipient      TYPE REF TO if_recipient_bcs.
***
***" Step 1: Retrieve Standard Text from SO10
***CALL FUNCTION 'READ_TEXT'
***  EXPORTING
***    id                      = 'MAIL'  " Text ID (adjust based on your setup)
***    name                    = 'Z_OVERDUE_REMINDER'
***    language                = sy-langu
***    object                  = 'TEXT'
***  TABLES
***    lines                   = lt_text_lines
***  EXCEPTIONS
***    id                      = 1
***    language                = 2
***    name                    = 3
***    not_found               = 4
***    object                  = 5
***    reference_check         = 6
***    wrong_access_to_archive = 7
***    OTHERS                  = 8.
***
***IF sy-subrc <> 0.
***  WRITE: / 'Error: Could not read the standard text.'.
***  STOP.
***ENDIF.
***
***" Step 2: Concatenate the lines into a single string
***LOOP AT lt_text_lines INTO DATA(ls_line).
***  CONCATENATE lv_email_body ls_line-tdline INTO lv_email_body SEPARATED BY cl_abap_char_utilities=>newline.
***ENDLOOP.
***
***" Replace placeholders in the text dynamically
***REPLACE '{RefNo}' WITH '12345' INTO lv_email_body.
***REPLACE '{Member Unit Name}' WITH 'ABC Ltd.' INTO lv_email_body.
***REPLACE '{Address}' WITH '123 Street, City' INTO lv_email_body.
***REPLACE '{Date}' WITH sy-datum INTO lv_email_body.
***
***" Step 3: Split the email body into lines for sending
***SPLIT lv_email_body AT cl_abap_char_utilities=>newline INTO TABLE lt_email_content.
***
***" Step 4: Create email request
***lo_send_request = cl_bcs=>create_persistent( ).
***
***" Create email content
***lv_subject = 'Overdue Payment Reminder'.
***lo_document = cl_document_bcs=>create_document(
***                i_type    = 'RAW'
***                i_text    = lt_email_content
***                i_subject = lv_subject ).
***
***lo_send_request->set_document( lo_document ).
***
***" Add recipient
***lv_email_receiver = 'customer@example.com'. " Replace dynamically
***lo_recipient = lo_send_request->add_recipient(
***                  i_address  = lv_email_receiver
***                  i_express  = 'X' ).
***
*****lo_send_request->add_recipient(
*****  EXPORTING
*****    "i_address_type = 'U'
*****    i_address      = lv_email_receiver
*****    i_express      = 'X' ).
***
***" Step 5: Send the email
***CALL METHOD lo_send_request->send(
***  i_with_error_screen = 'X' ).
***
***COMMIT WORK.
***WRITE: / 'Email sent successfully.'.
