Data declaration:

DATA: wa_bin_ext TYPE soli,
      it_bin_ext TYPE soli_tab,

v_email                TYPE ad_smtpadr,

document           TYPE REF TO cl_document_bcs,
sender             TYPE REF TO cl_sapuser_bcs,
recipient          TYPE REF TO if_recipient_bcs,
recipient1         TYPE REF TO if_recipient_bcs,
bcs_exception      TYPE REF TO cx_bcs,
sent_to_all        TYPE os_boolean,

it_lenght          TYPE so_obj_len,

send_request       TYPE REF TO cl_bcs,
  cc                TYPE OS_BOOLEAN,
subject            TYPE so_obj_des,
att_type            TYPE soodk-objtp,
it_text            TYPE bcsy_text,
wa_text            LIKE soli,
it_bin             TYPE solix_tab,

wa_bin              TYPE string,"SO_RAW255,"SOLIX,
wa_bin1            TYPE solix,

n10(10)            TYPE n.

CONSTANTS: con_newl  TYPE abap_char1 VALUE cl_abap_char_utilities=>newline,

(space for line)
                 

con_tab  TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab..

(tab space)

TRY.
        send_request = cl_bcs=>create_persistent( ).
PERFORM f_head_cont.

PERFORM f_xls_att2.

•Dokument (mit Anhang) setzen(comment)
        CALL METHOD send_request->set_document( document ).

*•"EMAIL RECIPIENTS.**        making recipient a dl(if you want recipient as DL)
         recipient = cl_distributionlist_bcs=>getu_persistent(
         i_dliname = lv_mlrec
         i_private = space ).

*•Absender setzen p_sender as the parameter(the sap id of sender)
        sender = cl_sapuser_bcs=>create( p_sender ).
        CALL METHOD send_request->set_sender
          EXPORTING
            i_sender = sender.

*•"SET RECEIVER.*•"EMAIL RECIPIENTS.
        recipient = cl_cam_address_bcs=>create_internet_address(
                                       v_email ).

        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient = recipient
            i_express   = 'X'.***adding the carbon copy(cc) in the mail**** validating in bu_sso is not blank.


         recipient1 = cl_cam_address_bcs=>create_internet_address(
                                       v_email1 ).


        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient = recipient1
            i_copy      = 'X'.
*•Dokument senden
        CALL METHOD send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = sent_to_all ).
        COMMIT WORK.*•Sende-Error abfangen
      CATCH cx_bcs INTO bcs_exception.
        WRITE: 'error Occurred.'(001).
        WRITE: 'error Type:'(002), bcs_exception->error_type.
        EXIT.
    ENDTRY.


FORM f_head_cont .

Subject= ‘subject line for mail’.

It_text =’Body Content’.

att_type = 'RAW'.

  DESCRIBE TABLE it_text LINES n10.
  n10 = ( n10 - 1 ) * 255 + STRLEN( wa_text ).
  it_lenght = n10.
  document = cl_document_bcs=>create_document(
            i_type    = att_type
            i_text    = it_text
            i_length  = it_lenght
            i_subject = subject ).

Endform.
FORM f_xls_att2 .

********************************************************************* Fill Entire Final Data into it_bin_ext.

converting into raw255 format.*******************************************************************
  CALL FUNCTION 'SO_SOLITAB_TO_SOLIXTAB'
    EXPORTING
      ip_solitab  = it_bin_ext
    IMPORTING
      ep_solixtab = it_bin.
  REFRESH it_bin_ext.

  att_type = 'XLS'.
  subject = ‘Subject for attached file’.

DESCRIBE TABLE it_bin LINES n10.
  n10 = ( n10 - 1 ) * 255 + STRLEN( wa_bin_ext ).
  it_lenght = n10.
  CLEAR wa_bin_ext.

  CALL METHOD document->add_attachment
    EXPORTING
      i_attachment_type    = att_type
      i_att_content_hex    = it_bin
      i_attachment_size    = it_lenght
      i_attachment_subject = subject.


  REFRESH: it_bin,
           it_bin_ext.
  CLEAR:    it_lenght.

Endform.