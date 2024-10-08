*&---------------------------------------------------------------------*
*& Report ZWASTE_REQUEST_SS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZWASTE_REQUEST_SS. "kjsd

TABLES: t005t. " Table for countries
**
**
*** Define the parameters for the selection screen
**SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
**PARAMETERS: p_option TYPE char10 AS LISTBOX VISIBLE LENGTH 20.
**SELECTION-SCREEN END OF BLOCK b1.
**
*** Populate dropdown values when the selection screen is displayed
**AT SELECTION-SCREEN OUTPUT.
**  PERFORM populate_dropdown.
**
**START-OF-SELECTION.
**  CASE p_option.
**    WHEN 'CREATED'.
**      WRITE: 'Created Date selected'.
**    WHEN 'RELEASED'.
**      WRITE: 'Release Date selected'.
**    WHEN 'ALL'.
**      WRITE: 'ALL'.
**  ENDCASE.
**
***&---------------------------------------------------------------------*
***&      Form  populate_dropdown
***&---------------------------------------------------------------------*
*** This form populates the dropdown values for the selection screen
**FORM populate_dropdown.
**  DATA: lt_values TYPE vrm_values,
**        ls_value  TYPE vrm_value.
**
**  " Append 'Created Date' option to the dropdown
**  ls_value-key = 'CREATED'.
**  ls_value-text = 'Created Date'.
**  APPEND ls_value TO lt_values.
**
**  " Append 'Release Date' option to the dropdown
**  ls_value-key = 'RELEASED'.
**  ls_value-text = 'Release Date'.
**  APPEND ls_value TO lt_values.
**
**   " Append 'Release Date' option to the dropdown
**  ls_value-key = 'ALL'.
**  ls_value-text = 'ALL Date'.
**  APPEND ls_value TO lt_values.
**
**  " Set the values for the parameter p_option using VRM_SET_VALUES
**  CALL FUNCTION 'VRM_SET_VALUES'
**    EXPORTING
**      id     = 'P_OPTION'
**      values = lt_values
**    EXCEPTIONS
**      id_illegal_name = 1
**      others          = 2.
**
**  IF sy-subrc <> 0.
**    WRITE: 'Error populating dropdown'.
**  ENDIF.
**ENDFORM.


*&---------------------------------------------------------------------*
*& Report ZWASTE_REQUEST_SS
*&---------------------------------------------------------------------*
*& This report demonstrates how to populate a dropdown list in the selection screen
*& using the VRM_SET_VALUES function module.
*&---------------------------------------------------------------------*




* Define the parameters for the selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_option TYPE char10 AS LISTBOX VISIBLE LENGTH 20.
SELECTION-SCREEN END OF BLOCK b1.

DATA: log_handle TYPE balloghndl.

* Populate dropdown values when the selection screen is displayed
AT SELECTION-SCREEN OUTPUT.
  PERFORM populate_dropdown.

START-OF-SELECTION.
  CASE p_option.
    WHEN 'CREATED'.
      WRITE: 'Created Date selected'.
    WHEN 'RELEASED'.
      WRITE: 'Release Date selected'.
    WHEN OTHERS.
      WRITE: 'Unknown option selected'.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  populate_dropdown
*&---------------------------------------------------------------------*
* This form populates the dropdown values for the selection screen
FORM populate_dropdown.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  TRY.
      " Append 'Created Date' option to the dropdown
      ls_value-key = 'CREATED'.
      ls_value-text = 'Created Date'.
      APPEND ls_value TO lt_values.

      " Append 'Release Date' option to the dropdown
      ls_value-key = 'RELEASED'.
      ls_value-text = 'Release Date'.
      APPEND ls_value TO lt_values.

      " Set the values for the parameter p_option using VRM_SET_VALUES
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = 'P_OPTION'
          values = lt_values
        EXCEPTIONS
          id_illegal_name = 1
          others          = 2.

      IF sy-subrc <> 0.
        " Log error
        CALL FUNCTION 'BAL_LOG_CREATE'
          EXPORTING
            object     = 'ZWASTE'
            subobject  = 'SCREEN'
            extnumber  = '0001'
          IMPORTING
            log_handle = log_handle.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            log_handle   = log_handle
            msgid        = 'ZW'
            msgno        = '001'
            msgty        = 'E'
            msgv1        = 'Dropdown Error'
            msgv2        = 'Unable to populate dropdown'.
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            log_handle = log_handle.
        WRITE: 'Error populating dropdown'.
      ENDIF.

    CATCH cx_root INTO DATA(lx_root).
      " Catch any other error
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          object     = 'ZWASTE'
          subobject  = 'SCREEN'
          extnumber  = '0001'
        IMPORTING
          log_handle = log_handle.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          log_handle   = log_handle
          msgid        = 'ZW'
          msgno        = '002'
          msgty        = 'E'
          msgv1        = lx_root->get_text( ).
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          log_handle = log_handle.
      WRITE: / 'Unexpected error:', lx_root->get_text( ).
  ENDTRY.
ENDFORM.
