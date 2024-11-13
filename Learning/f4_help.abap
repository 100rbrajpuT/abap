*&---------------------------------------------------------------------*
*& Report ZF4_HELP_EXAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zf4_help_example.


*PARAMETERS: p_insti TYPE ztblinstitute-institute.

PARAMETERS: p_insti TYPE ztblinstitute-institute
  MATCHCODE OBJECT ZF4_ZDINSTITUTE.   " Link to elementary search help



*
*PARAMETERS: p_ins TYPE vbak-vbeln
*  MATCHCODE OBJECT ZF4_ZDINSTITUTE.   " Link to elementary search help

START-OF-SELECTION.
  WRITE: / 'Selected Institute:', p_insti.
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_insti.

*  PERFORM f4_help_for_institute.
*
*INCLUDE zhelp_example_f4_help_for_if01.


*************************************
