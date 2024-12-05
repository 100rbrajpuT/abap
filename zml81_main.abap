*&---------------------------------------------------------------------*
*& Module Pool      ZML81N_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZML81N1_TOP_CP.
*INCLUDE ZML81N1_TOP                             .    " Global Data

* INCLUDE ZML81N1_O01                             .  " PBO-Modules
* INCLUDE ZML81N1_I01                             .  " PAI-Modules
* INCLUDE ZML81N1_F01                             .  " FORM-Routines


INCLUDE ZML81N_1_GET_HELPF01_CP.
*INCLUDE zml81n_1_get_helpf01.

INCLUDE ZML81N_PBO_CP.
*INCLUDE ZML81N_PBO.

INCLUDE ZML81N_1_PAI_CP.
*INCLUDE zml81n_1_pai.

at SELECTION-SCREEN OUTPUT .

   LOOP AT SCREEN.
     IF screen-name EQ 'P_BOTH'.
         SCREEN-INVISIBLE =  '1'.
          MODIFY SCREEN.
      endif.
   ENDLOOP.
at SELECTION-SCREEN  .
*  IF  S_DATE-LOW is INITIAL .
*     MESSAGE 'Please Enter The From Date' TYPE 'E'.
*  ENDIF.
*
*  IF  S_DATE-high is INITIAL .
*     MESSAGE 'Please Enter The To Date' TYPE 'E'.
*  ENDIF.

at SELECTION-SCREEN on VALUE-REQUEST FOR S_veh-low.
  PERFORM get_help.

INCLUDE ZML81N_1_9004_PBO_CP.
*INCLUDE zml81n_1_9004_pbo.

INCLUDE ZML81N_1_9004_PAI_CP.
*INCLUDE zml81n_1_9004_pai.

INCLUDE ZML81N_1_9006_PBO_CP.
*INCLUDE zml81n_1_9006_pbo.

INCLUDE ZML81N_1_9006_PAI_CP.
*INCLUDE zml81n_1_9006_pai.
