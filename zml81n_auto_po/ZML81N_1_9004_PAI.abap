*----------------------------------------------------------------------*
***INCLUDE ZML81N_1_9004_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.

  CASE  sy-ucomm.

    WHEN 'BACK'.
      CLEAR : lv_cal_amt, lv_zdrt ,vbrp-kzwi3,vbrp-netwr,vbrp-kzwi1,
             vbrp-kzwi3,vbrp-kzwi4,ztp_hdr-zf_amt,vbrp-kzwi6,vbrp-skfbp,vbrp-wavwr,vbrp-kzwi5.
      SET SCREEN 0.
    WHEN 'ADD' OR 'ENTER'.
      PERFORM update_amt.
    WHEN 'CP'.
      PERFORM create_po.
    WHEN 'SE'.
      IF  ekko-ebeln IS NOT INITIAL .
        PERFORM create_ser.
      ELSE.
        MESSAGE ' Please Create The Po ' TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form UPDATE_AMT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_amt .
  REFRESH :lt_rows.
 CLEAR : ztp_item-zt_adn,
ztp_item-zt_ded,
ztp_item-zt_toll,
ztp_item-zt_cond,
ztp_item-zd_adn,
ztp_item-zd_ded,
ztp_item-zt_tamt,
ztp_item-zd_tamt,
ztp_item-zaqty.


  CLEAR : lv_cal_amt ,
          lv_zdrt  ,
          lv_tot_t  ,
          lv_tot_d ,
         lv_tot_t_a,
  lv_tot_t_d   ,
  lv_tot_tl_c ,
  lv_tot_con  ,
  lv_tot_de_ad ,
  lv_tot_de_du ,
  lv_final_amt  .








  DATA : lv_m TYPE string.
  CALL METHOD obj->check_changed_data .

***************************************
  CALL METHOD obj->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

* ***************************************
  IF lt_rows IS NOT INITIAL .
    LOOP AT lt_rows INTO ls_rows.
      READ TABLE lt_final  INTO ls_final INDEX ls_rows-index .
      IF  sy-subrc = 0.
        ls_final-chk = 'X'.
        MODIFY lt_final FROM ls_final INDEX ls_rows-index TRANSPORTING chk.
        CLEAR :ls_final,ls_rows.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR : lv_cal_amt, lv_zdrt ,vbrp-kzwi3,vbrp-netwr.

  LOOP AT lt_final  INTO ls_final WHERE chk = 'X'.
*    IF ls_final-ebeln IS NOT INITIAL.
*      CONCATENATE ls_final-manifest 'Already Processed' INTO lv_m.
**      MESSAGE lv_m TYPE 'E'.
*    ELSE.
    IF ls_final-add_amt < 0  .
      MESSAGE 'Transport Addition Amount Can not be negative'  TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.

      IF ls_final-ADD_AMT > 0 AND ( ls_final-ZTRP_REMARK IS INITIAL OR ls_final-ZTRP_REMARK = '').
       MESSAGE 'Please provide remark for transport other add amount.' TYPE 'E' DISPLAY LIKE 'I'.
     ENDIF.

           IF ls_final-d_ad_amt > 0 AND ( ls_final-ZDETEN_REMARK IS INITIAL OR ls_final-ZDETEN_REMARK = '').
       MESSAGE 'Please provide remark for detention other add amount.' TYPE 'E' DISPLAY LIKE 'I'.
     ENDIF.

           IF ls_final-zdevkm > 0 AND ( ls_final-ZDEVKM_REMARK IS INITIAL OR ls_final-ZDEVKM_REMARK = '').
       MESSAGE 'Please provide remark for Deviation KM.' TYPE 'E' DISPLAY LIKE 'I'.
     ENDIF.


    IF ls_final-de_amt < 0 .
      MESSAGE 'Transport Less Amount Can not be negative'  TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.

    IF ls_final-d_ded_amt < 0 .
      MESSAGE 'Detention Less Amount Can not be negative'  TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.

     IF ls_final-cond_amt < 0 .
      MESSAGE 'Conductor Charge Can not be negative'  TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.




    lv_cal_amt = ( ls_final-calculated_amt + ls_final-add_amt + ls_final-toll_amt +
                  ls_final-cond_amt ) -  ls_final-de_amt.
*      IF ls_final-d_ded_amt IS NOT INITIAL AND  ls_final-d_ad_amt IS NOT INITIAL .
    lv_zdrt    =   ( ls_final-zdrt +  ls_final-d_ad_amt ) -  ls_final-d_ded_amt.
    ls_final-d_tot_amt =  lv_zdrt.
*      ENDIF.

    IF ls_final-zact_seskm > 0 AND LS_FINAL-ZDEVKM > 0 .
    MESSAGE 'Deviation KM already added..!!'  TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

    IF ls_final-zact_seskm = 0 AND LS_FINAL-ZDEVKM > 0 .
      ls_final-zact_seskm = ls_final-zses_km.
    ENDIF.

    ls_final-zses_km = ls_final-zses_km + ls_final-zdevkm.
    ls_final-net_amt =  lv_cal_amt.
    ls_final-net_amt = ls_final-net_amt - ls_final-PENALTY_AMT.

    MODIFY  lt_final FROM ls_final TRANSPORTING net_amt d_tot_amt zses_km zact_seskm WHERE manifest = ls_final-manifest.


    lv_tot_t =   lv_tot_t + ls_final-net_amt.
    lv_tot_d =   lv_tot_d + ls_final-d_tot_amt.


    lv_zdrt    = lv_zdrt  + ls_final-zdrt.


    lv_tot_t_a    = lv_tot_t_a + ls_final-add_amt.
    lv_tot_t_d    = lv_tot_t_d + ls_final-de_amt.
    lv_tot_tl_c   = lv_tot_tl_c + ls_final-toll_amt.
    lv_tot_con    = lv_tot_con + ls_final-cond_amt.

    lv_tot_de_ad  = lv_tot_de_ad + ls_final-d_ad_amt.
    lv_tot_de_du  = lv_tot_de_du + ls_final-d_ded_amt.


    lv_actual_qty =  lv_actual_qty + ls_final-actual_qty.


  ENDLOOP.



  LOOP AT lt_final  INTO ls_final WHERE chk = ' '.
    CLEAR : ls_final-d_tot_amt ,
            ls_final-net_amt,
            ls_final-add_amt,
            ls_final-de_amt,
           " ls_final-toll_amt,
            ls_final-cond_amt,
            ls_final-d_ad_amt,
            ls_final-d_ded_amt,
            ZTP_ITEM-ZODT.


    MODIFY  lt_final FROM ls_final TRANSPORTING net_amt d_tot_amt add_amt
                     de_amt toll_amt cond_amt d_ad_amt d_ded_amt
                     WHERE manifest = ls_final-manifest
                      AND  chk = ' '.
  ENDLOOP.


 if  p_both  =  'X'.

  IF lv_tot_t_a IS NOT INITIAL .
    CLEAR : ztp_item-zt_adn.
    ztp_item-zt_adn =  lv_tot_t_a.
    CLEAR lv_tot_t_a.
    ELSE.
  CLEAR : ztp_item-zt_adn.
  ENDIF.

  IF lv_tot_t_d IS NOT INITIAL .
    CLEAR : ztp_item-zt_ded.
    ztp_item-zt_ded = lv_tot_t_d.
    CLEAR lv_tot_t_d.
    else .
  CLEAR : ztp_item-zt_ded.
  ENDIF.

  IF  lv_tot_tl_c IS NOT INITIAL .
    CLEAR ztp_item-zt_toll.
    ztp_item-zt_toll =  lv_tot_tl_c.
    CLEAR lv_tot_tl_c.
    else .
    CLEAR ztp_item-zt_toll.
  ENDIF.

  IF lv_tot_con IS NOT INITIAL .
    CLEAR ztp_item-zt_cond.
    ztp_item-zt_cond =  lv_tot_con.
    CLEAR lv_tot_con.
    else .
       CLEAR ztp_item-zt_cond.
  ENDIF.


  IF lv_tot_de_ad IS NOT INITIAL .
    CLEAR ztp_item-zd_adn.
    ztp_item-zd_adn = lv_tot_de_ad.
    CLEAR lv_tot_de_ad.
    else .
        CLEAR ztp_item-zd_adn.
  ENDIF.


  IF lv_tot_de_du IS NOT INITIAL .
    CLEAR ztp_item-zd_ded.
    ztp_item-zd_ded = lv_tot_de_du.
    CLEAR lv_tot_de_du.
    else .
      CLEAR ztp_item-zd_ded.

  ENDIF .


  IF lv_tot_t IS NOT INITIAL .
    CLEAR ztp_item-zt_tamt.

    ztp_item-zt_tamt  =   lv_tot_t.
    CLEAR lv_tot_t.
    else .
         CLEAR ztp_item-zt_tamt.
  ENDIF .

  IF lv_tot_d IS NOT INITIAL .
    CLEAR  ztp_item-zd_tamt .
    ztp_item-zd_tamt  =  lv_tot_d .
    CLEAR lv_tot_d.
    else .
        CLEAR  ztp_item-zd_tamt .
  ENDIF.

ELSEIF p_tr =  'X'.

  IF lv_tot_t_a IS NOT INITIAL .
    CLEAR : ztp_item-zt_adn.
    ztp_item-zt_adn =  lv_tot_t_a.
    CLEAR lv_tot_t_a.
    ELSE.
  CLEAR : ztp_item-zt_adn.
  ENDIF.

  IF lv_tot_t_d IS NOT INITIAL .
    CLEAR : ztp_item-zt_ded.
    ztp_item-zt_ded = lv_tot_t_d.
    CLEAR lv_tot_t_d.
    else .
  CLEAR : ztp_item-zt_ded.
  ENDIF.

  IF  lv_tot_tl_c IS NOT INITIAL .
    CLEAR ztp_item-zt_toll.
    ztp_item-zt_toll =  lv_tot_tl_c.
    CLEAR lv_tot_tl_c.
    else .
    CLEAR ztp_item-zt_toll.
  ENDIF.

  IF lv_tot_con IS NOT INITIAL .
    CLEAR ztp_item-zt_cond.
    ztp_item-zt_cond =  lv_tot_con.
    CLEAR lv_tot_con.
    else .
       CLEAR ztp_item-zt_cond.
  ENDIF.


  IF lv_tot_t IS NOT INITIAL .
    CLEAR ztp_item-zt_tamt.

    ztp_item-zt_tamt  =   lv_tot_t.
    CLEAR lv_tot_t.
    else .
         CLEAR ztp_item-zt_tamt.
  ENDIF .




ELSEIF p_de =  'X'.









  IF lv_tot_de_ad IS NOT INITIAL .
    CLEAR ztp_item-zd_adn.
    ztp_item-zd_adn = lv_tot_de_ad.
    CLEAR lv_tot_de_ad.
    else .
        CLEAR ztp_item-zd_adn.
  ENDIF.


  IF lv_tot_de_du IS NOT INITIAL .
    CLEAR ztp_item-zd_ded.
    ztp_item-zd_ded = lv_tot_de_du.
    CLEAR lv_tot_de_du.
    else .
      CLEAR ztp_item-zd_ded.

  ENDIF .



  IF lv_tot_d IS NOT INITIAL .
    CLEAR  ztp_item-zd_tamt .
    ztp_item-zd_tamt  =  lv_tot_d .
    CLEAR lv_tot_d.
    else .
        CLEAR  ztp_item-zd_tamt .
  ENDIF.

ENDIF.

  IF lv_actual_qty IS NOT INITIAL .
    CLEAR : ztp_item-zaqty.
    ztp_item-zaqty = lv_actual_qty.
    CLEAR lv_actual_qty.
    else .
       CLEAR : ztp_item-zaqty.
  ENDIF.

  lv_final_amt =    ztp_item-zt_tamt +  ztp_item-zd_tamt .


     if  lv_final_amt is NOT INITIAL.
     CLEAR  ZTP_ITEM-ZODT.
     ZTP_ITEM-ZODT =  lv_final_amt.
     CLEAR lv_final_amt.
       ENDIF.

  IF p_tr =  'X'.
    vbrp-netwr   =  lv_cal_amt.
  ENDIF.

  IF p_de =  'X'.
    vbrp-kzwi3  =  lv_zdrt.
  ENDIF.

  IF  p_both =  'X'..
    vbrp-netwr   =  lv_cal_amt.
    vbrp-kzwi3  =  lv_zdrt.
  ENDIF.


  vbrp-kzwi1  = ( vbrp-netwr + vbrp-skfbp ) - vbrp-wavwr.

  vbrp-kzwi6  =  ( vbrp-kzwi3 + vbrp-kzwi4 )  -  vbrp-kzwi5.

  ztp_hdr-zf_amt  =  vbrp-kzwi1 +  vbrp-kzwi6.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_po .


  DATA :
    lv_datum(10) TYPE c,
    lv_year(4)   TYPE c,
    lv_date(2)   TYPE c,
    lv_mon(2)    TYPE c,
*    lv_datum1(10) TYPE c,
    lv_datum2    TYPE sy-datum,
    start_date   TYPE sy-datum,
    end_date     TYPE sy-datum,
    lv_mesg      TYPE  string,
    lv_po_cr     TYPE string,
    ans          TYPE C.

CLEAR lv_po_cr.
 CONCATENATE 'Do You Want To Create The Po For This Vendor' LFA1-NAME1 INTO lv_po_cr SEPARATED BY ' '.


*if ANS =  '1'.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = s_date-low
    IMPORTING
      ev_month_begin_date = start_date
      ev_month_end_date   = end_date.

  lv_datum =  s_date-low.
  lv_year  =  lv_datum+0(4).
  lv_mon  =  lv_datum+4(2).
  lv_date =   '01'.
  REFRESH: lt_message,lt_bdc.
  CONCATENATE lv_date '.' lv_mon '.' lv_year  INTO lv_datum1.


  CLEAR : lv_ses , lv_ebeln,lv_bukrs ,lv_strlen.
  REFRESH : lt_fieldcat,lt_bdc,lt_bdcdata,lt_message,lt_ztp_hdr,lt_ztp_item.





  IF ekko-ebeln IS INITIAL .

*********************
  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
*   TITLEBAR                    = ' '
*   DIAGNOSE_OBJECT             = ' '
    text_question               = lv_po_cr
   TEXT_BUTTON_1                = 'Yes' "'Ja'(001)
*   ICON_BUTTON_1               = ' '
   TEXT_BUTTON_2               =  'No'  "'Nein'(002)
*   ICON_BUTTON_2               = ' '
*   DEFAULT_BUTTON              = '1'
*   DISPLAY_CANCEL_BUTTON       = 'X'
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
 IMPORTING
   ANSWER                      = ANS
* TABLES
*   PARAMETER                   =
* EXCEPTIONS
*   TEXT_NOT_FOUND              = 1
*   OTHERS                      = 2
          .

   if ans =  '1'.
    PERFORM create_po_bdc.

*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=MEDOCTYPE'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'MEPO_TOPLINE-BSART'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                'ZSRV'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'MEPO_TOPLINE-SUPERFIELD'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                'ZSRV'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-SUPERFIELD'
*                                 lfa1-lifnr.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'MEPO1222-EKORG'
*                                '1000'.
*  PERFORM bdc_field       USING 'MEPO1222-EKGRP'
*                                '100'.
*  PERFORM bdc_field       USING 'MEPO1222-BUKRS'
*                                lv_bukrs.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=MEV4001BUTTON'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'MEPO_TOPLINE-SUPERFIELD'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                'ZSRV'.
**perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
**                              '1014034 D K Transport'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'MEPO1222-EKORG'
*                                '1000'.
*  PERFORM bdc_field       USING 'MEPO1222-EKGRP'
*                                '100'.
*  PERFORM bdc_field       USING 'MEPO1222-BUKRS'
*                                lv_bukrs.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                'ZSRV'.
**perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
**                              '1014034 D K Transport'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'MEPO1222-EKORG'
*                                '1000'.
*  PERFORM bdc_field       USING 'MEPO1222-EKGRP'
*                                '100'.
*  PERFORM bdc_field       USING 'MEPO1222-BUKRS'
*                                 lv_bukrs.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'MEPO1211-NAME1(01)'.
*  PERFORM bdc_field       USING 'MEPO1211-KNTTP(01)'
*                                'U'.
*  PERFORM bdc_field       USING 'MEPO1211-EPSTP(01)'
*                                'D'.
*  PERFORM bdc_field       USING 'MEPO1211-TXZ01(01)'
*                                'tansportation service'.
*  PERFORM bdc_field       USING 'MEPO1211-MENGE(01)'
*                                '                1'.
*  PERFORM bdc_field       USING 'MEPO1211-MEINS(01)'
*                                'AU'.
*  PERFORM bdc_field       USING 'MEPO1211-WGBEZ(01)'
*                                'S'.
*  PERFORM bdc_field       USING 'MEPO1211-NAME1(01)'
*                                lv_plant.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=TABIDT2'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                'ZSRV'.
**perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
**                              '1014034 D K Transport'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ESLL-UEBTO(01)'.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_field       USING 'ESLL-KTEXT1(01)'
*                                'tranport service'.
*  PERFORM bdc_field       USING 'ESLL-MENGE(01)'
*                                '1'.
*  PERFORM bdc_field       USING 'ESLL-MEINS(01)'
*                                'au'.
*  PERFORM bdc_field       USING 'ESLL-TBTWR(01)'
*                                '1'.
*  PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=MESAVE'.
*  PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                '   1'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ESUH-COMMITMENT'.
*  PERFORM bdc_field       USING 'ESUH-SUMNOLIM'
*                                'X'.
*  PERFORM bdc_field       USING 'ESUH-COMMITMENT'
*                                '1'.
*  PERFORM bdc_dynpro      USING 'SAPLSPO2' '0101'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=OPT1'.






*  CALL TRANSACTION 'ME21N' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_message.
    DATA : lv_flag TYPE c.
    IF lt_message IS NOT INITIAL.
      delete lt_message WHERE msgtyp = 'W'.
      delete lt_message WHERE msgtyp = 'I'.

      LOOP AT lt_message INTO  ls_message.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = ls_message-msgid
*           LANG      = '-D'
            no        = ls_message-msgnr
            v1        = sy-msgv1
            v2        = sy-msgv2
            v3        = sy-msgv3
            v4        = sy-msgv4
          IMPORTING
            msg       = v_msg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
          IF ls_message-msgtyp = 'E'.
            ls_bdc-icon   =  c_red.
            ls_bdc-id     =  ls_message-msgid.
            ls_bdc-number =  ls_message-msgnr.
            ls_bdc-message = v_msg.
            APPEND ls_bdc TO lt_bdc.

          ELSEIF ls_message-msgtyp = 'S' ."AND  ls_message-msgv2 IS NOT INITIAL .

            lv_strlen =  strlen(  ls_message-msgv2 ).
            IF  lv_strlen = 10 AND ls_message-msgv1 <> 'ESLL-TBTWR'.
              ls_bdc-icon   =  c_green.
              lv_ebeln       = ls_message-msgv2.
              lv_flag         =  'X'.
              ls_bdc-id     =  ls_message-msgid.
              ls_bdc-number =  ls_message-msgnr.
              ls_bdc-message = v_msg.
              APPEND ls_bdc TO lt_bdc.
              else .
*            ENDIF.
              ls_bdc-icon   =  c_yellow.
              ls_bdc-id     =  ls_message-msgid.
              ls_bdc-number =  ls_message-msgnr.
              ls_bdc-message = v_msg.
              APPEND ls_bdc TO lt_bdc.
              ENDIF.
          ENDIF.
*        ls_bdc-type   =  ls_message-msgtyp .

        ENDIF.
      ENDLOOP.

    ENDIF.

    IF  lv_flag =  'X'.


      ls_ztp_hdr-tplst     =  lv_plant.
      ls_ztp_hdr-zshtyp    =  zunload-shtyp.
      ls_ztp_hdr-ebeln     =  lv_ebeln.
      ls_ztp_hdr-zdate     =  ztrpq-zvalidf.
      ls_ztp_hdr-lifnr     =  lfa1-lifnr.
      ls_ztp_hdr-zfdate     =  start_date.
      ls_ztp_hdr-ztdate     =  end_date.
      ls_ztp_hdr-eqtyp      =  equi-eqtyp.
      APPEND ls_ztp_hdr TO lt_ztp_hdr.
      MODIFY ztp_hdr FROM TABLE lt_ztp_hdr.
      WAIT UP TO 1 SECONDS.
      COMMIT WORK .
      CLEAR lv_flag.
    ENDIF.




*     ENDIF.
*  ELSE.
*    CONCATENATE 'For This Transporter' lfa1-lifnr 'Po Already created'
*    INTO lv_mesg SEPARATED BY ' '.
*    MESSAGE lv_mesg  TYPE 'E'.
*  ENDIF.


*  REFRESH :lt_bdcdata,lt_message.
*  CLEAR :  lv_strlen.
*  IF p_tr = 'X'.
*
*    lv_text1 = 'Transport Charges'.
*    lv_text2 = 'T'.
*  ELSEIF p_de =  'X'.
*
*    lv_text1 =  'Detention Charges'.
*    lv_text2 = 'D'.
*  ELSEIF  p_both =  'X'.
*
*    lv_text1 =  ' Transport & Detention '.
*    lv_text2 = 'T&D'.
*  ENDIF.
*  DATA : lv_famt(15) TYPE c.
*  lv_famt  =  ztp_hdr-zf_amt.
*  CONDENSE : lv_famt.
*
*
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=SELP'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RM11P-NEW_ROW'.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0340'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RM11R-EBELN'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=ENTE'.
*  PERFORM bdc_field       USING 'RM11R-EBELN'
*                                 lv_ebeln.
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=NEU'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RM11P-NEW_ROW'.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'ESSR-TXZ01'
*                                lv_text1.
*  PERFORM bdc_field       USING 'ESSR-KNTTP'
*                                'K'.
*  PERFORM bdc_field       USING 'ESSR-LBLDT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'ESSR-BLDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'ESSR-BUDAT'
*                                lv_datum1.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RM11P-NPLNR(01)'.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_field       USING 'ESLL-KTEXT1(01)'
*                                'Tranport Service'.
*  PERFORM bdc_field       USING 'ESLL-MENGE(01)'
*                                '1'.
*  PERFORM bdc_field       USING 'ESLL-MEINS(01)'
*                                'au'.
*  PERFORM bdc_field       USING 'ESLL-TBTWR(01)'
*                                lv_famt .          "ZTP_HDR-ZF_AMT.
*  PERFORM bdc_field       USING 'RM11P-KOSTL(01)'
*                                '2302112001'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ESKN-SAKTO(01)'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'VRTKZ1'
*                                'X'.
*  PERFORM bdc_field       USING 'ESKN-KOSTL(01)'
*                                '2302112001'.
*  PERFORM bdc_field       USING 'ESKN-SAKTO(01)'
*                                '605010'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ESKN-SAKTO(01)'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=BACK'.
*  PERFORM bdc_field       USING 'VRTKZ1'
*                                'X'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ESSR-TXZ01'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=ACCP'.
*  PERFORM bdc_field       USING 'ESSR-TXZ01'
*                                 lv_text1.
*  PERFORM bdc_field       USING 'ESSR-BLDAT'
*                                 lv_datum1.
*  PERFORM bdc_field       USING 'ESSR-BUDAT'
*                                 lv_datum1.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=SAVE'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RM11P-NEW_ROW'.
*  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                '10'.
*  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=YES'.
*
*  CALL TRANSACTION 'ML81N' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_message.
*
*  IF lt_message IS NOT INITIAL.
*    CLEAR ls_message.
*    LOOP AT lt_message INTO  ls_message.
*      CALL FUNCTION 'FORMAT_MESSAGE'
*        EXPORTING
*          id        = ls_message-msgid
**         LANG      = '-D'
*          no        = ls_message-msgnr
*          v1        = sy-msgv1
*          v2        = sy-msgv2
*          v3        = sy-msgv3
*          v4        = sy-msgv4
*        IMPORTING
*          msg       = v_msg
*        EXCEPTIONS
*          not_found = 1
*          OTHERS    = 2.
*      IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
*        IF ls_message-msgtyp = 'E'.
*          ls_bdc-icon   =  c_red.
*          ls_bdc-id     =  ls_message-msgid.
*          ls_bdc-number =  ls_message-msgnr.
*          ls_bdc-message = v_msg.
*          APPEND ls_bdc TO lt_bdc.
*
*        ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .
*
*          lv_strlen =  strlen(  ls_message-msgv1 ).
*          IF  lv_strlen = 10.
*            ls_bdc-icon   =  c_green.
*            lv_ses       = ls_message-msgv1.
*
*            ls_bdc-id     =  ls_message-msgid.
*            ls_bdc-number =  ls_message-msgnr.
*            ls_bdc-message = v_msg.
*            APPEND ls_bdc TO lt_bdc.
*
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*
*  CALL METHOD obj->check_changed_data .

*  IF lv_flag  =  'X' AND lv_ses IS NOT INITIAL .
*
*    ls_ztp_hdr-tplst     =  lv_plant.
*    ls_ztp_hdr-zshtyp    =  zunload-shtyp.
*    ls_ztp_hdr-ebeln     =  lv_ebeln.
*    ls_ztp_hdr-zdate     =  ztrpq-zvalidf.
*    ls_ztp_hdr-lifnr     =  lfa1-lifnr.
*
*    ls_ztp_hdr-zfdate     =  lfa1-lifnr.
*    ls_ztp_hdr-ztdate     =  lfa1-lifnr.
*
*    ls_ztp_hdr-eqtyp      =  zunload-shtyp.
*
**    ls_ztp_hdr-zt_adn    =  vbrp-skfbp.
**    ls_ztp_hdr-zt_ded    =  vbrp-wavwr.
**    ls_ztp_hdr-zt_tamt   =  vbrp-kzwi1.
**    ls_ztp_hdr-zd_adn    =  vbrp-kzwi4.
**    ls_ztp_hdr-zd_ded    =  vbrp-kzwi5.
**    ls_ztp_hdr-zd_tamt   =  vbrp-kzwi6.
**    ls_ztp_hdr-zf_amt    =  ztp_hdr-zf_amt.
**    ls_ztp_hdr-lblni     =  lv_ses.
*    APPEND ls_ztp_hdr TO lt_ztp_hdr.
*
*    MODIFY ztp_hdr FROM TABLE lt_ztp_hdr.
*
*    LOOP AT lt_final INTO ls_final WHERE chk = 'X'.
*      ls_ztp_item-tplst      = lv_plant.
*      ls_ztp_item-shtyp      = zunload-shtyp.
*      ls_ztp_item-ebeln      = lv_ebeln.
*      ls_ztp_item-tknum      = ls_final-shipment_number .
*      ls_ztp_item-vbeln      = ls_final-manifest .
*      ls_ztp_item-kunnr      = ls_final-client_code.
*      ls_ztp_item-transpzone = ls_final-area .
*      ls_ztp_item-zakm       = ls_final-actual_km .
*      ls_ztp_item-zaqty      =  ls_final-actual_qty.
*      ls_ztp_item-zveh       = ls_final-vechile_number.
*      ls_ztp_item-zvehgrp    = ls_final-invnr .
*      ls_ztp_item-ztpamt     = ls_final-calculated_amt.
*      ls_ztp_item-zdrt       = ls_final-zdrt .
*      ls_ztp_item-zcgrdate   = ls_final-zcgrdate .
*      ls_ztp_item-zcgrt      = ls_final-zcgrt .
*      ls_ztp_item-zcgedate   = ls_final-zcgedate.
*      ls_ztp_item-zcget      = ls_final-zcget .
*      ls_ztp_item-zcdt       = ls_final-zcdt.
*      ls_ztp_item-zgtrdate   = ls_final-zgtrdate.
*      ls_ztp_item-zgtrt      = ls_final-zgtrt .
*      ls_ztp_item-zgtoudate  = ls_final-zgtoudate.
*      ls_ztp_item-zgtout     = ls_final-zgtout .
*      ls_ztp_item-zodt       = ls_final-zodt .
*      APPEND ls_ztp_item TO lt_ztp_item.
*      CLEAR ls_ztp_item.
*    ENDLOOP.
*    MODIFY ztp_item FROM TABLE lt_ztp_item.
*  ENDIF.


  CLEAR ls_fieldcat.
  REFRESH lt_fieldcat.
  ls_fieldcat-fieldname = 'ICON'.
  ls_fieldcat-seltext_m = 'LIGHT' .
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ID'.
  ls_fieldcat-seltext_m = 'ID' .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NUMBER'.
  ls_fieldcat-seltext_m = 'NUMBER' .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-seltext_l = 'MESSAGE' .
  ls_fieldcat-outputlen = 100.
  APPEND ls_fieldcat TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = lt_bdc
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here

  ENDIF.
CALL SCREEN 9002. " BY HS 07.12.2019
     ENDIF.
  ELSE.
    CONCATENATE 'For This Transporter' lfa1-lifnr 'Po Already Created'
    INTO lv_mesg SEPARATED BY ' '.
    MESSAGE lv_mesg  TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_DYNPRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM bdc_dynpro  USING program dynpro.
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM bdc_field  USING   fnam fval.
  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fnam.
  ls_bdcdata-fval = fval.
  APPEND  ls_bdcdata TO  lt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PO_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_po_bdc .
data:lv_tcode TYPE sy-tcode VALUE 'ZONLINE_PO'.
data : zmaterial_group TYPE T023-MATKL.
DATA : LV_PAYMENT_TERMS(4) TYPE C,
       LV_PI(2) TYPE C.
SELECT SINGLE ZVALUE INTO LV_PAYMENT_TERMS FROM ZPARAMAST  WHERE ZTYPE = 'ZML81N_PAY_TERMS' AND ZSALORG = LV_PLANT AND zstatus = 'T'.


IF  zunload-shtyp = '0010'.
    lv_sakto = '605010' .
  ELSEIF zunload-shtyp = '0001'.
    lv_sakto =  '605030'.
  ENDIF.

"DATA : LS_ZPI_VENDOR TYPE ZPI_VENDOR.

CLEAR : LS_ZPI_VENDOR , LS_ZTP_TAXCODE, ZTAX_CODE.

SELECT SINGLE * FROM ZTP_TAXCODE INTO LS_ZTP_TAXCODE
    WHERE TPLST = LV_PLANT
      AND LIFNR = lfa1-lifnr
      AND SHTYP = ZUNLOAD-SHTYP
      AND ZEQTYP = EQUI-EQTYP.

 IF LS_ZTP_TAXCODE IS NOT INITIAL.
    ZTAX_CODE = LS_ZTP_TAXCODE-MWSKZ.
    ELSE.
      ZTAX_CODE = 'Z0'.
   ENDIF.

SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
      WHERE WERKS = LV_PLANT
      AND BSART = 'ZTSP'
      AND ZFDATE LE s_date-low
      AND ZTDATE GE s_date-high
      AND act_lifnr = lfa1-lifnr
      AND ZSTATUS = 'T'.
  IF SY-subrc <> 0 .
    SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
      WHERE WERKS = LV_PLANT
      AND BSART = 'ZTSP'
      AND ZFDATE LE s_date-low
      AND ZTDATE GE s_date-high
      AND ( act_lifnr IS NULL OR act_lifnr = '' )
      AND ZSTATUS = 'T'.
    ENDIF.

IF LS_ZPI_VENDOR-lifnr IS INITIAL.
    LV_PI = ''.
    ELSE.
    LV_PI = 'PI'.
ENDIF.

CLEAR : zmaterial_group.

IF ZUNLOAD-SHTYP = '0010' AND EQUI-EQTYP = 'D'.
    zmaterial_group = 'S30201210'.
    ELSEIF ZUNLOAD-SHTYP = '0010' AND EQUI-EQTYP = 'N'.
    zmaterial_group = 'S30201510'.
  ELSEIF ZUNLOAD-SHTYP = '0001' AND EQUI-EQTYP = 'D'.
    zmaterial_group = 'S30301110'.
    ELSEIF ZUNLOAD-SHTYP = '0001' AND EQUI-EQTYP = 'N'.
    zmaterial_group = 'S30301310'.
    ELSE.
    zmaterial_group = 'S30'.
ENDIF.


CONCATENATE LV_PLANT 'XXXXXX' INTO zcost_center. " BY HS 15.03.2021

  REFRESH :lt_bdcdata.
  SELECT SINGLE bukrs FROM t001k INTO lv_bukrs WHERE bwkey = lv_plant.
  DATA : vpodate TYPE CHAR10.

  IF LV_PAYMENT_TERMS IS INITIAL.
    SELECT SINGLE ZTERM INTO LV_PAYMENT_TERMS FROM LFB1 WHERE LIFNR = lfa1-lifnr AND BUKRS = lv_bukrs.
  ENDIF.

CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
  EXPORTING
    INPUT         = sy-datum
 IMPORTING
   OUTPUT        = vpodate.


  IF lfa1-lifnr IS NOT INITIAL AND lv_datum1 IS NOT INITIAL AND lv_bukrs IS NOT INITIAL
      AND  lv_plant IS NOT INITIAL .
    export lv_tcode FROM lv_tcode to MEMORY id 'ZONLINE'.

perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'MEPO_TOPLINE-BSART'.
perform bdc_field       using 'MEPO_TOPLINE-BSART'
                              'ZTSP'.
perform bdc_field       using 'DYN_6000-LIST'
                              '   1'.
perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
perform bdc_field       using 'BDC_OKCODE'
                              '=TABHDT1'.
perform bdc_field       using 'MEPO_TOPLINE-BSART'
                              'ZTSP'.
perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
                              lfa1-lifnr.
perform bdc_field       using 'BDC_CURSOR'
                              'MEPO1222-BUKRS'.
perform bdc_field       using 'MEPO1222-EKORG'
                              '1000'.
perform bdc_field       using 'MEPO1222-EKGRP'
                              '100'.
perform bdc_field       using 'MEPO1222-BUKRS'
                              lv_bukrs.
perform bdc_field       using 'DYN_6000-LIST'
                              '   1'.
perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
perform bdc_field       using 'BDC_OKCODE'
                              '=TABHDT6'.
perform bdc_field       using 'MEPO_TOPLINE-BSART'
                              'ZTSP'.
perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
                              lfa1-lifnr.
perform bdc_field       using 'BDC_CURSOR'
                              'MEPO1226-ZTERM'.
perform bdc_field       using 'MEPO1226-ZTERM'
                              LV_PAYMENT_TERMS. "PAYMENT TERM
perform bdc_field       using 'DYN_6000-LIST'
                              '   1'.
perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MEV4001BUTTON'.
perform bdc_field       using 'MEPO_TOPLINE-BSART'
                              'ZTSP'.
perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
                              lfa1-lifnr.
perform bdc_field       using 'BDC_CURSOR'
                              'WRF02K-GPARN(01)'.
perform bdc_field       using 'MMPA-PARVW(01)'
                              LV_PI.
perform bdc_field       using 'WRF02K-GPARN(01)'
                              LS_ZPI_VENDOR-LIFNR.
perform bdc_field       using 'DYN_6000-LIST'
                              '   1'.
perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'MEPO_TOPLINE-BSART'
                              'ZTSP'.
perform bdc_field       using 'MEPO_TOPLINE-SUPERFIELD'
                              lfa1-lifnr.
PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MEPO1211-NAME1(01)'.
    PERFORM bdc_field       USING 'MEPO1211-KNTTP(01)'
                                  'K'.
    PERFORM bdc_field       USING 'MEPO1211-EPSTP(01)'
                                  'D'.
    PERFORM bdc_field       USING 'MEPO1211-TXZ01(01)'
                                  'Transportation Service'.
    PERFORM bdc_field       USING 'MEPO1211-MENGE(01)'
                                  '                1'.
    PERFORM bdc_field       USING 'MEPO1211-WGBEZ(01)'
                                  zmaterial_group.
                                  "'S20202113'.

    PERFORM bdc_field       USING 'MEPO1211-NAME1(01)'
                                  lv_plant.
    PERFORM bdc_field       USING 'DYN_6000-LIST'
                                  '   1'.
    PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
                                  'ZTSP'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-SUPERFIELD'
*                                '1014034 D K Transport'.
*  PERFORM bdc_field       USING 'MEPO_TOPLINE-BEDAT'
*                                '01.01.2018'.

   "Hiren Surati 03-08-2018 Start
   "Commeted By Kalpesh Pawar 21-11-2018 Start
*    PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                  '   1'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'ESLL-TBTWR(01)'.
*    PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                  '10'.
*    PERFORM bdc_field       USING 'ESLL-KTEXT1(01)'
*                                  'Transportation Service'.
*    PERFORM bdc_field       USING 'ESLL-MENGE(01)'
*                                  '1'.
*    PERFORM bdc_field       USING 'ESLL-MEINS(01)'
*                                  'au'.
*    PERFORM bdc_field       USING 'ESLL-TBTWR(01)'
*                                  '100'.
*    PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
   "Commeted By Kalpesh Pawar 21-11-2018 End
    PERFORM bdc_field       USING 'BDC_OKCODE'
*{   REPLACE        SNDK900056                                        1
*\                                  '=TABIDT2'.
                                  '=TABIDT3'.  " BY HS 09.03.2022
*}   REPLACE
    PERFORM bdc_field       USING 'DYN_6000-LIST'
                                 '   1'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'ESLL-TBTWR(01)'.
*    PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                  '10'.
    PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=TABIDT7'.
    PERFORM bdc_field       USING 'DYN_6000-LIST'
                                  '   1'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'ESUH-COMMITMENT'.
    PERFORM bdc_field       USING 'ESUH-SUMNOLIM'
                                  'X'.
    PERFORM bdc_field       USING 'ESUH-COMMITMENT'
                                  '100'.

    "BY HS 15.03.2021 S
    perform bdc_dynpro      using 'SAPLMLSK' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'ESKN-SAKTO(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'VRTKZ3'
                              'X'.
perform bdc_field       using 'RM11K-MKNTM(01)'
                              '100'.
perform bdc_field       using 'ESKN-KOSTL(01)'
                              zcost_center. "'2302XXXXXX'.
*perform bdc_field       using 'ESKN-SAKTO(01)'
*                              lv_sakto.

 "BY HS 15.03.2021 E



    PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'DYN_6000-LIST'
                                  '   1'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MEPO1317-MWSKZ'.
    PERFORM bdc_field       USING 'MEPO1317-MWSKZ'
                                  "'Z0'. BY HS 02.12.2019
                                  ZTAX_CODE. "BY HS 02.12.2019
    PERFORM bdc_field       USING 'MEPO1317-WEBRE'
                                  'X'.
    PERFORM bdc_field       USING 'MEPO1317-LEBRE'
                                  'X'.
    PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=MESAVE'.
    PERFORM bdc_field       USING 'DYN_6000-LIST'
                                  '   1'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MEPO1317-MWSKZ'.
    PERFORM bdc_field       USING 'MEPO1317-MWSKZ'
                                  "'Z0'."BY HS 02.12.2019
                                  ZTAX_CODE. "BY HS 02.12.2019
    PERFORM bdc_field       USING 'MEPO1317-WEBRE'
                                  'X'.
    PERFORM bdc_field       USING 'MEPO1317-LEBRE'
                                  'X'.
    PERFORM bdc_dynpro      USING 'SAPLSPO2' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=OPT1'.

       "Hiren Surati 03-08-2018 END

    CALL TRANSACTION 'ME21N' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_message.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_SER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_ser .
  IF  p_tr = 'X'.
    PERFORM create_bdc_tr.
    PERFORM display_alv.
  ELSEIF p_de =  'X'.
    PERFORM create_de_bdc1.
    PERFORM display_alv.

  ELSEIF P_COM = 'X'.
    PERFORM create_com_se_bdc.
    PERFORM display_alv.
*  ELSEIF p_both =  'X'.
*    PERFORM create_se_bdc.
*    PERFORM display_alv.
  ENDIF.

*  IF lv_ses IS NOT INITIAL .
*
*
*  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DE_BDC1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_de_bdc1 .
  REFRESH :lt_rows,lt_bdc.
  CALL METHOD obj->check_changed_data .

***************************************
  CALL METHOD obj->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  CLEAR : ls_rows,ls_final.


  IF  zunload-shtyp = '0010'.
    "lv_sakto = '605020' .
    lv_sakto = '605025'. "New Detention Given By Baheti sir
  ELSEIF zunload-shtyp = '0001'.
    lv_sakto =  '605040'.
  ENDIF.


CLEAR : LS_ZPI_VENDOR,zcommission_charges, LS_ACC_GRP.

" BY HIREN SURATI stopp commison for commitment "07.10.2022
*SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
*      WHERE WERKS = LV_PLANT
*      AND BSART = 'ZTSP'
*      AND ZFDATE LE s_date-low
*      AND ZTDATE GE s_date-high
*      AND act_lifnr = lfa1-lifnr
*      AND ZSTATUS = 'T'.
*IF SY-subrc <> 0.
*    SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
*      WHERE WERKS = LV_PLANT
*      AND BSART = 'ZTSP'
*      AND ZFDATE LE s_date-low
*      AND ZTDATE GE s_date-high
*      AND ( act_lifnr IS NULL OR act_lifnr = '' )
*      AND KBETR > 0
*      AND ZSTATUS = 'T'.
*  ENDIF.


  REFRESH : lt_bdcdata.

  CLEAR : LT_ZTSP_MAPPING.
  SELECT * INTO TABLE LT_ZTSP_MAPPING FROM ZTSP_MAPPING
    WHERE ZSHTYP = zunload-shtyp
        AND ZEQTYP = EQUI-EQTYP
        AND ZTP_TYPE = 'D'.

  IF lt_rows IS NOT INITIAL .
    LOOP AT lt_rows INTO ls_rows .
      READ TABLE  lt_final INTO ls_final INDEX ls_rows-index.


      CLEAR ls_item.
      SELECT SINGLE * FROM ztp_item INTO ls_item WHERE tplst =  lv_plant
                                          AND   shtyp =  zunload-shtyp
                                         " AND   ebeln =  ekko-ebeln  Commented By Hiren Surati 06.08.2018
                                          AND   vbeln =  ls_final-manifest.

    IF ls_final-lblni = 'COMMITMENT'.
            MESSAGE 'Commitment Entry Sheet Already Created For this Vehicle of this period ' TYPE 'E' DISPLAY LIKE 'I'.
            EXIT.
           ENDIF.

      IF ls_item-zlblni IS INITIAL .

        CLEAR : lv_inward_y,lv_inward_m,lv_inward_d.
        lv_inward_y  =  ls_final-inward_date+0(4).
        lv_inward_m  =  ls_final-inward_date+4(2).
        lv_inward_d =   ls_final-inward_date+6(2) .
        CONCATENATE  lv_inward_d '.' lv_inward_m '.' lv_inward_y  INTO lv_inward.

          IF zunload-shtyp = '0010'.
            lv_postdate_y  =  ls_final-ZWBROUDATE+0(4).
            lv_postdate_m  =  ls_final-ZWBROUDATE+4(2).
            lv_postdate_d =   ls_final-ZWBROUDATE+6(2) .
            CONCATENATE  lv_postdate_d '.' lv_postdate_m '.' lv_postdate_y  INTO lv_postdate.
            ELSE.
            lv_postdate = lv_inward.
         ENDIF.



        IF ls_final-d_tot_amt IS NOT INITIAL .
          CLEAR :  lv_deamt.
          lv_deamt   =  ls_final-d_tot_amt.
          CONDENSE : lv_newamt ,lv_deamt.

          CLEAR : zphy_status ,ls_ZVTRPQ_CC,zcost_center.

IF ( lv_plant = '2302' OR lv_plant = '1006' OR lv_plant = '1402' OR lv_plant = '1203' OR lv_plant = '2304' OR lv_plant = '1206' OR lv_plant = '1207'   ).
         CALL FUNCTION 'ZSD_GETMANIFEST_PHYSTATUS'
           EXPORTING
             ZMANIFEST_NO         = LS_FINAL-MANIFEST
             ZSHIPMENT_TYPE       = zunload-shtyp
          IMPORTING
            ZPHY_STATUS          = zphy_status .
ENDIF.

          "Commented By Hiren Surati 07-01-2018 Cost Center start
*          SELECT SINGLE * FROM zvtrp_cc INTO ls_zvtrp_cc
*                          WHERE werks = lv_plant
*                           AND  spart = ls_final-spart
*                           AND  shtyp = zunload-shtyp.
          "Commented By Hiren Surati 07-01-2018 Cost Center END
*          SELECT SINGLE * FROM ZVTRPQ_CC INTO ls_ZVTRPQ_CC
*                          WHERE werks = lv_plant
*                           AND  shtyp = zunload-shtyp
*                           AND KURZTEXT = zphy_status.

IF ( lv_plant = '2302' OR lv_plant = '1006' OR lv_plant = '1402' OR lv_plant = '1203'  OR lv_plant = '2304'  OR lv_plant = '1206'  OR lv_plant = '1207'   ) AND  zphy_status IS NOT INITIAL .
        SELECT SINGLE * FROM ZVTRPQ_CC INTO ls_ZVTRPQ_CC
                          WHERE werks = lv_plant
                           AND  shtyp = zunload-shtyp
                           AND KURZTEXT = zphy_status.
          zcost_center = ls_ZVTRPQ_CC-KOSTL.

          ELSE.
            SELECT SINGLE * FROM zvtrp_cc INTO ls_zvtrp_cc
                            WHERE werks = lv_plant
                             AND  spart = ls_final-spart
                             AND  shtyp = zunload-shtyp.
              zcost_center = ls_zvtrp_cc-KOSTL.
 ENDIF.




 "Logic Add by Hiren To Post in Given Month 22-06-2018 Start
         DATA zparapostdate TYPE D.
          DATA zmanifestdate TYPE D.
         DATA zvalidupto(10) TYPE C.

         DATA zfinaldate(10) TYPE C.

         concatenate lv_postdate+6(4) lv_postdate+3(2) lv_postdate+0(2) into zmanifestdate.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
          day_in = zmanifestdate
          IMPORTING
          last_day_of_month = zparapostdate.

          WRITE zparapostdate to zvalidupto DD/MM/YYYY.

          SELECT single ZVALUE INTO zfinaldate FROM ZPARAMAST
             WHERE zplan = LV_PLANT
             AND ZTYPE = 'DETENTION_PERIOD'
             AND ZVALID_UPTO = zvalidupto
             AND ZSTATUS = 'T'.

         IF zfinaldate is not INITIAL.
           lv_postdate = zfinaldate.
         ENDIF.
    "Logic End

      DATA: zveh_type(2) TYPE C.
      DATA: zlength TYPE I.
      CLEAR : LS_ZTSP_MAPPING , zveh_type ,zlength .

      zlength = STRLEN( LS_FINAL-INVNR ).
      zlength = zlength - 2.
      zveh_type =   LS_FINAL-INVNR+zlength(2).


      READ TABLE LT_ZTSP_MAPPING INTO LS_ZTSP_MAPPING WITH KEY ZVEHTYP = zveh_type.
      "End 12.03.2019

      IF LS_ZTSP_MAPPING IS INITIAL.
        MESSAGE 'Service Code is not maintained for this vehicle type..!!' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.

        IF LS_ZPI_VENDOR-ASNUM IS NOT INITIAL AND ls_final-inward_date >= '20190601'.
          CLEAR : zcommission_charges_amt.
          zcommission_charges_amt = ( LS_ZPI_VENDOR-KBETR * lv_deamt ) / 100.
          ZCOMMISSION_CHARGES =  zcommission_charges_amt.
          ENDIF.


data:lv_tcode TYPE sy-tcode VALUE 'ZONLINE_PO'.
 export lv_tcode FROM lv_tcode to MEMORY id 'ZONLINE'.

CLEAR : LV_ACCPT.
FREE MEMORY ID 'SYSTEMSES'.
DATA:lv_tcode_t TYPE sy-tcode VALUE 'AUTOSES'.
export lv_tcode_t FROM lv_tcode_t to MEMORY id 'SYSTEMSES'. " 03.01.2018
LV_ACCPT = 'T'.


"BY HS 16.03.2021 CHECK ACC ASSIGNMENT GRP S
SELECT SINGLE * INTO @DATA(LS_EKPO) FROM EKPO WHERE EBELN =  @ekko-ebeln.
  LS_ACC_GRP = LS_EKPO-KNTTP.
"BY HS 16.03.2021 End


          "IF ls_zvtrp_cc-kostl IS NOT INITIAL .
          IF zcost_center IS NOT INITIAL .

             PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=SELP'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0340'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11R-EBELN'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=ENTE'.
              PERFORM bdc_field       USING 'RM11R-EBELN'
                                             ekko-ebeln.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=NEU'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                            "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                            LS_FINAL-MANIFEST . "Added By hiren Surati 11-08-2018
              IF LS_ACC_GRP <> 'K'. " BY HS 16.03.2021
              PERFORM bdc_field       USING 'ESSR-KNTTP'
                                            'K'.
              ENDIF.
              PERFORM bdc_field       USING 'ESSR-LBLDT'
                                            lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.

              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                         LS_FINAL-MANIFEST.

              PERFORM bdc_field       USING 'ESSR-BKTXT'   "Added By Hiren Surati on 18-07-2018
                                        transporttext.

              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST . "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                             'ESSR-LBLNE'.
              IF LS_ACC_GRP <> 'K'. " BY HS 16.03.2021
              PERFORM bdc_field       USING 'ESSR-KNTTP'
                                            'K'.
              ENDIF.
              PERFORM bdc_field       USING 'ESSR-LBLDT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-KOSTL(01)'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.

               PERFORM bdc_field       USING 'ESLL-SRVPOS(01)'
                                            LS_ZTSP_MAPPING-ASNUM..

              PERFORM bdc_field       USING 'ESLL-MENGE(01)'
                                            '1'.

              PERFORM bdc_field       USING 'ESLL-TBTWR(01)'
                                            lv_deamt.
              PERFORM bdc_field       USING 'RM11P-KOSTL(01)'

                                            zcost_center.



              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.

              PERFORM bdc_field       USING 'ESKN-KOSTL(01)'

                                            zcost_center.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=BACK'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.


""""""""""2nd Line For Gobolt Start
IF LS_ZPI_VENDOR-KBETR > 0 AND ls_final-inward_date >= '20190601'..
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST. "Added By hiren Surati 11-08-2018
*              PERFORM bdc_field       USING 'ESSR-KNTTP'
*                                            'K'.
*              PERFORM bdc_field       USING 'ESSR-LBLDT'
*                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-KOSTL(02)'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '20'.

               PERFORM bdc_field       USING 'ESLL-SRVPOS(02)'
                                            "LS_ZTSP_MAPPING-ASNUM.
                                            LS_ZPI_VENDOR-ASNUM..

               "HS Pawar
*              PERFORM bdc_field       USING 'ESLL-KTEXT1(01)'
*                                            "'Transport Expense'." Commeted By Hiren Surati 11-08-2018
*                                        transporttext. "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'ESLL-MENGE(02)'
                                            '1'.
              "Commed By H.S 12.03.2019 start
*              PERFORM bdc_field       USING 'ESLL-MEINS(01)'
*                                            'au'.

              "End
              PERFORM bdc_field       USING 'ESLL-TBTWR(02)'
                                            ZCOMMISSION_CHARGES.
              PERFORM bdc_field       USING 'RM11P-KOSTL(02)'
                                            "ls_zvtrp_cc-kostl.
                                            zcost_center.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.
              PERFORM bdc_field       USING 'ESKN-KOSTL(01)'
                                            "ls_zvtrp_cc-kostl.
                                            zcost_center.

*              PERFORM bdc_field       USING 'ESKN-SAKTO(01)' bY hs 08.06.2019
*                                            lv_sakto.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=BACK'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.


              """""108.03.2022
perform bdc_dynpro      using 'SAPLMLSK' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM11K-MKNTM(02)'.
perform bdc_field       using 'ESKN-KOSTL(01)'
                              zcost_center. "//////////



perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'VRTKZ1'
                              'X'.

ENDIF.
""""""""""""""'2nd Line For Gobolt End


              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESSR-TXZ01'.


              IF LV_ACCPT = 'T'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=ACCP'.
              ENDIF.





              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST. "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                            lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                            lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.

""""BY HS 08.03.2022

perform bdc_dynpro      using 'SAPLMLSK' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM11K-MKNTM(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'VRTKZ1'
                              'X'.

"""""



              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.

              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=SAVE'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=YES'.

            CALL TRANSACTION 'ML81N' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_message.
            WAIT UP TO 1 SECONDS .
            IF lt_message IS NOT INITIAL.
              DELETE lt_message WHERE msgtyp = 'W'.
              DELETE lt_message WHERE msgtyp = 'I'.
              CLEAR ls_message.
              LOOP AT lt_message INTO  ls_message.
                CALL FUNCTION 'FORMAT_MESSAGE'
                  EXPORTING
                    id        = ls_message-msgid
*                   LANG      = '-D'
                    no        = ls_message-msgnr
                    v1        = sy-msgv1
                    v2        = sy-msgv2
                    v3        = sy-msgv3
                    v4        = sy-msgv4
                  IMPORTING
                    msg       = v_msg
                  EXCEPTIONS
                    not_found = 1
                    OTHERS    = 2.
                IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
                  IF ls_message-msgtyp = 'E'.
                    ls_bdc-icon   =  c_red.
                    ls_bdc-id     =  ls_message-msgid.
                    ls_bdc-number =  ls_message-msgnr.
                    ls_bdc-message = v_msg.
                    APPEND ls_bdc TO lt_bdc.

                  ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .

                    lv_strlen =  strlen(  ls_message-msgv1 ).
                    IF  lv_strlen = 10 AND ls_message-msgv1 <> 'ESLL-TBTWR'.
                      ls_bdc-icon   =  c_green.
                      lv_ses       = ls_message-msgv1.
                      PERFORM : save_ztable.
                      ls_bdc-id     =  ls_message-msgid.
                      ls_bdc-number =  ls_message-msgnr.
                      ls_bdc-message = v_msg.
                      ls_bdc-text    = 'Detentation Expence'.
                      ls_bdc-mani    = ls_final-manifest.
                      APPEND ls_bdc TO lt_bdc.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

            ENDIF.

            REFRESH :lt_message.
            CLEAR : ls_final, ls_zvtrp_cc , ls_ZVTRPQ_CC .
          ELSE.
            MESSAGE 'Cost Center Missing' TYPE 'E'.
          ENDIF.
        ELSE .
          MESSAGE 'Please Maintain The Total Detention Amont' TYPE 'E'.
        ENDIF.
        REFRESH lt_bdcdata .
        CLEAR : ls_final,ls_rows.
      ELSE.
        CONCATENATE 'Line Item' ls_rows-index 'Service Entry Sheet' ls_final-lblni 'Exsit'
     INTO lv_service SEPARATED BY ' '.
        MESSAGE lv_service TYPE 'E'.

      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE 'Please select line item and create' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_BDC_TR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_bdc_tr .


  REFRESH lt_bdcdata .

  REFRESH: lt_rows,lt_bdc.
  CALL METHOD obj->check_changed_data .

***************************************
  CALL METHOD obj->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.



  IF  zunload-shtyp = '0010'.
    lv_sakto = '605010' .
  ELSEIF zunload-shtyp = '0001'.
    lv_sakto =  '605030'.
  ENDIF.


CLEAR : LS_ZPI_VENDOR,zcommission_charges , LS_ACC_GRP.

SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
      WHERE WERKS = LV_PLANT
      AND BSART = 'ZTSP'
      AND ZFDATE LE s_date-low
      AND ZTDATE GE s_date-high
      AND act_lifnr = lfa1-lifnr
      AND ZSTATUS = 'T'.
IF SY-subrc <> 0.
  SELECT SINGLE * FROM ZPI_VENDOR INTO LS_ZPI_VENDOR
      WHERE WERKS = LV_PLANT
      AND BSART = 'ZTSP'
      AND ZFDATE LE s_date-low
      AND ZTDATE GE s_date-high
      AND KBETR > 0
      AND ( act_lifnr IS NULL OR act_lifnr = '' )
      AND ZSTATUS = 'T'.
  ENDIF.


  CLEAR : LT_ZTSP_MAPPING.
  SELECT * INTO TABLE LT_ZTSP_MAPPING FROM ZTSP_MAPPING
    WHERE ZSHTYP = zunload-shtyp
        AND ZEQTYP = EQUI-EQTYP
        AND ZTP_TYPE = 'T'.

  CLEAR : ls_rows,ls_final.
  IF lt_rows IS NOT INITIAL .
    LOOP AT lt_rows INTO ls_rows .
      READ TABLE  lt_final INTO ls_final INDEX ls_rows-index.

      IF ls_final-ADD_AMT > 0 AND ( ls_final-ZTRP_REMARK IS INITIAL OR ls_final-ZTRP_REMARK = '').
          MESSAGE 'Please provide remark for other add amount.' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.

        IF ls_final-zdevkm > 0 AND ( ls_final-ZDEVKM_REMARK IS INITIAL OR ls_final-ZDEVKM_REMARK = '').
          MESSAGE 'Please provide remark for Deviation KM.' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.

      CLEAR ls_item.
      SELECT SINGLE * FROM ztp_item INTO ls_item WHERE tplst =  lv_plant
                                          AND   shtyp =  zunload-shtyp
                                          AND TKNUM = ls_final-SHIPMENT_NUMBER.
                                          "AND   ebeln =  ekko-ebeln  Commented By Hiren Surati on 06.08.2018
                                          "AND   vbeln =  ls_final-manifest. BY HS 02.10.2020

      IF ls_final-lblni = 'COMMITMENT'.
        MESSAGE 'Commitment Entry Sheet Already Created For this Vehicle of this period ' TYPE 'E' DISPLAY LIKE 'I'.
        EXIT.
       ENDIF.

CLEAR : LV_MULTI_FLAG.

SELECT COUNT(*) FROM ZTRPQ WHERE ZCON = ls_final-zcon AND z_mpqty > 0 .
    IF SY-subrc = 0.
            LV_MULTI_FLAG = 1.
            ENDIF.

    SELECT COUNT(*) FROM ZTRPQ_MULTI WHERE ZCON = ls_final-zcon.
          IF SY-subrc = 0.
            LV_MULTI_FLAG = 1.
            ENDIF.


      IF ls_item-lblni IS INITIAL .

        IF ls_final-inward_date IS NOT INITIAL .
          CLEAR : lv_inward_y,lv_inward_m,lv_inward_d , lv_postdate_y , lv_postdate_m , lv_postdate_d , lv_inward , lv_postdate  .
          lv_inward_y  =  ls_final-inward_date+0(4).
          lv_inward_m  =  ls_final-inward_date+4(2).
          lv_inward_d =   ls_final-inward_date+6(2) .
          CONCATENATE  lv_inward_d '.' lv_inward_m '.' lv_inward_y  INTO lv_inward.

          IF zunload-shtyp = '0010'.
            lv_postdate_y  =  ls_final-ZWBROUDATE+0(4).
            lv_postdate_m  =  ls_final-ZWBROUDATE+4(2).
            lv_postdate_d =   ls_final-ZWBROUDATE+6(2) .
            CONCATENATE  lv_postdate_d '.' lv_postdate_m '.' lv_postdate_y  INTO lv_postdate.
            ELSE.
            lv_postdate = lv_inward.
           ENDIF.

          IF  ls_final-net_amt IS NOT INITIAL .
            CLEAR :  lv_newamt.


            DATA : ZTOT_MST_KM TYPE NETWR.
            DATA : ZTOT_COMM_KM TYPE NETWR.
            DATA : ZFINAL_RATE TYPE NETWR.
            DATA : ZCAL_BASE TYPE ZCALBASE1.
            DATA : ZFLAG     TYPE I.
            DATA :ZTRP_RATE TYPE NETWR,
                 ZMIN_RATE TYPE NETWR,
                 ZPENDING_KM TYPE NETWR.


            lv_newamt   =  ls_final-net_amt.


            IF LS_FINAL-ZCALBASE_CODE = 'CB06'.
              CALL FUNCTION 'ZTRPQ_GET_TRANSPORTRATE_KM1'
              EXPORTING
                ZTRANSPORTER       = LS_FINAL-TRANSPORTER
                ZVEHICLEGRP        = LS_FINAL-INVNR
                ZPONUMBER          = EKKO-EBELN
                ZEQTYP             = EQUI-EQTYP
                ZPLANT             = LV_PLANT

             IMPORTING
               ZMSTOTKM           = ZTOT_MST_KM
               ZCOMMITKM          = ZTOT_COMM_KM
               ZFINALRATE         = ZFINAL_RATE
               ZCALBASE           = ZCAL_BASE
               ZFLAG              =  ZFLAG.



                lv_newamt  = ( ls_final-LOCCO * ZFINAL_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT .
                LS_FINAL-QUOTATION_AMT = ZFINAL_RATE.
                ls_final-ZTRP_RATE = ZFINAL_RATE.
                ls_final-calculated_amt = ( ls_final-LOCCO * ZFINAL_RATE ).
                ls_final-net_amt = lv_newamt.
            ENDIF.


            IF ( LS_FINAL-ZCALBASE_CODE = 'CB14' OR LS_FINAL-ZCALBASE_CODE = 'CB05' ) AND  LV_MULTI_FLAG = 1.
            IF ls_final-zret_manifest <> 'Return'.

            SELECT SINGLE * INTO @DATA(LS_ZTRPQ_MULTIH) FROM ZTRPQ_MULTI WHERE zcon = @ls_final-zcon.
               IF SY-subrc <> 0.


              CALL FUNCTION 'ZTRPQ_GET_TRANSPORTRATE_KM1'
              EXPORTING
                ZTRANSPORTER       = LS_FINAL-TRANSPORTER
                ZVEHICLEGRP        = LS_FINAL-INVNR
                ZPONUMBER          = EKKO-EBELN
                ZEQTYP             = EQUI-EQTYP
                ZPLANT             = LV_PLANT
                ZCALCBASE          = LS_FINAL-ZCALBASE_CODE
                ZVEHPRIMARY        = LS_FINAL-ZVEH_PRIMARY
             IMPORTING
               ZMSTOTKM           = ZTOT_MST_KM
               ZCOMMITKM          = ZTOT_COMM_KM
               ZFINALRATE         = ZFINAL_RATE
               ZCALBASE           = ZCAL_BASE
               ZTRP_RATE          = ZTRP_RATE
               ZMIN_RATE          = ZMIN_RATE
                ZPENDINGKM         = ZPENDING_KM.

                IF ZFLAG = 1 OR ZFLAG = 3 .
                    lv_newamt  = ( ls_final-zses_km * ZMIN_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT .
                    LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ls_final-zses_km * ZMIN_RATE ).
                    ls_final-net_amt = lv_newamt.
                ENDIF.

                IF ZFLAG = 0.

                   IF ZPENDING_KM =  ls_final-zses_km.
                      lv_newamt  = ( ls_final-zses_km * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-zses_km * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF ZPENDING_KM >  ls_final-zses_km.
                      lv_newamt  = ( ls_final-zses_km * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT .
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-zses_km * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF  ls_final-zses_km > ZPENDING_KM .
                   DATA ZPENDING_KM_S TYPE STRING.

                   lv_newamt  = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-zses_km - ZPENDING_KM ) * ZMIN_RATE ) ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                   LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-zses_km - ZPENDING_KM ) * ZMIN_RATE ) ).
                    ls_final-net_amt = lv_newamt.

                    ZPENDING_KM_S = ZPENDING_KM.
                 CONCATENATE 'Pending KM-' ZPENDING_KM_S INTO ls_final-ZTRP_REMARK.
                   ENDIF.


                  ENDIF.

                ELSE.
              "27.11.2020 start
              CLEAR : pZTRANS_AMT, pZTRANS_RATE , pZTRANS_REMARK, pACT_KM .
               pACT_KM = ls_final-zses_km.
              CALL FUNCTION 'ZTRPQ_GET_MULTI_RATE_FM'
                EXPORTING
                  zcon                =  ls_final-zcon
                  zmanikm             = pACT_KM
                  zcalcbase           = LS_FINAL-ZCALBASE_CODE
                  zponumber           = EKKO-EBELN
                  ptotalkm            = 0
                  ZVEHPRIMARY        = LS_FINAL-ZVEH_PRIMARY
               IMPORTING
                 ztrans_amt      = pZTRANS_AMT
                 ztrans_rate     = pZTRANS_RATE
                 ztrans_remark   = pZTRANS_REMARK
                 .
                lv_newamt =  pZTRANS_AMT + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                LS_FINAL-QUOTATION_AMT = pZTRANS_RATE.
                ls_final-ZTRP_RATE = pZTRANS_RATE.
                ls_final-calculated_amt = pZTRANS_AMT.
                ls_final-net_amt = lv_newamt.
                ls_final-ZTRP_REMARK = pZTRANS_REMARK.


                ENDIF.

                "27.11.2020 End


                  ENDIF.
            ENDIF.




            IF LS_FINAL-ZCALBASE_CODE = 'CB17'.
              CALL FUNCTION 'ZTRPQ_GET_TRANSPORTRATE_KM1'
              EXPORTING
                ZTRANSPORTER       = LS_FINAL-TRANSPORTER
                ZVEHICLEGRP        = LS_FINAL-INVNR
                ZPONUMBER          = EKKO-EBELN
                ZEQTYP             = EQUI-EQTYP
                ZPLANT             = LV_PLANT
                ZCALCBASE          = LS_FINAL-ZCALBASE_CODE
                ZVEHPRIMARY        = LS_FINAL-ZVEH_PRIMARY
             IMPORTING
               ZMSTOTKM           = ZTOT_MST_KM
               ZCOMMITKM          = ZTOT_COMM_KM
               ZFINALRATE         = ZFINAL_RATE
               ZCALBASE           = ZCAL_BASE
               ZTRP_RATE          = ZTRP_RATE
               ZMIN_RATE          = ZMIN_RATE
                ZPENDINGKM         = ZPENDING_KM.

                IF ZFLAG = 1 OR ZFLAG = 3 .
                    lv_newamt  = ( ls_final-ACTUAL_KM * ZMIN_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT .
                    LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZMIN_RATE ).
                    ls_final-net_amt = lv_newamt.
                ENDIF.

                IF ZFLAG = 0.

                   IF ZPENDING_KM =  ls_final-actual_km.
                      lv_newamt  = ( ls_final-ACTUAL_KM * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF ZPENDING_KM >  ls_final-actual_km.
                      lv_newamt  = ( ls_final-ACTUAL_KM * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT .
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF  ls_final-actual_km > ZPENDING_KM .
                   CLEAR: ZPENDING_KM_S .

                   lv_newamt  = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-ACTUAL_KM - ZPENDING_KM ) * ZMIN_RATE ) ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                   LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-ACTUAL_KM - ZPENDING_KM ) * ZMIN_RATE ) ).
                    ls_final-net_amt = lv_newamt.

                    ZPENDING_KM_S = ZPENDING_KM.
                 CONCATENATE 'Pending KM-' ZPENDING_KM_S INTO ls_final-ZTRP_REMARK.
                   ENDIF.


                  ENDIF.

            ENDIF.

            IF LS_FINAL-ZCALBASE_CODE = 'CB15' OR LS_FINAL-ZCALBASE_CODE = 'CB18'.
              CALL FUNCTION 'ZTRPQ_GET_TRANSPORTRATE_KM1'
              EXPORTING
                ZTRANSPORTER       = LS_FINAL-TRANSPORTER
                ZVEHICLEGRP        = LS_FINAL-INVNR
                ZPONUMBER          = EKKO-EBELN
                ZEQTYP             = EQUI-EQTYP
                ZPLANT             = LV_PLANT
                ZCALCBASE          = LS_FINAL-ZCALBASE_CODE
                ZVEHPRIMARY        = LS_FINAL-ZVEH_PRIMARY
             IMPORTING
               ZMSTOTKM           = ZTOT_MST_KM
               ZCOMMITKM          = ZTOT_COMM_KM
               ZFINALRATE         = ZFINAL_RATE
               ZCALBASE           = ZCAL_BASE
               ZTRP_RATE          = ZTRP_RATE
               ZMIN_RATE          = ZMIN_RATE
                ZPENDINGKM         = ZPENDING_KM.

                IF ZFLAG = 1 OR ZFLAG = 3 .
                    lv_newamt  = ( ls_final-ACTUAL_KM * ZMIN_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                    LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZMIN_RATE ).
                    ls_final-net_amt = lv_newamt.
                ENDIF.

                IF ZFLAG = 0.

                   IF ZPENDING_KM =  ls_final-actual_km.
                      lv_newamt  = ( ls_final-ACTUAL_KM * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF ZPENDING_KM >  ls_final-actual_km.
                      lv_newamt  = ( ls_final-ACTUAL_KM * ZTRP_RATE ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                    LS_FINAL-QUOTATION_AMT = ZTRP_RATE.
                    ls_final-ZTRP_RATE = ZTRP_RATE.
                    ls_final-calculated_amt = ( ls_final-ACTUAL_KM * ZTRP_RATE ).
                    ls_final-net_amt = lv_newamt.
                    IF LS_FINAL-ADD_AMT IS INITIAL AND LS_FINAL-DE_AMT IS INITIAL.
                    ls_final-ZTRP_REMARK = ''.
                    ENDIF.
                 ENDIF.

                 IF  ls_final-actual_km > ZPENDING_KM .


                   lv_newamt  = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-ACTUAL_KM - ZPENDING_KM ) * ZMIN_RATE ) ) + LS_FINAL-ADD_AMT - LS_FINAL-DE_AMT + LS_FINAL-TOLL_AMT + LS_FINAL-COND_AMT - LS_FINAL-PENALTY_AMT.
                   LS_FINAL-QUOTATION_AMT = ZMIN_RATE.
                    ls_final-ZTRP_RATE = ZMIN_RATE.
                    ls_final-calculated_amt = ( ( ZPENDING_KM * ZTRP_RATE ) + ( ( ls_final-ACTUAL_KM - ZPENDING_KM ) * ZMIN_RATE ) ).
                    ls_final-net_amt = lv_newamt.

                    ZPENDING_KM_S = ZPENDING_KM.
                 CONCATENATE 'Pending KM-' ZPENDING_KM_S INTO ls_final-ZTRP_REMARK.
                   ENDIF.


                  ENDIF.

            ENDIF.


            CONDENSE : lv_newamt.


            CLEAR : zphy_status ,ls_ZVTRPQ_CC,zcost_center.
IF ( lv_plant = '2302' OR lv_plant = '1006' OR lv_plant = '1402' OR lv_plant = '1203' OR lv_plant = '2304' OR lv_plant = '1206' OR lv_plant = '1207').
         CALL FUNCTION 'ZSD_GETMANIFEST_PHYSTATUS'
           EXPORTING
             ZMANIFEST_NO         = LS_FINAL-MANIFEST
             ZSHIPMENT_TYPE       = zunload-shtyp
          IMPORTING
            ZPHY_STATUS          = zphy_status .
ENDIF.

*          IF zphy_status IS INITIAL OR zphy_status = ''.
*            zphy_status = 'COMMON'.
*            ENDIF.

*            SELECT SINGLE * FROM zvtrp_cc INTO ls_zvtrp_cc
*                            WHERE werks = lv_plant
*                             AND  spart = ls_final-spart
*                             AND  shtyp = zunload-shtyp.
IF ( lv_plant = '2302' OR lv_plant = '1006' OR lv_plant = '1402' OR lv_plant = '1203' OR lv_plant = '2304' OR lv_plant = '1206' OR lv_plant = '1207' ) AND  zphy_status IS NOT INITIAL .
        SELECT SINGLE * FROM ZVTRPQ_CC INTO ls_ZVTRPQ_CC
                          WHERE werks = lv_plant
                           AND  shtyp = zunload-shtyp
                           AND KURZTEXT = zphy_status.
          zcost_center = ls_ZVTRPQ_CC-KOSTL.

          ELSE.
            SELECT SINGLE * FROM zvtrp_cc INTO ls_zvtrp_cc
                            WHERE werks = lv_plant
                             AND  spart = ls_final-spart
                             AND  shtyp = zunload-shtyp.
              zcost_center = ls_zvtrp_cc-KOSTL.
 ENDIF.



   "Logic Add by Hiren To Post in Given Month 22-06-2018 Start
         DATA zparapostdate TYPE D.
          DATA zmanifestdate TYPE D.
         DATA zvalidupto(10) TYPE C.

         DATA zfinaldate(10) TYPE C.

         concatenate lv_postdate+6(4) lv_postdate+3(2) lv_postdate+0(2) into zmanifestdate.


        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
          day_in = zmanifestdate
          IMPORTING
          last_day_of_month = zparapostdate.

          WRITE zparapostdate to zvalidupto DD/MM/YYYY.

          SELECT single ZVALUE INTO zfinaldate FROM ZPARAMAST
             WHERE zplan = LV_PLANT
             AND ZTYPE = 'TRANSPORT_PERIOD'
             AND ZVALID_UPTO = zvalidupto
             AND ZSTATUS = 'T'.

         IF zfinaldate is not INITIAL.
           lv_postdate = zfinaldate.
         ENDIF.
         "transporttext
         DATA : act_qty TYPE STRING.
         DATA : rate TYPE STRING.
         CLEAR : act_qty,rate.
         act_qty = LS_FINAL-ACTUAL_QTY.
         rate = LS_FINAL-QUOTATION_AMT.
         CONDENSE act_qty.
         CONDENSE rate.
        CONCATENATE 'Transport|Rate:'rate INTO transporttext.


    "Logic End
      "Logic to Get TT/TK From Vehicle Group Start
      DATA: zveh_type(2) TYPE C.
      DATA: zlength TYPE I.
      CLEAR : LS_ZTSP_MAPPING , zveh_type ,zlength .

      zlength = STRLEN( LS_FINAL-INVNR ).
      zlength = zlength - 2.
      zveh_type =   LS_FINAL-INVNR+zlength(2).

      READ TABLE LT_ZTSP_MAPPING INTO LS_ZTSP_MAPPING WITH KEY ZVEHTYP = zveh_type.
      "End 12.03.2019

      IF LS_ZTSP_MAPPING IS INITIAL.
        MESSAGE 'Service Code is not maintained for this vehicle type..!!' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.



        IF LS_ZPI_VENDOR-ASNUM IS NOT INITIAL AND ls_final-inward_date >= '20190601'.
          CLEAR : zcommission_charges_amt.
          zcommission_charges_amt = ( LS_ZPI_VENDOR-KBETR * ( lv_newamt - ls_final-toll_amt - ls_final-cond_amt ) ) / 100 .
          ZCOMMISSION_CHARGES = zcommission_charges_amt .
          ENDIF.

lv_newamt = round( val = lv_newamt dec = 2 ).

SELECT COUNT(*) FROM TVARVC WHERE NAME = 'ZSESDEBUG_OPEN' AND low = 'T'.
    IF SY-subrc = 0 .
        lv_mode = 'A'.
      ENDIF.

CONDENSE : lv_newamt.

data:lv_tcode TYPE sy-tcode VALUE 'ZONLINE_PO'.
 export lv_tcode FROM lv_tcode to MEMORY id 'ZONLINE'.

CLEAR : LV_ACCPT.
DATA:lv_tcode_t TYPE sy-tcode.
CLEAR : lv_tcode_t.
FREE MEMORY ID 'SYSTEMSES'.

CLEAR : LS_ZPARAMAST.
IF ZUNLOAD-SHTYP = '0010' .
    SELECT SINGLE * INTO LS_ZPARAMAST FROM ZPARAMAST
          WHERE ZTYPE IN ( 'TRANSPORT_SES_RELEASE_INWARD'  )
            AND ZSALORG = lv_plant
            AND ZVALUE = 'T' AND ZSTATUS = 'T'.
      ELSEIF ZUNLOAD-SHTYP = '0001'.
          SELECT SINGLE * INTO LS_ZPARAMAST FROM ZPARAMAST
             WHERE ZTYPE IN ( 'TRANSPORT_SES_RELEASE_INWARD' )
               AND ZSALORG = lv_plant
               AND ZVALUE = 'T' AND ZSTATUS = 'T'.
  ENDIF.

IF LS_ZPARAMAST IS INITIAL  .

  SELECT COUNT(*)  FROM ZPARAMAST
          WHERE ZTYPE IN ( 'TRANSPORT_SES_OTH_ADD_APPROVAL' )
            AND ZSALORG = lv_plant
            AND ZVALUE = 'T' AND ZSTATUS = 'T'.

    IF SY-subrc = 0 .
        IF ls_final-ADD_AMT = 0 AND ls_final-zdevkm = 0.
           lv_tcode_t = 'AUTOSES'.
           export lv_tcode_t FROM lv_tcode_t to MEMORY id 'SYSTEMSES'. " 03.01.2018
           LV_ACCPT = 'T'.
         ENDIF.

         ELSE.
           lv_tcode_t = 'AUTOSES'.
           export lv_tcode_t FROM lv_tcode_t to MEMORY id 'SYSTEMSES'. " 03.01.2018
           LV_ACCPT = 'T'.
  ENDIF.
ENDIF.

"BY HS 16.03.2021 CHECK ACC ASSIGNMENT GRP S
SELECT SINGLE * INTO @DATA(LS_EKPO) FROM EKPO WHERE EBELN =  @ekko-ebeln.
  LS_ACC_GRP = LS_EKPO-KNTTP.
"BY HS 16.03.2021 End



            "IF ls_zvtrp_cc-kostl IS NOT INITIAL . 07.01.2018 Hiren Surati
             IF zcost_center IS NOT INITIAL .
*              if ls_final-net_amt is not INITIAL .
             PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=SELP'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0340'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11R-EBELN'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=ENTE'.
              PERFORM bdc_field       USING 'RM11R-EBELN'
                                             ekko-ebeln.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=NEU'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                            "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                            LS_FINAL-MANIFEST . "Added By hiren Surati 11-08-2018
              IF LS_ACC_GRP <> 'K'. " BY HS 16.03.2021
              PERFORM bdc_field       USING 'ESSR-KNTTP'
                                            'K'.
              ENDIF.
              PERFORM bdc_field       USING 'ESSR-LBLDT'
                                            lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.

              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                         LS_FINAL-MANIFEST.

              PERFORM bdc_field       USING 'ESSR-BKTXT'   "Added By Hiren Surati on 18-07-2018
                                        transporttext.

              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST . "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                             'ESSR-LBLNE'.
              IF LS_ACC_GRP <> 'K'. " BY HS 16.03.2021
              PERFORM bdc_field       USING 'ESSR-KNTTP'
                                            'K'.
              ENDIF.
              PERFORM bdc_field       USING 'ESSR-LBLDT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
*              PERFORM bdc_field       USING 'BDC_CURSOR'
*                                            'RM11P-KOSTL(01)'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.

               PERFORM bdc_field       USING 'ESLL-SRVPOS(01)'
                                            LS_ZTSP_MAPPING-ASNUM..

              PERFORM bdc_field       USING 'ESLL-MENGE(01)'
                                            '1'.

              PERFORM bdc_field       USING 'ESLL-TBTWR(01)'
                                            lv_newamt.
*              PERFORM bdc_field       USING 'RM11P-KOSTL(01)'
*
*                                            zcost_center.



              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.

              PERFORM bdc_field       USING 'ESKN-KOSTL(01)'

                                            zcost_center.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=BACK'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.


""""""""""2nd Line For Gobolt Start
IF LS_ZPI_VENDOR-KBETR > 0 AND ls_final-inward_date >= '20190601'..
              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST. "Added By hiren Surati 11-08-2018
*              PERFORM bdc_field       USING 'ESSR-KNTTP'
*                                            'K'.
*              PERFORM bdc_field       USING 'ESSR-LBLDT'
*                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                             lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                             lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
*              PERFORM bdc_field       USING 'BDC_CURSOR'
*                                            'RM11P-KOSTL(02)'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '20'.

               PERFORM bdc_field       USING 'ESLL-SRVPOS(02)'
                                            "LS_ZTSP_MAPPING-ASNUM.
                                            LS_ZPI_VENDOR-ASNUM..

               "HS Pawar
*              PERFORM bdc_field       USING 'ESLL-KTEXT1(01)'
*                                            "'Transport Expense'." Commeted By Hiren Surati 11-08-2018
*                                        transporttext. "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'ESLL-MENGE(02)'
                                            '1'.
              "Commed By H.S 12.03.2019 start
*              PERFORM bdc_field       USING 'ESLL-MEINS(01)'
*                                            'au'.

              "End
              PERFORM bdc_field       USING 'ESLL-TBTWR(02)'
                                            ZCOMMISSION_CHARGES.
*              PERFORM bdc_field       USING 'RM11P-KOSTL(02)'
*                                            "ls_zvtrp_cc-kostl.
*                                            zcost_center.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '/00'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.
              PERFORM bdc_field       USING 'ESKN-KOSTL(01)'
                                            "ls_zvtrp_cc-kostl.
                                            zcost_center.

*              PERFORM bdc_field       USING 'ESKN-SAKTO(01)' bY hs 08.06.2019
*                                            lv_sakto.

              PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESKN-SAKTO(01)'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=BACK'.
              PERFORM bdc_field       USING 'VRTKZ1'
                                            'X'.


              """""108.03.2022
perform bdc_dynpro      using 'SAPLMLSK' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM11K-MKNTM(02)'.
perform bdc_field       using 'ESKN-KOSTL(01)'
                              zcost_center. "//////////



perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'VRTKZ1'
                              'X'.

ENDIF.
""""""""""""""'2nd Line For Gobolt End


              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'ESSR-TXZ01'.


              IF LV_ACCPT = 'T'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=ACCP'.
              ENDIF.





              PERFORM bdc_field       USING 'ESSR-TXZ01'
                                             "ls_final-vtext. Commeted By Hiren Surati 11-08-2018
                                              LS_FINAL-MANIFEST. "Added By hiren Surati 11-08-2018
              PERFORM bdc_field       USING 'ESSR-BLDAT'
                                            lv_inward.
              PERFORM bdc_field       USING 'ESSR-BUDAT'
                                            lv_postdate.
              PERFORM bdc_field       USING 'ESSR-XBLNR'
                                        LS_FINAL-MANIFEST.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.

""""BY HS 08.03.2022

perform bdc_dynpro      using 'SAPLMLSK' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM11K-MKNTM(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'VRTKZ1'
                              'X'.

"""""



              PERFORM bdc_dynpro      USING 'SAPLMLSR' '0400'.

              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=SAVE'.
              PERFORM bdc_field       USING 'BDC_CURSOR'
                                            'RM11P-NEW_ROW'.
              PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                            '10'.
              PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
              PERFORM bdc_field       USING 'BDC_OKCODE'
                                            '=YES'.
              CALL TRANSACTION 'ML81N' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_message.
              WAIT UP TO 1 SECONDS .
              IF lt_message IS NOT INITIAL.
                DELETE lt_message WHERE msgtyp = 'W'.
                DELETE lt_message WHERE msgtyp = 'I'.
                CLEAR ls_message.
                LOOP AT lt_message INTO  ls_message.
                  CALL FUNCTION 'FORMAT_MESSAGE'
                    EXPORTING
                      id        = ls_message-msgid
*                     LANG      = '-D'
                      no        = ls_message-msgnr
                      v1        = sy-msgv1
                      v2        = sy-msgv2
                      v3        = sy-msgv3
                      v4        = sy-msgv4
                    IMPORTING
                      msg       = v_msg
                    EXCEPTIONS
                      not_found = 1
                      OTHERS    = 2.
                  IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
                    IF ls_message-msgtyp = 'E'.
                      ls_bdc-icon   =  c_red.
                      ls_bdc-id     =  ls_message-msgid.
                      ls_bdc-number =  ls_message-msgnr.
                      ls_bdc-message = v_msg.
                      APPEND ls_bdc TO lt_bdc.

                    ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .
                      CLEAR : lv_err_t.
                      lv_err_t = ls_message-msgv1.
                      lv_strlen =  strlen(  ls_message-msgv1 ).
                      IF  lv_strlen = 10 AND lv_err_t <> 'E'  .
                        ls_bdc-icon   =  c_green.
                        lv_ses_t       = ls_message-msgv1.
                        PERFORM : save_ztable.
                        ls_bdc-id     =  ls_message-msgid.
                        ls_bdc-number =  ls_message-msgnr.
                        ls_bdc-message = v_msg.
                        ls_bdc-text    = 'Tranport Service'.
                        ls_bdc-mani    = ls_final-manifest.
                        APPEND ls_bdc TO lt_bdc.

                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.

              ENDIF.

              REFRESH :lt_message.
              CLEAR : ls_final,ls_zvtrp_cc,ls_ZVTRPQ_CC.

            ELSE .
              MESSAGE 'Cost center Missing ' TYPE 'E' DISPLAY LIKE 'I'.
            ENDIF.
          ELSE .
            MESSAGE 'Please Maintain The Tranportaion Total Amount by Clicking Add Button.' TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.
        ELSE .
          MESSAGE 'Inward Date Missing' TYPE 'E'.
        ENDIF.
      ELSE .

        CONCATENATE 'Line Item' ls_rows-index 'Service Entry Sheet' ls_final-lblni 'Exsit'
           INTO lv_service SEPARATED BY ' '.
        MESSAGE lv_service TYPE 'E'.

      ENDIF.
      REFRESH lt_bdcdata .
    ENDLOOP.
  ELSE.
    MESSAGE 'Please select line item and create' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_ZTABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_ztable .
  REFRESH :lt_ztp_item.

  CLEAR : ls_ztp_item,ls_ekbe.
  SELECT SINGLE EBELN BELNR LFBNR FROM EKBE INTO ls_ekbe
          WHERE EBELN =  ekko-ebeln
          AND  BELNR =   lv_ses_t.

         IF ls_ekbe IS INITIAL.
           SELECT SINGLE EBELN BELNR LFBNR FROM EKBE INTO ls_ekbe
          WHERE EBELN =  ekko-ebeln
          AND  BELNR =   lv_ses.

           ENDIF.


  SELECT * FROM ztp_item INTO TABLE  lt_item
                         WHERE tplst = lv_plant
                         AND   shtyp =  zunload-shtyp
                         AND   ebeln = ekko-ebeln
                         AND   tknum =  ls_final-shipment_number
                         AND   vbeln =  ls_final-manifest .
  CONDENSE ls_final-actual_km.



  IF sy-subrc <>  0.

    ls_ztp_item-tplst      = lv_plant.
    ls_ztp_item-shtyp      = zunload-shtyp.
    ls_ztp_item-ebeln      = ekko-ebeln.
    ls_ztp_item-tknum      = ls_final-shipment_number .
    ls_ztp_item-vbeln      = ls_final-manifest .
    ls_ztp_item-kunnr      = ls_final-client_code.
    ls_ztp_item-transpzone = ls_final-area .
    ls_ztp_item-zakm       = ls_final-actual_km .
    ls_ztp_item-zaqty      =  ls_final-actual_qty.
    ls_ztp_item-zveh       = ls_final-vechile_number.
    ls_ztp_item-zvehgrp    = ls_final-invnr .
    ls_ztp_item-ztpamt     = ls_final-calculated_amt.
    ls_ztp_item-zdrt       = ls_final-zdrt .
    ls_ztp_item-zcgrdate   = ls_final-zcgrdate .
    ls_ztp_item-zcgrt      = ls_final-zcgrt .
    ls_ztp_item-zcgedate   = ls_final-zcgedate.
    ls_ztp_item-zcget      = ls_final-zcget .
    ls_ztp_item-zcdt       = ls_final-zcdt.
    ls_ztp_item-zgtrdate   = ls_final-zgtrdate.
    ls_ztp_item-zgtrt      = ls_final-zgtrt .
    ls_ztp_item-zgtoudate  = ls_final-zgtoudate.
    ls_ztp_item-zgtout     = ls_final-zgtout .
    ls_ztp_item-zodt       = ls_final-zodt .
    ls_ztp_item-ZWBROUDATE = ls_final-ZWBROUDATE.
    ls_ztp_item-zt_adn     = ls_final-add_amt.
    ls_ztp_item-zt_ded     = ls_final-de_amt.
    ls_ztp_item-zt_toll    = ls_final-toll_amt.
    ls_ztp_item-zt_cond    = ls_final-cond_amt.
    ls_ztp_item-zt_tamt    = ls_final-net_amt.
    ls_ztp_item-zd_adn     = ls_final-d_ad_amt.
    ls_ztp_item-zd_ded     = ls_final-d_ded_amt.
    ls_ztp_item-zd_tamt    = ls_final-d_tot_amt.
    ls_ztp_item-ZCON       = ls_final-ZCON.
    ls_ztp_item-ZD_RATE    = ls_final-ZDETEN_RATE.
    ls_ztp_item-ZPEN_AMT   = ls_final-PENALTY_AMT.
    ls_ztp_item-ZPCON      = ls_final-ZPCON.
    ls_ztp_item-ZMKM       = ls_final-LOCCO.
    ls_ztp_item-ZTRP_RATE  = ls_final-ZTRP_RATE.
    ls_ztp_item-ZVEH_PRIMARY = ls_final-ZVEH_PRIMARY.
    ls_ztp_item-zseskm  = ls_final-zses_km.
    ls_ztp_item-zdevkm  = ls_final-zdevkm.
    ls_ztp_item-zact_seskm = ls_final-zact_seskm.
    ls_ztp_item-ZDEVKM_REMARK = ls_final-ZDEVKM_REMARK.
    ls_ztp_item-zdcon      = ls_final-zdcon.
    ls_ztp_item-veh_primary_plant = ls_final-veh_primary_plant.
    IF p_tr = 'X'.
    ls_ztp_item-ZSES_FLAG  = 'MANUAL'.
    ENDIF.

    IF ls_final-ZTRP_REMARK IS NOT INITIAL.
    ls_ztp_item-ZTRP_REMARK = ls_final-ZTRP_REMARK.
    ENDIF.

    IF ls_final-ZDETEN_REMARK IS NOT INITIAL.
    ls_ztp_item-ZDETEN_REMARK = ls_final-ZDETEN_REMARK.
    ENDIF.
    IF ls_final-ZDEVKM_REMARK IS NOT INITIAL.
    ls_ztp_item-ZDEVKM_REMARK = ls_final-ZDEVKM_REMARK.
    ENDIF.



    IF lv_ses_t IS NOT INITIAL .
      ls_ztp_item-lblni      = lv_ses_t.
      ls_ztp_item-ZSLBLNI = ls_ekbe-LFBNR.
      ls_final-lblni  = lv_ses_t.
      MODIFY lt_final FROM ls_final TRANSPORTING lblni WHERE manifest = ls_final-manifest .
    ENDIF.

    IF lv_ses IS NOT INITIAL .
      ls_ztp_item-zlblni    = lv_ses.
      ls_ztp_item-ZDLBLNI = ls_ekbe-LFBNR.
      ls_final-zlblni  = lv_ses.
      MODIFY lt_final FROM ls_final TRANSPORTING zlblni WHERE manifest = ls_final-manifest .
    ENDIF.


    IF ls_ztp_item-lblni IS NOT INITIAL.
      DATA: zveh_cap TYPE STRING,
      zactual_qty TYPE STRING,
      zcond_charge TYPE STRING,
      zother_add TYPE STRING,
      zother_less TYPE STRING,
      ztoll_amt TYPE STRING.
      CLEAR : LS_ZTRP_Z ,LS_QPCT_Z,ls_wastein_z,zveh_cap,zactual_qty,ls_wasteout_z,zcond_charge,ZOTHER_ADD,zother_less,ztoll_amt,IT_TLINES,IT_TLINES[].
    "Added BY HS ON 19.03.2019 Start
      x_header-tdobject = 'ESSR'.
      x_header-tdname   = ls_ztp_item-ZSLBLNI.
      x_header-tdid     = 'TX01'.
      x_header-tdspras  = 'EN'.

      READ TABLE LT_ZTRP INTO LS_ZTRP_Z WITH KEY ZCON = ls_ztp_item-ZCON.
      READ TABLE LT_QPCT INTO LS_QPCT_Z WITH KEY CODE = LS_ZTRP_Z-Z_TRPUNIT.

      IF zunload-shtyp = '0010'.
      READ TABLE LT_WASTEIN INTO ls_wastein_z WITH KEY VBELN = ls_ztp_item-VBELN.
      ZVEH_CAP = ls_wastein_z-ENGINE_CAP.
        ELSEIF zunload-shtyp = '0010'.
          READ TABLE LT_WASTEOUT INTO ls_wasteout_z WITH KEY VBELN = ls_ztp_item-VBELN.
          ZVEH_CAP = ls_wasteout_z-ENGINE_CAP.

      ENDIF.

    CONCATENATE 'Manifest No:' ls_ztp_item-vbeln INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
    APPEND IT_TLINES.
    CLEAR  IT_TLINES.

    CONCATENATE 'Vehicle No:' ls_ztp_item-zveh '(' ls_ztp_item-ZVEHGRP ')' INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
      APPEND IT_TLINES.
      CLEAR  IT_TLINES.



    zactual_qty = ls_ztp_item-zaqty.
    CONCATENATE 'Vehicle Capacity:' ZVEH_CAP 'Actual Qty:' zactual_qty INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
      APPEND IT_TLINES.
      CLEAR  IT_TLINES.


    IF LS_ZTRP_Z-ZCALBASE = 'CB01'.
      CONCATENATE 'Transport Zone:' LS_ZTRP_Z-ZTRANSPZONE_TEXT INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
      APPEND IT_TLINES.
      CLEAR  IT_TLINES.
    ENDIF.

    IF LS_ZTRP_Z-ZCALBASE = 'CB05' OR LS_ZTRP_Z-ZCALBASE = 'CB06' OR LS_ZTRP_Z-ZCALBASE = 'CB07' OR LS_ZTRP_Z-ZCALBASE = 'CB07'.
      CONCATENATE 'Master KM:' ls_ztp_item-ZMKM 'Actual KM:' ls_ztp_item-zakm INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
      APPEND IT_TLINES.
      CLEAR  IT_TLINES.
      ENDIF.





    DATA : ZTP_RATE TYPE STRING.
    CLEAR : ZTP_RATE.
    ZTP_RATE = ls_ztp_item-ZTRP_RATE.
    CONCATENATE 'Rate:' ZTP_RATE 'Unit:' LS_QPCT_Z-KURZTEXT INTO IT_TLINES-TDLINE  SEPARATED BY SPACE .
    APPEND IT_TLINES.
    CLEAR : IT_TLINES , ZTP_RATE.

     IF LS_ZTRP_Z-Z_TRPUNIT = '1'.
       DATA : ZMIN_PAYABLE TYPE STRING.
       CLEAR : ZMIN_PAYABLE.
       ZMIN_PAYABLE = LS_ZTRP_Z-Z_MPQTY.
       CONCATENATE 'Minimum Payable Qty:' ZMIN_PAYABLE INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
       APPEND IT_TLINES.
       CLEAR  IT_TLINES.
     ENDIF.

     IF ls_ztp_item-zt_cond IS NOT INITIAL.
       zcond_charge = ls_ztp_item-zt_cond.
       CONCATENATE 'Conductor Charge:' zcond_charge INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
       APPEND IT_TLINES.
       CLEAR  IT_TLINES.
     ENDIF.


     IF ls_ztp_item-zt_adn IS NOT INITIAL.
       ZOTHER_ADD = ls_ztp_item-zt_adn.
       CONCATENATE 'Other Add:' ZOTHER_ADD INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
       APPEND IT_TLINES.
       CLEAR  IT_TLINES.
     ENDIF.

     IF ls_ztp_item-ZT_DED IS NOT INITIAL.
       zother_less = ls_ztp_item-ZT_DED.
       CONCATENATE 'Other Less:' zother_less INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
       APPEND IT_TLINES.
       CLEAR  IT_TLINES.
     ENDIF.

     IF ls_ztp_item-ZT_TOLL IS NOT INITIAL.
       ZTOLL_AMT = ls_ztp_item-ZT_TOLL.
       CONCATENATE 'Toll Tax:' ZTOLL_AMT INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
       APPEND IT_TLINES.
       CLEAR  IT_TLINES.
     ENDIF.



    CONCATENATE 'Rate ID:' ls_ztp_item-ZCON INTO IT_TLINES-TDLINE SEPARATED BY SPACE .
      APPEND IT_TLINES.
      CLEAR  IT_TLINES.

      CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      CLIENT                = SY-MANDT
      HEADER                = X_HEADER
      SAVEMODE_DIRECT       = 'X'
    TABLES
      LINES                 = IT_TLINES[]
     EXCEPTIONS
       ID                    = 1
       LANGUAGE              = 2
       NAME                  = 3
       OBJECT                = 4
       OTHERS                = 5.

      ENDIF.

      CLEAR : IT_TLINES,IT_TLINES[].
    "Added BY HS ON 19.03.2019 End



    APPEND ls_ztp_item TO lt_ztp_item.
    CLEAR ls_ztp_item.


    MODIFY ztp_item FROM TABLE lt_ztp_item.
    WAIT UP TO 1 SECONDS .
    COMMIT WORK.
    CLEAR: lv_ses_t, lv_ses.




  ELSE .

    IF  lv_ses  IS NOT INITIAL .
      UPDATE ztp_item
        SET
        ZD_RATE  = LS_FINAL-ZDETEN_RATE
        ZCDT     = LS_FINAL-ZCDT
        ZODT     = LS_FINAL-ZODT
        zdrt     = ls_final-zdrt
        zt_ded   = ls_final-de_amt
        zd_adn   = ls_final-d_ad_amt
        zd_ded   = ls_final-d_ded_amt
        zd_tamt  = ls_final-d_tot_amt
        zlblni =  lv_ses
        ZDLBLNI = ls_ekbe-LFBNR
        ZDETEN_REMARK = ls_final-ZDETEN_REMARK
        WHERE vbeln =   ls_final-manifest .
      COMMIT WORK.
    ENDIF.

    IF lv_ses_t  IS NOT INITIAL  .
      UPDATE ztp_item SET lblni =   lv_ses_t
              ZSLBLNI = ls_ekbe-LFBNR
             WHERE vbeln =   ls_final-manifest .
      COMMIT WORK.
    ENDIF.

    CLEAR : lv_ses_t,lv_ses.



  ENDIF.








ENDFORM.


FORM save_ztable_commitment .



  "REFRESH :lt_ztp_item,lt_ztp_itemcomm. BY HS 03.04.2019
  REFRESH :lt_ztp_itemcomm ,LT_ZTP_ITEMCOMLINE.
  "CLEAR : ls_ztp_item,ls_ztp_itemcomm,. BY HS 03.04.2019
  CLEAR : ls_ztp_itemcomm , LS_ZTP_ITEMCOMLINE  .

  DATA : zmaxdate(10) TYPE C , zmindate(10) TYPE C.
LOOP AT lt_final_c2 INTO LS_FINAL_C2.
  READ TABLE  lt_final_c2 INTO ls_final_c2 INDEX ls_rows-index .
    IF zmindate IS INITIAL.
      zmindate = ls_final_c2-DATE.
      "CONCATENATE ls_final_c2-date+6(2) '.' ls_final_c2-date+4(2) '.'  ls_final_c2-date+0(4) INTO zmindate.
      ENDIF.

ENDLOOP.
"CONCATENATE ls_final_c2-date+6(2) '.' ls_final_c2-date+4(2) '.'  ls_final_c2-date+0(4) INTO zmaxdate.
zmaxdate =  ls_final_c2-DATE.

*  SELECT * FROM ztp_item INTO TABLE  lt_item
*                         WHERE tplst = lv_plant
*                         AND   shtyp =  zunload-shtyp
*                         AND   ebeln = ZTP_ITEM-EBELN
*                         AND   ZVEH =  S_VEH-LOW
*                         AND   ZGTRDATE =  LS_FINAL_C2-DATE .

      CLEAR : ls_ekbe.
      "BREAK-POINT.
  SELECT SINGLE EBELN BELNR LFBNR FROM EKBE INTO ls_ekbe
          WHERE EBELN =  ZTP_ITEM-EBELN
          AND  BELNR =   lv_ses.

  SELECT * FROM ztp_itemcomm INTO TABLE  lt_itemcomm
                        WHERE tplst = lv_plant
                        AND   shtyp =  zunload-shtyp
                        AND   ZVEH =  S_VEH-LOW
                        AND ( ZFDATE ge zmindate AND ZTDATE le zmaxdate ).





  IF sy-subrc <>  0.

*    ls_ztp_item-tplst      = lv_plant.
*    ls_ztp_item-shtyp      = zunload-shtyp.
*    ls_ztp_item-ebeln      = ZTP_ITEM-EBELN.
*    ls_ztp_item-tknum      = ls_final_c2-SHIPMENT_NO.
*    ls_ztp_item-vbeln      = ZUNLOAD-VBELN .
*    ls_ztp_item-kunnr      = ''.
*    ls_ztp_item-transpzone = 'COMMITMENT' .
*    ls_ztp_item-zakm       = '' .
*    ls_ztp_item-zaqty      = 0 .
*    ls_ztp_item-zveh       = S_VEH-LOW.
*    ls_ztp_item-zvehgrp    = '' .
*    ls_ztp_item-ztpamt     = 0 .
*    ls_ztp_item-zdrt       = LS_FINAL_C2-TRP_RATE .
*    ls_ztp_item-zcgrdate   = '' .
*    ls_ztp_item-zcgrt      = '' .
*    ls_ztp_item-zcgedate   = ''.
*    ls_ztp_item-zcget      = ''.
*    ls_ztp_item-zcdt       = ''.
*    ls_ztp_item-zgtrdate   = LS_FINAL_C2-DATE.
*    ls_ztp_item-zgtrt      = ''.
*    ls_ztp_item-zgtoudate  = ''.
*    ls_ztp_item-zgtout     = ''.
*    ls_ztp_item-zodt       = ''.
*    ls_ztp_item-zt_adn     = 0 .
*    ls_ztp_item-zt_ded     = 0 .
*    ls_ztp_item-zt_toll    = 0 .
*    ls_ztp_item-zt_cond    = 0 .
*    ls_ztp_item-zt_tamt    = 0 .
*    ls_ztp_item-zd_adn     = 0 .
*    ls_ztp_item-zd_ded     = 0 .
*    ls_ztp_item-zd_tamt    = LS_FINAL_C2-ZF_AMT.
DATA zparavalidupto(10) TYPE C.
DATA ztempDate(10) TYPE C.
"CONCATENATE S_DATE-HIGH+6(2) '.' S_DATE-HIGH+4(2) '.' S_DATE-HIGH+0(4) INTO  ztempDate.
CONCATENATE zmaxdate+6(2) '.' zmaxdate+4(2) '.' zmaxdate+0(4) INTO  ztempDate.
      SELECT single ZVALUE INTO zparavalidupto FROM ZPARAMAST
                    WHERE zplan = LV_PLANT
                    AND ZTYPE = 'COMMITMENT_PERIOD'
                    AND ZVALID_UPTO = ztempDate
                    AND ZSTATUS = 'T'.


       IF  zparavalidupto is not INITIAL.
         zparavalidupto = zparavalidupto.
       CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input  = zparavalidupto
        IMPORTING
          output = zparavalidupto.
         ELSE .
         "zparavalidupto =   S_DATE-HIGH.
           zparavalidupto =   zmaxdate.

         ENDIF.
      " CONCATENATE   zparavalidupto+0(4)  zparavalidupto+4(2)  zparavalidupto+6(2) INTO zparavalidupto.



    ls_ztp_itemcomm-tplst      = lv_plant.
    ls_ztp_itemcomm-shtyp      = zunload-shtyp.
    ls_ztp_itemcomm-EQTYP      = equi-eqtyp.
    ls_ztp_itemcomm-ebeln      = ZTP_ITEM-EBELN.
    ls_ztp_itemcomm-LIFNR      = lfa1-lifnr.

    IF LS_TRP_C-ZCALBASE = 'CB09'.
          ls_ztp_itemcomm-zveh       = S_VEH-LOW.
    ENDIF.
    ls_ztp_itemcomm-ZFDATE     = zmindate.
    ls_ztp_itemcomm-ZTDATE     = zmaxdate.
    ls_ztp_itemcomm-ZCMTTRIP   = ZCT_HDR-ZCT_QTY.
    ls_ztp_itemcomm-ZCMRT      = LS_FINAL_C2-TRP_RATE.
    "ls_ztp_itemcomm-ZTOTTRIPS  = ls_final_c2-SHIPMENT_NO.
    IF LS_TRP_C-Z_TRPUNIT = '3'.
    ls_ztp_itemcomm-ZTOTTRIPS  = LV_ROW.
    ENDIF.

    IF LS_TRP_C-Z_TRPUNIT = '4'.
    ls_ztp_itemcomm-ZTOTTRIPS  = ZCT_HDR-ZSTRIP.
    ENDIF.
    ls_ztp_itemcomm-ZTPCMAMT  = ls_final-d_tot_amt.
    ls_ztp_itemcomm-ZCOMMTYPE = LS_TRP_C-Z_TRPUNIT.
    ls_ztp_itemcomm-ZRATEID  = LS_TRP_C-ZCON.
    ls_ztp_itemcomm-ZVEHGRP  = LS_TRP_C-ZVEHGRP.

*    IF lv_ses_t IS NOT INITIAL .
*      ls_ztp_item-lblni      = lv_ses_t.
*      ls_final-lblni  = lv_ses_t.
*      MODIFY lt_final FROM ls_final TRANSPORTING lblni WHERE manifest = ls_final-manifest .
*    ENDIF.
*
*    IF lv_ses IS NOT INITIAL .
*      ls_ztp_item-zlblni    = lv_ses.
*      ls_final-zlblni  = lv_ses.
*      MODIFY lt_final FROM ls_final TRANSPORTING zlblni WHERE manifest = ls_final-manifest .
*    ENDIF.

     ls_ztp_itemcomm-ZLBLNICM = lv_ses.
     ls_ztp_itemcomm-ZPOSTDATE = zparavalidupto.
     ls_ztp_itemcomm-ZSLBLNI = ls_ekbe-LFBNR.

*    APPEND ls_ztp_item TO lt_ztp_item.
*    CLEAR ls_ztp_item.

    APPEND ls_ztp_itemcomm TO lt_ztp_itemcomm.

    "MODIFY  LT_ZTP_ITEMCOMLINE FROM ls_ztp_itemcomm TRANSPORTING ZLBLNICM.

    CLEAR ls_ztp_itemcomm.


*    MODIFY ztp_item FROM TABLE lt_ztp_item.
*    WAIT UP TO 1 SECONDS .
*    COMMIT WORK.
*    CLEAR: lv_ses_t, lv_ses.


    MODIFY ztp_itemcomm FROM TABLE lt_ztp_itemcomm.
    WAIT UP TO 1 SECONDS .
    COMMIT WORK.


    MOVE-CORRESPONDING lt_ztp_item TO LT_ZTP_ITEMCOMLINE.
    LOOP AT LT_ZTP_ITEMCOMLINE INTO LS_ZTP_ITEMCOMLINE.
        LS_ZTP_ITEMCOMLINE-ZLBLNICM = LV_SES.
        LS_ZTP_ITEMCOMLINE-ZSLBLNI = ls_ekbe-LFBNR.
        MODIFY  LT_ZTP_ITEMCOMLINE FROM LS_ZTP_ITEMCOMLINE .
      ENDLOOP.

    MODIFY ZTP_ITEMCOMLINE FROM TABLE LT_ZTP_ITEMCOMLINE.
    WAIT UP TO 1 SECONDS .
       CLEAR: lv_ses_t, lv_ses.
*  ELSE .
*
*    IF  lv_ses  IS NOT INITIAL .
*      UPDATE ztp_item SET zlblni =  lv_ses
*             WHERE vbeln =   ls_final-manifest .
*      COMMIT WORK.
*    ENDIF.
*
*    IF lv_ses_t  IS NOT INITIAL  .
*      UPDATE ztp_item SET lblni =   lv_ses_t
*             WHERE vbeln =   ls_final-manifest .
*      COMMIT WORK.
*   ENDIF.

    CLEAR : lv_ses_t,lv_ses ,LS_ZTP_ITEMCOMLINE,LS_ZTP_ITEMCOMM ,lt_ztp_itemcomm,LT_ZTP_ITEMCOMLINE.



  ENDIF.








ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .
  REFRESH lt_fieldcat.
  CLEAR ls_fieldcat.
  REFRESH lt_fieldcat.
  ls_fieldcat-fieldname = 'ICON'.
  ls_fieldcat-seltext_m = 'LIGHT' .
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ID'.
  ls_fieldcat-seltext_m = 'ID' .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NUMBER'.
  ls_fieldcat-seltext_m = 'NUMBER' .
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-seltext_l = 'MESSAGE' .
  ls_fieldcat-outputlen = 50.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TEXT'.
  ls_fieldcat-seltext_l =  'Expence'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MANI'.
  ls_fieldcat-seltext_l =  'MANIFEST NO.'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = lt_bdc
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc = 0.
**    BREAK-POINT .
    CALL SCREEN 9002.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_message .

  IF lt_message IS NOT INITIAL.
    DELETE lt_message WHERE msgtyp = 'W'.
    DELETE lt_message WHERE msgtyp = 'I'.
    CLEAR ls_message.
    LOOP AT lt_message INTO  ls_message.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_message-msgid
*         LANG      = '-D'
          no        = ls_message-msgnr
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = v_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
        IF ls_message-msgtyp = 'E'.
          ls_bdc-icon   =  c_red.
          ls_bdc-id     =  ls_message-msgid.
          ls_bdc-number =  ls_message-msgnr.
          ls_bdc-message = v_msg.
          APPEND ls_bdc TO lt_bdc.

        ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .

          lv_strlen =  strlen(  ls_message-msgv1 ).
          IF  lv_strlen = 10 AND ls_message-msgv1 <> 'ESLL-TBTWR'.
            ls_bdc-icon   =  c_green.
            lv_ses_t       = ls_message-msgv1.
            PERFORM : save_ztable.
            ls_bdc-id     =  ls_message-msgid.
            ls_bdc-number =  ls_message-msgnr.
            ls_bdc-message = v_msg.
            ls_bdc-text    = 'Tranport Expence'.
            ls_bdc-mani    = ls_final-manifest.
            APPEND ls_bdc TO lt_bdc.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    REFRESH  lt_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MESSAGE1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_message1 .

  IF lt_message IS NOT INITIAL.
    DELETE lt_message WHERE msgtyp = 'W'.
    DELETE lt_message WHERE msgtyp = 'I'.
    CLEAR ls_message.
    LOOP AT lt_message INTO  ls_message.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_message-msgid
*         LANG      = '-D'
          no        = ls_message-msgnr
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = v_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
        IF ls_message-msgtyp = 'E'.
          ls_bdc-icon   =  c_red.
          ls_bdc-id     =  ls_message-msgid.
          ls_bdc-number =  ls_message-msgnr.
          ls_bdc-message = v_msg.
          APPEND ls_bdc TO lt_bdc.

        ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .

          lv_strlen =  strlen(  ls_message-msgv1 ).
          IF  lv_strlen = 10 AND ls_message-msgv1 <> 'ESLL-TBTWR'.
            ls_bdc-icon   =  c_green.
            lv_ses       = ls_message-msgv1.
            PERFORM : save_ztable.
            ls_bdc-id     =  ls_message-msgid.
            ls_bdc-number =  ls_message-msgnr.
            ls_bdc-message = v_msg.
            ls_bdc-text    = 'Detentation Expence'.
            ls_bdc-mani    = ls_final-manifest.
            APPEND ls_bdc TO lt_bdc.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.


FORM get_message_commitment .

  IF lt_message IS NOT INITIAL.
    DELETE lt_message WHERE msgtyp = 'W'.
    DELETE lt_message WHERE msgtyp = 'I'.
    CLEAR ls_message.
    LOOP AT lt_message INTO  ls_message.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_message-msgid
*         LANG      = '-D'
          no        = ls_message-msgnr
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = v_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF ls_message-msgtyp <> 'W' AND ls_message-msgtyp <> 'I' .
        IF ls_message-msgtyp = 'E'.
          ls_bdc-icon   =  c_red.
          ls_bdc-id     =  ls_message-msgid.
          ls_bdc-number =  ls_message-msgnr.
          ls_bdc-message = v_msg.
          APPEND ls_bdc TO lt_bdc.

        ELSEIF ls_message-msgtyp = 'S' AND  ls_message-msgv1 IS NOT INITIAL .

          lv_strlen =  strlen(  ls_message-msgv1 ).
          IF  lv_strlen = 10 AND ls_message-msgv1 <> 'ESLL-TBTWR'.
            ls_bdc-icon   =  c_green.
            lv_ses       = ls_message-msgv1.
            PERFORM : save_ztable_commitment.
            ls_bdc-id     =  ls_message-msgid.
            ls_bdc-number =  ls_message-msgnr.
            ls_bdc-message = v_msg.
            ls_bdc-text    = 'Commitment Expence'.
            ls_bdc-mani    = ls_final-manifest.
            APPEND ls_bdc TO lt_bdc.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.