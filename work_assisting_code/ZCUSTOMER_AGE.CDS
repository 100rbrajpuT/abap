@AbapCatalog.sqlViewName: 'ZCUST_TBL_CDS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Zcustomer Age CDS'
@Metadata.ignorePropagatedAnnotations: true
define view zcust_tmp_cds as select from zcust_tbl
{
      sno,               // Sequence number
    bukrs,             // Company Code
    kunnr,             // Customer Number
    name1,             // Customer Name
    kdgrp,             // Customer Group
    vkorg,             // Sales Organization
    vkorg_txt,         // Sales Org Text
    vkgrp,             // Sales Group
    vkgrp_txt,         // Sales Group Text
    vtweg,             // Distribution Channel
    dist_chnl,         // Distribution Channel Text
    bzirk,             // Sales District
    sal_dis,           // Sales District Description
    prctr,             // Profit Center
    prctr_desc,        // Profit Center Description
    gsber,             // Business Area
    gtext,             // Business Area Text
    vkbur,             // Sales Office
    xblnr,             // Reference Document Number
    awkey,             // Reference Key
    augbl,             // Clearing Document Number
    augdt,             // Clearing Date
    belnr,             // Accounting Document Number
    blart,             // Document Type
    umskz,             // Special G/L Indicator
    saknr,             // G/L Account Number
    hkont,             // Bank Account
    budat,             // Posting Date
    bldat,             // Document Date
    faedt,             // Net Due Date
    zfbdt,             // Baseline Date
    shkzg,             // Debit/Credit Indicator
    zlspr,             // Payment Method
    current1 ,  // Current Amount (renamed if necessary)
    unblock_date,      // Unblock Date
    raise_date,        // Raise Date
    block_date,        // Block Date
    dispute_no,        // Dispute Number
    dis_age,           // Dispute Age
    raise_age,         // Raise Age
    remark,            // Remarks
    original,          // Original Amount
    bal_amt,           // Balance Amount
    arrears,           // Arrears / Pendays
    not_due,           // Not Due Amount
    a_15,              // 0-15 Days Amount
    a_30,              // 15-30 Days Amount
    a_45,              // 30-45 Days Amount
    a_60,              // 45-60 Days Amount
    a_75,              // 60-75 Days Amount
    a_90,              // 75-90 Days Amount
    a_105,             // 90-105 Days Amount
    a_120,             // 105-120 Days Amount
    total,             // Total Amount
    email,             // Email
    lifnr,             // Vendor Number
    name_p             // Vendor Name
}
*****************************************
code to insert data in Table*******
*----------------------------------------------------------------------*
***INCLUDE ZRFI_CSTMER_AGE_RPT_CP_INF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form insert_data_to_zcust_tbl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM insert_data_to_zcust_tbl .

  DATA: lt_zcust_tbl TYPE TABLE OF zcust_tbl,   " Internal table for ZCUST_TBL
        ls_zcust_tbl TYPE zcust_tbl,
         lv_index     TYPE numc4 VALUE '0001'.


  LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).

    CLEAR ls_zcust_tbl.

    " Map fields from gt_final to ls_zcust_tbl
**    ls_zcust_tbl-bukrs       = <fs_final>-bukrs.
**    ls_zcust_tbl-kunnr       = <fs_final>-kunnr.
**    ls_zcust_tbl-belnr       = <fs_final>-belnr.
**    ls_zcust_tbl-faedt       = <fs_final>-faedt.
ls_zcust_tbl-sno         = lv_index.
 ls_zcust_tbl-bukrs       = <fs_final>-bukrs.
  ls_zcust_tbl-kunnr       = <fs_final>-kunnr.
  ls_zcust_tbl-name1       = <fs_final>-name1.
  ls_zcust_tbl-kdgrp       = <fs_final>-kdgrp.
  ls_zcust_tbl-vkorg       = <fs_final>-vkorg.
  ls_zcust_tbl-vkorg_txt   = <fs_final>-vkorg_txt.
  ls_zcust_tbl-vkgrp       = <fs_final>-vkgrp.
  ls_zcust_tbl-vkgrp_txt   = <fs_final>-vkgrp_txt.
  ls_zcust_tbl-vtweg       = <fs_final>-vtweg.
  ls_zcust_tbl-dist_chnl   = <fs_final>-dist_chnl.
  ls_zcust_tbl-bzirk       = <fs_final>-bzirk.
  ls_zcust_tbl-sal_dis     = <fs_final>-sal_dis.
  ls_zcust_tbl-prctr       = <fs_final>-prctr.
  ls_zcust_tbl-prctr_desc  = <fs_final>-prctr_desc.
  ls_zcust_tbl-gsber       = <fs_final>-gsber.
  ls_zcust_tbl-gtext       = <fs_final>-gtext.
  ls_zcust_tbl-vkbur       = <fs_final>-vkbur.
  ls_zcust_tbl-xblnr       = <fs_final>-xblnr.
  ls_zcust_tbl-awkey       = <fs_final>-awkey.
  ls_zcust_tbl-augbl       = <fs_final>-augbl.
  ls_zcust_tbl-augdt       = <fs_final>-augdt.
  ls_zcust_tbl-belnr       = <fs_final>-belnr.
  ls_zcust_tbl-blart       = <fs_final>-blart.
  ls_zcust_tbl-umskz       = <fs_final>-umskz.
  ls_zcust_tbl-saknr       = <fs_final>-saknr.
  ls_zcust_tbl-hkont       = <fs_final>-hkont.
  ls_zcust_tbl-budat       = <fs_final>-budat.
  ls_zcust_tbl-bldat       = <fs_final>-bldat.
  ls_zcust_tbl-faedt       = <fs_final>-faedt.
  ls_zcust_tbl-zfbdt       = <fs_final>-zfbdt.
  ls_zcust_tbl-shkzg       = <fs_final>-shkzg.
  ls_zcust_tbl-zlspr       = <fs_final>-zlspr.
  ls_zcust_tbl-current1     = <fs_final>-current.
  ls_zcust_tbl-unblock_date = <fs_final>-unblock_date.
  ls_zcust_tbl-raise_date  = <fs_final>-raise_date.
  ls_zcust_tbl-block_date  = <fs_final>-block_date.
  ls_zcust_tbl-dispute_no  = <fs_final>-dispute_no.
  ls_zcust_tbl-dis_age     = <fs_final>-dis_age.
  ls_zcust_tbl-raise_age   = <fs_final>-raise_age.
  ls_zcust_tbl-remark      = <fs_final>-remark.
  ls_zcust_tbl-original    = <fs_final>-original.
  ls_zcust_tbl-bal_amt     = <fs_final>-bal_amt.
  ls_zcust_tbl-arrears     = <fs_final>-pendays. "may be   pendays
  ls_zcust_tbl-not_due     = <fs_final>-not_due.
  ls_zcust_tbl-a_15        = <fs_final>-a_15.
  ls_zcust_tbl-a_30        = <fs_final>-a_30.
  ls_zcust_tbl-a_45        = <fs_final>-a_45.
  ls_zcust_tbl-a_60        = <fs_final>-a_60.
  ls_zcust_tbl-a_75        = <fs_final>-a_75.
  ls_zcust_tbl-a_90        = <fs_final>-a_90.
  ls_zcust_tbl-a_105       = <fs_final>-a_105.
  ls_zcust_tbl-a_120       = <fs_final>-a_120.
*  ls_zcust_tbl-a_180       = <fs_final>-a_180.
*  ls_zcust_tbl-a_240       = <fs_final>-a_240.
  ls_zcust_tbl-total       = <fs_final>-total.
  ls_zcust_tbl-email       = <fs_final>-email.
  ls_zcust_tbl-LIFNR   = <fs_final>-LIFNR.
  ls_zcust_tbl-NAME_P  = <fs_final>-NAME_P.


    " Append the mapped data to the internal table
    APPEND ls_zcust_tbl TO lt_zcust_tbl.
      lv_index = lv_index + 1.

*    MODIFY zcust_tbl FROM TABLE lt_zcust_tbl.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ENDIF.
  ENDLOOP.

" Modify table in a single statement and commit once
IF lt_zcust_tbl IS NOT INITIAL.
  MODIFY zcust_tbl FROM TABLE lt_zcust_tbl.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE 'Data inserted/updated successfully' TYPE 'I'.
  ELSE.
    MESSAGE 'Error in inserting/updating data' TYPE 'E'.
  ENDIF.
ENDIF.




ENDFORM.