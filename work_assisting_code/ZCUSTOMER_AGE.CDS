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
