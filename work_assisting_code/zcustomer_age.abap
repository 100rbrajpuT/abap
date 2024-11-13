CLEAR lv_counter.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'RBUKRS'.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-seltext_l  = 'Comapny Code'. "'Customer Number
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'KUNNR'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = TEXT-004. "'Customer Number
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'NAME1'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = TEXT-005. "'Customer Name
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

*  lv_counter = lv_counter + 1.
*  gs_fieldcat-col_pos    = lv_counter.
*  gs_fieldcat-fieldname  = 'PRCTR'.
*  gs_fieldcat-tabname    = 'GT_FINAL1'.
*  gs_fieldcat-seltext_l  = TEXT-008. "'Profit Center
**  gs_fieldcat-no_zero    = 'X'.
*  APPEND gs_fieldcat TO gt_fieldcat.
*  CLEAR gs_fieldcat.
*
*  lv_counter = lv_counter + 1.
*  gs_fieldcat-col_pos    = lv_counter.
*  gs_fieldcat-fieldname  = 'PRCTR_DESC'.
*  gs_fieldcat-tabname    = 'GT_FINAL1'.
*  gs_fieldcat-seltext_l  = 'Profit Center Desc'.
**  gs_fieldcat-no_zero    = 'X'.
*  APPEND gs_fieldcat TO gt_fieldcat.
*  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'VKGRP'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = 'Sales Group'.   "'Sales office
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'VKGRP_TXT'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = 'Sales Group Desc'.   "'Sales office
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

***  lv_counter = lv_counter + 1.
***  gs_fieldcat-col_pos    = lv_counter.
***  gs_fieldcat-fieldname  = 'PENDAYS'.
***  gs_fieldcat-tabname    = 'GT_FINAL'.
***  gs_fieldcat-seltext_l  = 'Arrear'.
***  gs_fieldcat-no_zero    = 'X'.
***  APPEND gs_fieldcat TO gt_fieldcat.
***  CLEAR gs_fieldcat.
  IF r_inv NE abap_true.
    lv_counter = lv_counter + 1.
    gs_fieldcat-col_pos    = lv_counter.
    gs_fieldcat-fieldname  = 'NOT_DUE'.
    gs_fieldcat-tabname    = 'GT_FINAL1'.
    gs_fieldcat-seltext_l  = 'Not Due'.
    gs_fieldcat-no_zero    = 'X'.
    gs_fieldcat-do_sum    = 'X'.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.
  ENDIF.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_15'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a1. "text-010."'1-30 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*
  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_30'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a2. "text-011. "'31-60 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_45'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a3. "text-012."'61-90 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*
  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_60'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a4. "text-013."'91-120 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_75'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a5. "text-014."'121-150 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*
  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_90'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a6. "text-015."'151-180 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*
  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_105'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a7."text-016."'181-365 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*
  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'A_120'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = lv_a8. "text-018."'366-730 Days'.
  gs_fieldcat-do_sum     = 'X'.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.



  lv_counter = lv_counter + 1.
  gs_fieldcat-col_pos    = lv_counter.
  gs_fieldcat-fieldname  = 'TOTAL'.
  gs_fieldcat-tabname    = 'GT_FINAL1'.
  gs_fieldcat-seltext_l  = 'Total'.
  gs_fieldcat-do_sum     = abap_true.
*  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.