*&---------------------------------------------------------------------*
*& Report ZPRG_PS_REPORT_S
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprg_ps_report_s.

INCLUDE zps_declaration_top.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukr  TYPE ps_vbukr OBLIGATORY,    "Company code
              p_werks TYPE werks_d , "OBLIGATORY,     "Plant
              p_prctr TYPE prctr  ,  "OBLIGATORY ,    "Profit Center
              p_pspnr TYPE ps_posnr OBLIGATORY ,    "Project / wbs element
              p_banfn TYPE banfn.                  "Purchase Requisition
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
*  SELECT vbukr werks prctr  pspnr pspid post1 astnr plfaz plsez
*    FROM proj
*    INTO TABLE lt_proj
*    WHERE vbukr = p_bukr
*      AND werks = p_werks
*      AND prctr = p_prctr.

* IF lt_proj is not INITIAL.
*      SELECT pbukr werks prctr pspnr  posid   psphi
*      from prps
*      into table lt_prps
*        FOR ALL ENTRIES IN lt_proj
*      where pbukr  =  lt_proj-vbukr
*      and PSPNR  = lt_proj-pspnr.
*    ENDIF.




*  IF lt_proj IS NOT INITIAL.
*    SELECT pbukr werks prctr pspnr posid psphi "projn
*    FROM prps
*     INTO TABLE lt_prps.
*    FOR ALL ENTRIES IN lt_proj
*     WHERE pspnr = 52.
*     pbukr = lt_proj-vbukr
*     AND werks = lt_proj-werks
*     AND prctr = lt_proj-prctr
*    AND pspnr = lt_proj-pspnr.
*     and psphi  = lt_proj-pspnr.   "pspid.  "pspnr.
*      and POSID  = lt_proj-pspnr.


**      pbukr TYPE ps_pbukr,  " Company code
**         werks TYPE werks_d,   " Plant
**         prctr TYPE prctr,     " Profit Center
**         pspnr TYPE ps_posnr,  " WBS Element
**         posid TYPE ps_posid,   "Work Breakdown Structure Element (WBS Element)
**         "  projn TYPE proj,      " Project Definition Number (link to PROJ table)
**         psphi TYPE ps_psphi, "ps_pspid , "PS_PSPHI,



  SELECT pbukr werks prctr pspnr  posid   psphi
    FROM prps
    INTO TABLE lt_prps
    WHERE pbukr  =  p_bukr
    AND pspnr  = p_pspnr.


  IF lt_prps IS NOT INITIAL.
    SELECT vbukr werks prctr  pspnr pspid post1 astnr plfaz plsez
  FROM proj
  INTO TABLE lt_proj
      FOR ALL ENTRIES IN lt_prps
  WHERE vbukr = lt_prps-pbukr
    AND werks = lt_prps-werks
    AND prctr = lt_prps-prctr.
  ENDIF.

  IF lt_prps IS NOT INITIAL.
    SELECT banfn disub_pspnr
      FROM eban
      INTO TABLE lt_eban
      FOR ALL ENTRIES IN lt_prps
      WHERE disub_pspnr = lt_prps-pspnr.
  ENDIF.

*  auart TYPE aufart ,    "Order Type
*         autyp TYPE auftyp , " Order category

  IF lt_prps IS NOT INITIAL.
    SELECT aufnr auart autyp  KAPPL pspel
      FROM aufk
      INTO TABLE lt_aufk
      FOR ALL ENTRIES IN lt_prps
      WHERE pspel = lt_prps-pspnr.

    IF lt_aufk IS NOT INITIAL.
      SELECT aufnr anfaufnr aufpl aplzt pspel
        FROM caufv
        INTO TABLE lt_caufv
        FOR ALL ENTRIES IN lt_aufk
     WHERE aufnr = lt_aufk-aufnr.
*        WHERE anfaufnr = lt_aufk-aufnr.
    ENDIF.

    IF lt_caufv is not INITIAL .
      SELECT AUFPL aplzl banfn MILL_OC_AUFNR_MO
        from afvc
        INTO TABLE lt_afvc
        FOR ALL ENTRIES IN lt_caufv
        where aufpl =  lt_caufv-aufpl.
*        and aplzl = lt_caufv-aufpl
*        and MILL_OC_AUFNR_MO =  lt_caufv-aufnr.

         IF lt_afvc IS NOT INITIAL.
        LOOP AT lt_afvc INTO ls_afvc.
          CLEAR ls_final.
          " Map fields from lt_afvc to final table structure
*          ls_final-aufpl = ls_afvc-aufpl.
*          ls_final-aplzl = ls_afvc-aplzl.
          ls_final-banfn = ls_afvc-banfn.
*          ls_final-mill_oc_aufnr_mo = ls_afvc-mill_oc_aufnr_mo.

          " Optionally, map more fields if necessary
          " Append the entry to the final table
          APPEND ls_final TO lt_final.
        ENDLOOP.
      ENDIF.

    ENDIF.



  ENDIF.





END-OF-SELECTION.

*  * Merge lt_prps and lt_proj into lt_final
  LOOP AT lt_prps INTO ls_prps.
    LOOP AT lt_proj INTO ls_proj
      WHERE vbukr = ls_prps-pbukr
        AND werks = ls_prps-werks
        AND prctr = ls_prps-prctr.
*        AND pspnr = ls_prps-pspnr.

      CLEAR ls_final.
      ls_final-vbukr = ls_prps-pbukr.
      ls_final-werks = ls_prps-werks.
      ls_final-prctr = ls_prps-prctr.
      ls_final-pspnr = ls_prps-pspnr.
      ls_final-posid = ls_prps-posid.
      ls_final-pspid = ls_prps-psphi.

*      ls_final-vbukr = ls_proj-vbukr.
*      ls_final-pspid = ls_proj-pspid.
      ls_final-post1 = ls_proj-post1.
      ls_final-astnr = ls_proj-astnr.
      ls_final-plfaz = ls_proj-plfaz.
      ls_final-plsez = ls_proj-plsez.

      READ TABLE lt_eban INTO ls_eban WITH KEY disub_pspnr = ls_prps-pspnr.
      IF sy-subrc = 0.
        ls_final-banfn = ls_eban-banfn.  " Purchase Requisition
      ENDIF.


      READ TABLE lt_aufk INTO ls_aufk WITH KEY pspel = ls_prps-pspnr.
      IF sy-subrc = 0.
        ls_final-aufnr = ls_aufk-aufnr.
        ls_final-auart = ls_aufk-auart.
        ls_final-autyp = ls_aufk-autyp.   "KAPPL
        ls_final-KAPPL = ls_aufk-KAPPL.   "KAPPL
      ENDIF.

      READ TABLE lt_caufv INTO ls_caufv WITH KEY aufnr = ls_aufk-aufnr.
      IF sy-subrc = 0.
        ls_final-aufpl = ls_caufv-aufpl.
      ENDIF.

      APPEND ls_final TO lt_final.
    ENDLOOP.
  ENDLOOP.



  DATA : lo_alv TYPE REF TO cl_salv_table.
  TRY.
      CALL METHOD cl_salv_table=>factory
*      EXPORTING
*        list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*        r_container    =
*        container_name =
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_final. "lt_prps. "lt_proj. "lt_final.
      lo_alv->display( ).
*      CATCH cx_salv_msg.

    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'S'.

  ENDTRY.
