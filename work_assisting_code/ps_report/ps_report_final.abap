*&---------------------------------------------------------------------*
*& Report ZPRG_PS_REPORT_S
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprg_ps_report_ss.

INCLUDE zps_declaration_top_s.
*INCLUDE zps_declaration_top.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukr  TYPE ps_vbukr OBLIGATORY,    "Company code
              p_werks TYPE werks_d , "OBLIGATORY,     "Plant
              p_prctr TYPE prctr  ,  "OBLIGATORY ,    "Profit Center
              p_pspnr TYPE ps_posnr OBLIGATORY ,    "Project / wbs element
              p_banfn TYPE banfn.                  "Purchase Requisition
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  SELECT pbukr werks prctr pspnr  posid   objnr psphi
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

*  IF lt_prps IS NOT INITIAL.
*    SELECT banfn disub_pspnr
*      FROM eban
*      INTO TABLE lt_eban
*      FOR ALL ENTRIES IN lt_prps
*      WHERE disub_pspnr = lt_prps-pspnr.
*  ENDIF.


  IF lt_prps IS NOT INITIAL.
    SELECT aufnr auart autyp  kappl pspel
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

    IF lt_caufv IS NOT INITIAL .
      SELECT aufpl aplzl banfn  bnfpo mill_oc_aufnr_mo
        FROM afvc
        INTO TABLE lt_afvc
        FOR ALL ENTRIES IN lt_caufv
        WHERE aufpl =  lt_caufv-aufpl.
*        and aplzl = lt_caufv-aufpl
*        and MILL_OC_AUFNR_MO =  lt_caufv-aufnr.

      IF lt_afvc IS NOT INITIAL.

        SELECT ebeln ebelp banfn bnfpo
          FROM ekpo
          INTO TABLE lt_ekpo
          FOR ALL ENTRIES IN lt_afvc
          WHERE banfn = lt_afvc-banfn
            AND bnfpo = lt_afvc-bnfpo.

        IF  lt_ekpo IS NOT INITIAL.
          SELECT ebeln  lifnr
            FROM ekko
            INTO TABLE lt_ekko
            FOR ALL ENTRIES IN lt_ekpo
            WHERE ebeln   =  lt_ekpo-ebeln.

          IF  lt_ekko IS NOT INITIAL.
            SELECT lifnr name1
              FROM lfa1
              INTO TABLE lt_lfa1
              FOR ALL ENTRIES IN lt_ekko
              WHERE lifnr = lt_ekko-lifnr.
          ENDIF.


        ENDIF.

        LOOP AT lt_afvc INTO ls_afvc.
          CLEAR ls_final.

          " Map fields from lt_afvc

*          ls_final-banfn = ls_afvc-banfn.  " This will ensure the banfn is appended correctly
*          IF ls_afvc-banfn = '' .
*             SKIP.
*          ENDIF.
          IF ls_afvc-banfn IS NOT INITIAL.
            ls_final-banfn = ls_afvc-banfn.
            ls_final-bnfpo = ls_afvc-bnfpo.
            " Retrieve PO data from EKPO
            READ TABLE lt_ekpo INTO ls_ekpo WITH KEY banfn = ls_afvc-banfn
                                                  bnfpo = ls_afvc-bnfpo.
            IF sy-subrc = 0.
              ls_final-ebeln = ls_ekpo-ebeln.  " Purchasing Document Number (PO)
              ls_final-ebelp = ls_ekpo-ebelp.  " Item Number of Purchasing Document

            ENDIF.
            IF  lt_ekpo IS NOT INITIAL.
              READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln  = ls_ekpo-ebeln.
              IF sy-subrc = 0.
                ls_final-lifnr = ls_ekko-lifnr.
              ENDIF.
              IF lt_ekko IS NOT INITIAL.
                READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr  = ls_ekko-lifnr.
                IF sy-subrc = 0.
                  ls_final-name1 = ls_lfa1-name1.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

          " Now add data from other related tables (PRPS, PROJ, EBAN, AUFK, CAUFV)
          " Read PRPS data
          READ TABLE lt_prps INTO ls_prps WITH KEY pspnr = p_pspnr.
          IF sy-subrc = 0.
            ls_final-vbukr = ls_prps-pbukr.
            ls_final-werks = ls_prps-werks.
            ls_final-prctr = ls_prps-prctr.
            ls_final-pspnr = ls_prps-pspnr.
            ls_final-posid = ls_prps-posid.
            ls_final-pspid = ls_prps-psphi.
          ENDIF.

          " Read PROJ data
          READ TABLE lt_proj INTO ls_proj WITH KEY vbukr = ls_prps-pbukr
                                           werks = ls_prps-werks
                                           prctr = ls_prps-prctr.
          IF sy-subrc = 0.
            ls_final-post1 = ls_proj-post1.
            ls_final-astnr = ls_proj-astnr.
            ls_final-plfaz = ls_proj-plfaz.
            ls_final-plsez = ls_proj-plsez.
          ENDIF.

          " Read EBAN data
*          READ TABLE lt_eban INTO ls_eban WITH KEY disub_pspnr = ls_prps-pspnr.
*          IF sy-subrc = 0.
*            ls_final-banfn = ls_eban-banfn.
*          ENDIF.

          " Read AUFK data
          READ TABLE lt_aufk INTO ls_aufk WITH KEY pspel = ls_prps-pspnr.
          IF sy-subrc = 0.
            ls_final-aufnr = ls_aufk-aufnr.
            ls_final-auart = ls_aufk-auart.
            ls_final-autyp = ls_aufk-autyp.
            ls_final-kappl = ls_aufk-kappl.
          ENDIF.

          " Read CAUFV data
          READ TABLE lt_caufv INTO ls_caufv WITH KEY aufnr = ls_aufk-aufnr.
          IF sy-subrc = 0.
            ls_final-aufpl = ls_caufv-aufpl.
          ENDIF.

          " Append the final record to lt_final
          APPEND ls_final TO lt_final.
        ENDLOOP.
      ENDIF.

    ENDIF.



  ENDIF.

  SORT lt_final BY aufnr banfn pspnr.
*  DELETE ADJACENT DUPLICATES FROM lt_final COMPARING aufnr banfn pspnr.



END-OF-SELECTION.

*  * Merge lt_prps and lt_proj into lt_final
****  LOOP AT lt_prps INTO ls_prps.
****    LOOP AT lt_proj INTO ls_proj
****      WHERE vbukr = ls_prps-pbukr
****        AND werks = ls_prps-werks
****        AND prctr = ls_prps-prctr.
*****        AND pspnr = ls_prps-pspnr.
****
****      CLEAR ls_final.
****      ls_final-vbukr = ls_prps-pbukr.
****      ls_final-werks = ls_prps-werks.
****      ls_final-prctr = ls_prps-prctr.
****      ls_final-pspnr = ls_prps-pspnr.
****      ls_final-posid = ls_prps-posid.
****      ls_final-pspid = ls_prps-psphi.
****
*****      ls_final-vbukr = ls_proj-vbukr.
*****      ls_final-pspid = ls_proj-pspid.
****      ls_final-post1 = ls_proj-post1.
****      ls_final-astnr = ls_proj-astnr.
****      ls_final-plfaz = ls_proj-plfaz.
****      ls_final-plsez = ls_proj-plsez.
****
****      READ TABLE lt_eban INTO ls_eban WITH KEY disub_pspnr = ls_prps-pspnr.
****      IF sy-subrc = 0.
****        ls_final-banfn = ls_eban-banfn.  " Purchase Requisition
****      ENDIF.
****
****
****      READ TABLE lt_aufk INTO ls_aufk WITH KEY pspel = ls_prps-pspnr.
****      IF sy-subrc = 0.
****        ls_final-aufnr = ls_aufk-aufnr.
****        ls_final-auart = ls_aufk-auart.
****        ls_final-autyp = ls_aufk-autyp.   "KAPPL
****        ls_final-kappl = ls_aufk-kappl.   "KAPPL
****      ENDIF.
****
****      READ TABLE lt_caufv INTO ls_caufv WITH KEY aufnr = ls_aufk-aufnr.
****      IF sy-subrc = 0.
****        ls_final-aufpl = ls_caufv-aufpl.
****      ENDIF.
****
****      APPEND ls_final TO lt_final.
****    ENDLOOP.
****  ENDLOOP.



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
          t_table      = lt_final.



      lo_alv->display( ).
*      CATCH cx_salv_msg.

    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'S'.

  ENDTRY.




*&---------------------------------------------------------------------*
*& Include          ZPS_DECLARATION_TOP
*&---------------------------------------------------------------------*

*tables:  proj , prps,  .


TYPES: BEGIN OF lty_proj,
         vbukr TYPE ps_vbukr,   "Company code
         werks TYPE werks_d,   "plant
         prctr TYPE prctr,     " Profit Center
         pspnr TYPE ps_intnr,  "Project (internal)
         pspid TYPE ps_pspid,     " Project ID (external)
         post1 TYPE ps_post1,
         astnr TYPE ps_astnr,     " Project Type / application number
         plfaz TYPE ps_plfaz_chg,  "Project planned start date  PLFAZPS_PLFAZ_CHG
         plsez TYPE ps_plsez_chg,   "Project planned finish date
       END OF lty_proj.

TYPES: BEGIN OF lty_prps,
         pbukr TYPE ps_pbukr,  " Company code
         werks TYPE werks_d,   " Plant
         prctr TYPE prctr,     " Profit Center
         pspnr TYPE ps_posnr,  " WBS Element
         posid TYPE ps_posid,   "Work Breakdown Structure Element (WBS Element)
         "  projn TYPE proj,      " Project Definition Number (link to PROJ table)
         objnr TYPE j_objnr,     "Object number
         psphi TYPE ps_psphi, "ps_pspid , "PS_PSPHI,    --- Current number of the appropriate project
       END OF lty_prps.

TYPES: BEGIN OF lty_eban,
         banfn       TYPE banfn,    " Purchase Requisition
         disub_pspnr TYPE ps_psp_pnr,    "ps_posnr - WBS Element
       END OF lty_eban.

TYPES: BEGIN OF lty_AUFK,
         aufnr TYPE aufnr,         " Order Number
         auart TYPE aufart ,       "Order Type
         autyp TYPE auftyp ,       " Order category
*         refnr TYPE aufrefnr,      " Reference Order Number
         kappl TYPE kappl ,       "Application
         pspel TYPE ps_psp_ele,    "Work breakdown structure element (WBS element)
       END OF lty_AUFK.



TYPES: BEGIN OF lty_caufv,
         aufnr    TYPE aufnr,           " Order Number
         anfaufnr TYPE aufanfnr ,    "Requesting order
         aufpl    TYPE co_aufpl ,      " Routing number of operations in the order
         aplzt    TYPE co_aplzl,           "General counter for order
         pspel    TYPE ps_psp_ele,    "Work breakdown structure element (WBS element)
       END OF lty_caufv.



TYPES: BEGIN OF lty_AFVC,
         aufpl            TYPE co_aufpl ,      " Routing number of operations in the order
         aplzl            TYPE co_aplzl,       "General counter for order
         banfn            TYPE co_banfn ,         "Purchase Requisition Number
         bnfpo            TYPE co_bnfpo,          "Item Number of Purchase Requisition in Order
         mill_oc_aufnr_mo TYPE mill_oc_aufnr_mo,    "Work breakdown structure element (WBS element)
       END OF lty_afvc.

*EKPO
TYPES: BEGIN OF lty_EKPO,
         ebeln TYPE ebeln ,      " Purchasing Document Number -po
         ebelp TYPE ebelp,       "Item Number of Purchasing Document
         banfn TYPE banfn ,        "Purchase Requisition Number -pr
         bnfpo TYPE bnfpo,         "Item Number of Purchase Requisition in Order
       END OF lty_EKPO.
* ekko - ebeln ebelp lifnr
TYPES: BEGIN OF lty_ekko,
         ebeln TYPE ebeln ,      " Purchasing Document Number -po
         lifnr TYPE elifn ,  "vendor account num
       END OF lty_Ekko.


TYPES: BEGIN OF lty_lfa1,
         lifnr TYPE lifnr ,      " Purchasing Document Number -po
         name1 TYPE name1_gp ,  "vendor account num
       END OF lty_lfa1.



TYPES: BEGIN OF lty_final,
         vbukr TYPE ps_vbukr,  "Company code
         werks TYPE werks_d,   "Plant
         prctr TYPE prctr,     "Profit Center
         pspnr TYPE ps_posnr,  "WBS Element
         posid TYPE ps_posid,  "WBS ID
         aufnr TYPE aufnr,
         auart TYPE aufart ,    "Order Type
         autyp TYPE auftyp , " Order category
         kappl TYPE kappl ,     "Application
         aufpl TYPE co_aufpl ,      " Routing number of operations in the order
         banfn TYPE banfn,     "Purchase Requisition
         bnfpo TYPE co_bnfpo,          "Item Number of Purchase Requisition in Order
         ebeln TYPE ebeln ,      " Purchasing Document Number -po
         ebelp TYPE ebelp,       "Item Number of Purchasing Document
         lifnr TYPE elifn ,  "vendor account num
         name1 TYPE name1_gp,
         pspid TYPE ps_pspid,  "Project ID (external)
         post1 TYPE ps_post1,  "Project description
         astnr TYPE ps_astnr,  "Project Type / application number
         plfaz TYPE ps_plfaz_chg, "Project planned start date
         plsez TYPE ps_plsez_chg, "Project planned finish date
       END OF lty_final.


DATA: lt_prps  TYPE TABLE OF lty_prps,   " Internal table for PRPS data
      ls_prps  TYPE lty_prps,            " Work area for PRPS data
      lt_proj  TYPE TABLE OF lty_proj,   " Internal table for PROJ data
      ls_proj  TYPE lty_proj,          " Work area for PROJ data
      lt_eban  TYPE TABLE OF lty_eban,   " Internal table for  data
      ls_eban  TYPE lty_eban,
      lt_AUFK  TYPE TABLE OF lty_AUFK,   " Order master data
      ls_AUFK  TYPE lty_AUFK,
      lt_caufv TYPE TABLE OF lty_caufv,   " Order master data
      ls_caufv TYPE lty_caufv,
      lt_AFVC  TYPE TABLE OF lty_AFVC,   " Operation within an order
      ls_AFVC  TYPE lty_AFVC,
      lt_ekpo  TYPE TABLE OF lty_ekpo,
      ls_ekpo  TYPE lty_ekpo,
      lt_ekko  TYPE TABLE OF lty_ekko,
      ls_ekko  TYPE lty_ekko,
      lt_lfa1  TYPE TABLE OF lty_lfa1,
      ls_lfa1  TYPE lty_lfa1,
      lt_final TYPE TABLE OF lty_final,  " Final internal table
      ls_final TYPE lty_final.