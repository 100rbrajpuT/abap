*&---------------------------------------------------------------------*
*& Report ZPP_FG_PRD_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPP_FG_PRD_ORDER.

TABLES : aufm .

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS : s_werks for aufm-werks ,
                    s_date for aufm-bldat ,
                    s_matnr  for aufm-matnr .

SELECTION-SCREEN: END OF BLOCK b1.

TYPES: BEGIN OF ty_afru,
         werks TYPE afru-werks,
         aufnr TYPE afru-aufnr,
         isdd  TYPE afru-isdd,
         isdz  TYPE afru-isdz,
         iedd  TYPE afru-iedd,
         iedz  TYPE afru-iedz,
         srno  TYPE i, " Correct field name for row number
       END OF ty_afru.

*for row rank
DATA: lt_afru TYPE TABLE OF ty_afru,
      lt_afru_filtered TYPE TABLE OF ty_afru.

SELECT werks, aufnr, isdd, isdz, iedd, iedz
  FROM afru
  WHERE stokz = ''
    AND stzhl = '00000000'
  INTO TABLE @lt_afru.

SORT lt_afru BY werks aufnr isdd DESCENDING.
DELETE ADJACENT DUPLICATES FROM lt_afru COMPARING werks aufnr.

DATA: lt_afru_ranked TYPE TABLE OF ty_afru,
      lv_srno TYPE i,
      lv_prev_werks TYPE afru-werks,
      lv_prev_aufnr TYPE afru-aufnr.

SELECT werks, aufnr, isdd, isdz, iedd, iedz
  FROM afru
  WHERE stokz = ''
    AND stzhl = '00000000'
  INTO TABLE @lt_afru_ranked.

SORT lt_afru_ranked BY werks aufnr isdd DESCENDING.

CLEAR: lv_srno, lv_prev_werks, lv_prev_aufnr.
LOOP AT lt_afru_ranked ASSIGNING FIELD-SYMBOL(<fs_afru>).
  IF lv_prev_werks IS INITIAL OR <fs_afru>-werks <> lv_prev_werks OR <fs_afru>-aufnr <> lv_prev_aufnr.
    lv_srno = 1. " Reset row number for a new group
  ELSE.
    lv_srno = lv_srno + 1. " Increment row number within the same group
  ENDIF.
  <fs_afru>-srno = lv_srno. " Assign the row number to the structure field
  lv_prev_werks = <fs_afru>-werks. " Update previous werks
  lv_prev_aufnr = <fs_afru>-aufnr. " Update previous aufnr
ENDLOOP.

DATA: lt_result TYPE TABLE OF ty_result. " Define a structure for the new fields

SELECT werks,
       aufnr,
       MAX(matnr) AS product_name,
       MAX(charg) AS batch,
       MAX(crtx~ktext) AS work_center_name,
       TO_CHAR(bldat, 'DD/MM/YYYY') AS posting_date,
       SUM(CASE WHEN charg LIKE 'W%' AND matnr LIKE '%SD%' AND bwart IN ('261', '262')
                THEN CASE WHEN shkzg = 'S' THEN erfmg ELSE -erfmg END ELSE 0 END) AS solid,
       SUM(CASE WHEN charg LIKE 'W%' AND matnr LIKE '%SS%' AND bwart IN ('261', '262')
                THEN CASE WHEN shkzg = 'S' THEN erfmg ELSE -erfmg END ELSE 0 END) AS semi_solid,
       // ...additional fields from the stored procedure...
  FROM aufm
  LEFT JOIN afpo ON afpo~aufnr = aufm~aufnr
  LEFT JOIN afko ON afko~aufnr = afpo~aufnr
  INNER JOIN afvc ON afvc~aufpl = afko~aufpl
  INNER JOIN crhd ON crhd~objid = afvc~arbid
  LEFT JOIN crtx ON crtx~objid = crhd~objid
  LEFT JOIN mcha ON mcha~charg = aufm~charg
               AND mcha~werks = aufm~werks
  WHERE aufm~werks IN @s_werks
    AND aufm~bldat IN @s_date
    AND aufm~matnr IN @s_matnr
  GROUP BY werks, aufnr, bldat
  INTO TABLE @lt_result.

* Display Results
LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs>).
  WRITE: / <fs>-werks, <fs>-aufnr, <fs>-product_name, <fs>-batch, <fs>-work_center_name, <fs>-posting_date.
  // ...display additional fields...
ENDLOOP.