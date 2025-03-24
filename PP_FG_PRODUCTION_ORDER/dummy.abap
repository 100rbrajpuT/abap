REPORT z_production_order_report.

* Data Definitions
TYPES: BEGIN OF ty_output,
         plant                   TYPE werks_d,
         product_name            TYPE matnr,
         production_order        TYPE aufnr,
         work_center             TYPE arbpl,
         work_centername         TYPE ktext,
         material_easiness       TYPE string,
         posting_date            TYPE datum,
         start_date              TYPE string,
         finish_date             TYPE string,
         diff_time               TYPE string,
         solid                   TYPE erfmg,
         semisolid               TYPE erfmg,
         liquid                  TYPE erfmg,
         aqs                     TYPE erfmg,
         binder                  TYPE erfmg,
         waste                   TYPE erfmg,
         intermediate            TYPE erfmg,
         fg_reconsumed           TYPE erfmg,
         residue_consumed        TYPE erfmg,
         binders                 TYPE erfmg,
         total_input             TYPE erfmg,
         fg_output               TYPE erfmg,
         residue_receipt        TYPE erfmg,
         total_output            TYPE erfmg,
         jumbo_bag               TYPE erfmg,
         packing_scrap_drums     TYPE erfmg,
         packing_scrap_bags      TYPE erfmg,
         total_scrap             TYPE erfmg,
         binders_per            TYPE string,
         jumbo_bag_mt           TYPE string,
         scrap_per_on_waste     TYPE string,
         moisture_loss_on_waste TYPE string,
         output_input_ratio     TYPE string,
         batch                   TYPE charg_d,
         physicalstate           TYPE string,
         texture                 TYPE string,
         color                   TYPE string,
         odour                   TYPE string,
         ph                      TYPE string,
         gcv                     TYPE string,
         loi                     TYPE string,
         cl                      TYPE string,
         sulphur                 TYPE string,
         moi                     TYPE string,
         ash                     TYPE string,
         pkgmode                 TYPE string,
         contno                  TYPE string,
         compatibilitygroup      TYPE string,
         safetycategory         TYPE string,
         processcategory        TYPE string,
         processroute           TYPE string,
         preprocessroute         TYPE string,
         vcode                   TYPE string,
         code_description        TYPE string,
       END OF ty_output.

DATA: gt_output TYPE TABLE OF ty_output,
      go_alv    TYPE REF TO cl_salv_table.

* Selection Screen Parameters
PARAMETERS: p_plant    TYPE werks_d DEFAULT '2304',
            p_fromdate TYPE datum DEFAULT '20230401',
            p_todate   TYPE datum DEFAULT '20231231',
            p_matnr    TYPE matnr.

* Main Logic
START-OF-SELECTION.
  PERFORM fetch_data.
  PERFORM display_alv.

* Fetch Data from SAP Tables
FORM fetch_data.
  SELECT 
    a~werks AS plant,
    p~matnr AS product_name,
    a~aufnr AS production_order,
    ch~arbpl AS work_center,
    cr~ktext AS work_centername,
    qp~kurztext AS material_easiness,
    a~bldat AS posting_date,
    afa~isdd AS start_date,
    afa~iedd AS finish_date,
    'Calculated Time' AS diff_time,  -- Replace with actual time calculation
    SUM( CASE WHEN a~charg LIKE 'W%' AND SUBSTRING( a~matnr, 13, 6 ) LIKE '%SD%' AND a~bwart IN ('261','262')
             THEN a~erfmg ELSE 0 END ) AS solid,
    " ... Add all other SUM/CASE columns similarly ...
    MAX( z~charg ) AS batch,
    MAX( z~physicalstate ) AS physicalstate,
    " ... Add all other MAX columns ...
    qe~vcode,
    CASE qe~vcode
      WHEN 'A1' THEN 'ACCEPTED'
      WHEN 'A2' THEN 'ACCEPTED WITH DEVIATION'
      WHEN 'R'  THEN 'REJECTED'
      ELSE ' ' END AS code_description

  FROM aufm AS a
  LEFT JOIN afpo AS p ON a~aufnr = p~aufnr
  LEFT JOIN afko AS ko ON a~aufnr = ko~aufnr
  LEFT JOIN afvc AS af ON ko~aufpl = af~aufpl
  LEFT JOIN crhd AS ch ON af~arbid = ch~objid
  LEFT JOIN crtx AS cr ON ch~objid = cr~objid
  LEFT JOIN mcha AS mc ON a~charg = mc~charg
  LEFT JOIN ausp AS au ON mc~cuobj_bm = au~objek
  LEFT JOIN qpct AS qp ON au~atwrt = qp~codegruppe
  LEFT JOIN afru AS afa ON a~aufnr = afa~aufnr
  LEFT JOIN zibatchcharinfo AS z ON a~charg = z~charg
  LEFT JOIN qals AS qa ON a~aufnr = qa~aufnr
  LEFT JOIN qave AS qe ON qa~prueflos = qe~prueflos

  WHERE a~bwart IN ('261','262','531','532','101','102')
    AND a~werks = @p_plant
    AND a~bldat BETWEEN @p_fromdate AND @p_todate
    AND ( p_matnr IS INITIAL OR p~matnr = @p_matnr )

  GROUP BY a~werks, a~aufnr, p~matnr, ch~arbpl, cr~ktext, qp~kurztext, 
           a~bldat, afa~isdd, afa~iedd, qe~vcode

  INTO CORRESPONDING FIELDS OF TABLE @gt_output.
ENDFORM.

* Display ALV Report
FORM display_alv.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output ).

      " Optimize column widths
      go_alv->get_columns( )->set_optimize( abap_true ).

      " Set column titles
      DATA(lo_columns) = go_alv->get_columns( ).
      lo_columns->get_column( 'PLANT' )->set_short_text( 'Plant' ).
      " Set all other column titles similarly...

      " Display ALV
      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.