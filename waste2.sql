REPORT zwaste_request_clnt2.

TABLES: zcustomer_ref, zcust_ref_log.

TYPES: BEGIN OF ty_final,
         item_id        TYPE zcustomer_ref-item_id,    "sales orgnx
         emp_id         TYPE zcustomer_ref-emp_id,
         customer_code  TYPE kunnr,  " Customer code from CDS table function
         customer_name  TYPE abap_char_100,  " Customer name from CDS table function
         waste_code     TYPE matnr,  " Waste code from CDS table function
         waste_name     TYPE maktx,  " Waste name from CDS table function
         lifting_priority TYPE abap_dec_18_0,  " Lifting priority from CDS table function
       END OF ty_final.

DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv,
      ls_layout   TYPE slis_layout_alv,
      lt_result   TYPE TABLE OF ty_final,
      ls_result   TYPE ty_final.

* Selection parameters for CDS Table Function
DATA: lv_sel_opt TYPE abap_char_1000 VALUE '1000',
      lv_paratype TYPE abap_char_30 VALUE 'PARAMETER'.

* Selecting data from zcustomer_ref and joining with zcust_ref_log
SELECT a~item_id,
       a~emp_id,
       b~customer_code,
       b~customer_name,
       b~waste_code,
       b~waste_name,
       b~lifting_priority
  INTO TABLE lt_result
  FROM zcustomer_ref AS a
  INNER JOIN zcust_ref_log AS b ON a~item_id = b~item_id
  LEFT JOIN zft_clinet_waste( sel_opt = lv_sel_opt, paratype = lv_paratype ) AS cw
    ON b~customer_code = cw~customer_code
   AND b~plant_code = cw~plant_code.

* Field catalog and ALV setup can be added here

* Call ALV display function to show the output
