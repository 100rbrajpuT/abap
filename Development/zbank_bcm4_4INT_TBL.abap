*&---------------------------------------------------------------------*
*& Report ZBANK_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbank_1.



TABLES :bnk_batch_item,bnk_batch_header.
**************** INPUT SCREEN ******************************************
SELECT-OPTIONS : s_ZBUKR FOR bnk_batch_header-zbukr,
                 s_LAUO FOR bnk_batch_item-laufi_o,
                 s_batch  FOR bnk_batch_header-Batch_no ,
                 s_LAUF FOR bnk_batch_header-laufd.

TYPES :BEGIN OF bnk_batch_header,
         guid      TYPE bnk_com_btch_guid,
         zbukr     TYPE dzbukr,
         batch_no  TYPE bnk_com_btch_no,     " batch number
         laufd     TYPE bnk_com_btch_mrge_dat,

         batch_sum TYPE bnk_com_btch_amount, " batch SUM
         rule_id   TYPE bnk_com_btch_rule_id,  " rule ID
         item_cnt  TYPE bnk_com_btch_ctr, " item number of paymnet
         hbkid     TYPE hbkid, " bank ID
       END OF bnk_batch_header.

TYPES : BEGIN OF bnk_batch_item,
          zbukr      TYPE dzbukr,
          batch_no   TYPE bnk_com_btch_no,
          laufd      TYPE bnk_com_btch_mrge_dat,
          laufi_o    TYPE laufi,
          item_no    TYPE bnk_com_btch_cnt,
          lifnr      TYPE lifnr,    "party name
          vblnr      TYPE vblnr, "pay no
          amt_rulecu TYPE bnk_com_pymntamt_in_rulcu, " amount
          kunnr      TYPE kunnr,
        END OF bnk_batch_item.

TYPES : BEGIN OF t001,
          bukrs TYPE bukrs,
          butxt TYPE butxt,
          adrnr TYPE adrnr,
        END OF t001.

TYPES : BEGIN OF adrc,
          addrnumber TYPE ad_addrnum,
          name1      TYPE ad_name1,
          name2      TYPE ad_name2,
        END OF adrc.

TYPES : BEGIN OF tbnk_rule_t,
          rule_id TYPE bnk_com_rule_id,
          text    TYPE bnk_com_rule_id_desc,
        END OF tbnk_rule_t.

TYPES : BEGIN OF bnka,
          banka TYPE banka,
          bankl TYPE bankk,
          bnklz TYPE bankl,
          swift TYPE swift,
        END OF bnka.
TYPES : BEGIN OF reguh, " laufd laufi hbkid zbukr ubnky ubknt  vblnr lifnr kunnr xvorl zswif zbnkn zbnkn_long  bkref
          laufd      TYPE laufd,
          laufi      TYPE laufi,
          hbkid      TYPE hbkid,
          zbukr      TYPE dzbukr,
          ubnky      TYPE ubnky,
          ubknt      TYPE ubknt,
          vblnr      TYPE vblnr,
          lifnr      TYPE lifnr,
          name1      TYPE name1_gp,
          kunnr      TYPE kunnr,
          xvorl      TYPE xvorl,
          zswif      TYPE swift,
          zbnkn      TYPE dzbnkn,
          zbnkn_long TYPE dzbnkn35,
          bkref      TYPE bkref,

        END OF reguh.

TYPES : BEGIN OF acdoca,
          rbukrs TYPE bukrs,
          belnr  TYPE belnr_d,
          prctr  TYPE prctr,
          blart  TYPE blart,
          netdt  TYPE netdt,
          awref  TYPE awref,
          hsl    TYPE fins_vhcur12,
          lifnr  TYPE lifnr,
          kunnr  TYPE kunnr,
          gjahr  TYPE gjahr,
          augdt  TYPE augdt,
          augbl  TYPE augbl,
          bldat  TYPE bldat,
        END OF acdoca.

DATA :it_bnkH   TYPE TABLE OF bnk_batch_header,
      wa_bnkH   TYPE bnk_batch_header,
      it_bnkI   TYPE TABLE OF bnk_batch_item,
      wa_bnki   TYPE bnk_batch_item,
      it_t001   TYPE TABLE OF t001,
      wa_t001   TYPE t001,
      it_adrc   TYPE TABLE OF adrc,
      wa_adrc   TYPE adrc,
      it_tt     TYPE TABLE  OF tbnk_rule_t,
      wa_tt     TYPE tbnk_rule_t,
      it_bnka   TYPE TABLE OF bnka,
      wa_bnka   TYPE bnka,
      it_reguh  TYPE TABLE OF reguh,
      wa_reguh  TYPE reguh,
      it_acdoca TYPE TABLE  OF acdoca,
      wa_acdoca TYPE acdoca.

TYPES: BEGIN OF header_data,
         name1     TYPE ad_name1,
         name2     TYPE ad_name2,
         full_name TYPE string,
         zbukr     TYPE dzbukr,
         hbkid     TYPE hbkid,
         ubnky     TYPE ubnky,
         ubknt     TYPE ubknt,
         banka     TYPE banka,
         batch_no  TYPE bnk_com_btch_no,
         batch_sum TYPE bnk_com_btch_amount,
         item_cnt  TYPE bnk_com_btch_ctr,
         rule_id   TYPE bnk_com_btch_rule_id,
         text      TYPE bnk_com_rule_id_desc,
         laufi     TYPE laufi,
       END OF header_data.

TYPES: BEGIN OF item_data,
         lifnr      TYPE lifnr,         " Party name
         vblnr      TYPE vblnr,        " Payment number
         amt_rulecu TYPE bnk_com_pymntamt_in_rulcu, " Amount
         laufd      TYPE bnk_com_btch_mrge_dat,     " Merge date
*           banka          TYPE banka,        " Bank Name
*         ifsc           TYPE bankl,        " IFSC Code
         "  beneficiary_ac TYPE bnklz,        " Beneficiary Account Number
         " rbukrs TYPE bukrs,
         belnr      TYPE belnr_d,
         prctr      TYPE prctr,
         blart      TYPE blart,
         werks      TYPE werks_d,
         name1      TYPE string,
         lif_name1  TYPE name1_gp,
         zswif      TYPE swift,
         zbnkn      TYPE dzbnkn,
         bkref      TYPE bkref,
         banka      TYPE banka,
         konzs      TYPE konzs,
*         bldat      TYPE bldat,
         xblnr      TYPE xblnr1,
       END OF item_data.

TYPES: BEGIN OF lt_bill_dt,
         belnr TYPE belnr_d,
         blart TYPE blart,
         bldat TYPE bldat,
         awref TYPE awref,
         netdt TYPE netdt,
         xblnr TYPE xblnr1,
         ppnam TYPE char12, " Adjust size based on expected length
         wrbtr TYPE wrbtr,
         skfbt TYPE skfbt,
         augbl TYPE belnr_d,
       END OF lt_bill_dt.


DATA: it_item TYPE TABLE OF item_data,
      wa_item TYPE item_data.


DATA: it_header TYPE TABLE OF header_data,
      wa_header TYPE header_data.

DATA: it_bill TYPE TABLE OF lt_bill_dt,
      wa_bill TYPE lt_bill_dt.

START-OF-SELECTION.

  SELECT guid zbukr batch_no laufd batch_sum rule_id item_cnt hbkid
    INTO TABLE it_bnkh
    FROM bnk_batch_header
    WHERE zbukr IN s_zbukr
    AND batch_no IN s_batch
    AND laufd IN s_LAUF.

*  SELECT zbukr batch_no laufd laufi_o item_no lifnr vblnr amt_rulecu item_no
*    INTO TABLE it_bnkI
*    FROM bnk_batch_item
*    FOR ALL ENTRIES IN it_bnkh
*    WHERE batch_no = it_bnkh-batch_no AND zbukr = it_bnkh-zbukr AND laufd = it_bnkh-laufd AND laufi_o IN s_LAUO.

  IF it_bnkh IS NOT INITIAL.
    SELECT zbukr batch_no laufd laufi_o item_no lifnr vblnr amt_rulecu item_no
    INTO TABLE it_bnkI
    FROM bnk_batch_item
    FOR ALL ENTRIES IN it_bnkh
    WHERE batch_no = it_bnkh-batch_no .
*  AND zbukr = it_bnkh-zbukr
*  AND laufd = it_bnkh-laufd
    " AND laufi_o IN s_LAUO.

  ENDIF.



*                 s_ZBUKR FOR bnk_batch_header-zbukr,        "company
*                 s_LAUO FOR bnk_batch_item-laufi_o,         "identification
*                 s_batch  FOR bnk_batch_header-Batch_no ,   "batch
*                 s_LAUF FOR bnk_batch_header-laufd.         "date

  SELECT bukrs butxt  adrnr
    INTO TABLE it_t001
    FROM t001
    FOR ALL ENTRIES IN it_bnkh
    WHERE bukrs = it_bnkh-zbukr.

  IF it_t001 IS NOT INITIAL.
    SELECT addrnumber name1 name2
      INTO TABLE it_adrc
      FROM adrc
      FOR ALL ENTRIES IN it_t001
      WHERE addrnumber = it_t001-adrnr.
  ENDIF.

  IF it_bnkh IS NOT INITIAL.
    SELECT rule_id text
      INTO TABLE it_tt
      FROM tbnk_rule_t
      FOR ALL ENTRIES IN it_bnkh
      WHERE rule_id = it_bnkh-rule_id.
  ENDIF.

  IF it_bnki IS NOT INITIAL.
    SELECT laufd laufi hbkid zbukr ubnky ubknt  vblnr lifnr name1 kunnr xvorl zswif zbnkn zbnkn_long  bkref
      INTO TABLE it_reguh
      FROM reguh
      FOR ALL ENTRIES IN it_bnki
      WHERE laufd = it_bnki-laufd
      AND laufi = it_bnki-laufi_o  "or laufi in s_LAUO
      AND zbukr = it_bnki-zbukr
      AND lifnr = it_bnki-lifnr
      AND xvorl <> 'X' . " or xvorl = 'X'.
  ENDIF.



  IF  it_reguh IS NOT INITIAL.
    SELECT  rbukrs belnr prctr blart netdt awref hsl lifnr kunnr gjahr augdt augbl bldat
      FROM Acdoca
      INTO TABLE it_ACDOCA
      FOR ALL ENTRIES IN it_reguh
      WHERE rbukrs = it_reguh-zbukr
       AND lifnr = it_reguh-lifnr
      AND augdt = it_reguh-laufd .
    "  AND blart IN ('RE' , 'AB' , 'zp', 'kz' , 'KB' , 'SA' ,  'KO' , 'AB' ).
  ENDIF.

  IF it_reguh IS NOT INITIAL .
    SELECT  banka bankl bnklz swift
        INTO TABLE it_bnka
        FROM bnka
        FOR ALL ENTRIES IN it_reguh
        WHERE swift = it_reguh-zswif . " and bankl = it_reguh-ubnky. "and BANKS = 'IN'.
  ENDIF.



*header internal table
  LOOP AT it_bnkh INTO wa_bnkH.
    CLEAR wa_header.
    wa_header-zbukr = wa_bnkH-zbukr.
    wa_header-batch_no = wa_bnkH-batch_no.
    wa_header-batch_sum = wa_bnkH-batch_sum.
    wa_header-item_cnt = wa_bnkH-item_cnt.
    wa_header-rule_id = wa_bnkH-rule_id.

    READ TABLE it_bnkI INTO wa_bnki WITH KEY batch_no = wa_bnkH-batch_no.   "it_bnkI INTO wa_bnki.
    IF sy-subrc = 0.
      wa_header-laufi = wa_bnki-laufi_o.
    ENDIF.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_bnkH-zbukr.
    IF sy-subrc = 0.
      " Get name1 and name2 from it_adrc based on adrnr
      READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber = wa_t001-adrnr.
      IF sy-subrc = 0.
        wa_header-name1 = wa_adrc-name1.
        wa_header-name2 = wa_adrc-name2.
        CONCATENATE wa_adrc-name1 wa_adrc-name2 INTO wa_header-full_name SEPARATED BY space.
      ELSE.
        WRITE: / 'No matching ADRC record for ADRNR:', wa_t001-adrnr.
      ENDIF.
    ELSE.
      WRITE: / 'No matching T001 record for ZBUKRS:', wa_bnkH-zbukr.
    ENDIF.

    " Get data from it_reguh
    READ TABLE it_reguh INTO wa_reguh WITH KEY zbukr = wa_bnkH-zbukr.
    IF sy-subrc = 0.
      wa_header-hbkid = wa_reguh-hbkid.
      wa_header-ubnky = wa_reguh-ubnky.
      wa_header-ubknt = wa_reguh-ubknt.
      "wa_header-laufi = wa_reguh-laufi.
    ENDIF.

    " Get data from it_bnka
    READ TABLE it_bnka INTO wa_bnka WITH KEY bankl = wa_reguh-ubnky.
    IF sy-subrc = 0.
      wa_header-banka = wa_bnka-banka.
    ENDIF.

    READ TABLE it_tt INTO wa_tt WITH KEY rule_id =  wa_bnkh-rule_id.
    IF sy-subrc = 0.
      wa_header-text = wa_tt-text.
    ENDIF.

    " Append the combined data to the header table
    APPEND wa_header TO it_header.
  ENDLOOP.




  DATA: lt_t001w TYPE TABLE OF t001w,
        wa_t001w TYPE t001w.

**  " Fetch all T001W records once
**  SELECT name1
**    INTO TABLE lt_t001w
**    FROM t001w.
**
**  SORT lt_t001w BY werks.


*item_internal table
  LOOP AT it_bnkI INTO wa_bnki.
    CLEAR wa_item.
    wa_item-lifnr = wa_bnki-lifnr.
    wa_item-vblnr = wa_bnki-vblnr.
    wa_item-amt_rulecu = wa_bnki-amt_rulecu.
    wa_item-laufd = wa_bnki-laufd.

    READ TABLE it_acdoca INTO wa_acdoca WITH KEY awref = wa_item-vblnr.
    IF sy-subrc = 0.
      wa_item-belnr = wa_acdoca-belnr.
      wa_item-prctr = wa_acdoca-prctr.
      wa_item-werks = wa_acdoca-prctr+0(4).
      wa_item-blart = wa_acdoca-blart.
    ENDIF.

    IF wa_item-werks IS NOT INITIAL.
      SELECT SINGLE name1
      INTO wa_item-name1
      FROM t001w
      WHERE werks = wa_item-werks.
    ENDIF.

    READ TABLE it_reguh INTO wa_reguh WITH KEY lifnr =  wa_item-lifnr.   "vblnr lifnr
    IF sy-subrc = 0.
      wa_item-zswif = wa_reguh-zswif.
      wa_item-zbnkn = wa_reguh-zbnkn.
      wa_item-lif_name1 = wa_reguh-name1.
    ENDIF.
    READ TABLE it_bnka INTO wa_bnka WITH KEY swift = wa_reguh-zswif .
    IF  sy-subrc = 0.
      wa_item-banka = wa_bnka-banka.
    ENDIF.

    IF wa_item-lifnr IS NOT INITIAL.
      SELECT SINGLE  konzs
        FROM lfa1
       INTO (wa_item-konzs )
        WHERE lifnr  =  wa_item-lifnr.
    ENDIF.

    READ TABLE  it_acdoca  INTO  wa_acdoca WITH KEY augbl = wa_item-vblnr.
    IF  sy-subrc = 0.
      SELECT SINGLE xblnr
      INTO (wa_item-xblnr  )
      FROM bkpf
      WHERE belnr =  wa_item-belnr
      AND  gjahr = wa_acdoca-gjahr
      AND bukrs IN s_ZBUKR.
    ENDIF.

    APPEND wa_item TO it_item.
  ENDLOOP.




  DATA: lt_processed TYPE TABLE OF lt_bill_dt, " Table to track processed entries
        ls_processed TYPE lt_bill_dt.

*for transactions / bill

  LOOP AT it_item INTO wa_item .
    LOOP AT it_acdoca INTO wa_acdoca WHERE augbl = wa_item-vblnr.
*      IF wa_acdoca-belnr NE wa_acdoca-awref.
*       IF wa_acdoca-blart IN 'AB''RE''KZ' 'ZE' 'KR' .
      IF wa_acdoca-blart = 'AB' OR wa_acdoca-blart = 'RE' OR wa_acdoca-blart = 'KZ' OR wa_acdoca-blart = 'ZE' OR wa_acdoca-blart = 'KR'.

        CLEAR: wa_bill.

        " Populate fields for the transaction
        wa_bill-belnr = wa_acdoca-belnr.
        wa_bill-blart = wa_acdoca-blart.
        wa_bill-bldat = wa_acdoca-bldat.
        wa_bill-awref = wa_acdoca-awref.
        wa_bill-netdt = wa_acdoca-netdt.
        wa_bill-augbl = wa_acdoca-augbl.

        " Fetch invoice and payment amounts from BSEG
        SELECT SINGLE wrbtr skfbt
          INTO (wa_bill-wrbtr, wa_bill-skfbt)
          FROM bseg
          WHERE lifnr = wa_item-lifnr
            AND belnr = wa_bill-belnr
            AND gjahr = wa_acdoca-gjahr.

        " Fetch additional details from BKPF
        SELECT SINGLE xblnr ppnam
          INTO (wa_bill-xblnr, wa_bill-ppnam)
          FROM bkpf
          WHERE belnr = wa_bill-belnr
            AND gjahr = wa_acdoca-gjahr
            AND bukrs IN s_zbukr.

        " Fetch user name from user_addr
        SELECT SINGLE name_textc
          INTO wa_bill-ppnam
          FROM user_addr
          WHERE bname = wa_bill-ppnam.

        " Check if the BELNR is already processed
        READ TABLE lt_processed WITH KEY belnr = wa_bill-belnr TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          " Append to the processed table and the it_bill table
          APPEND wa_bill TO lt_processed.
          APPEND wa_bill TO it_bill.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDLOOP.


  " Display the header table for debugging
  cl_demo_output=>display( it_header ).
  cl_demo_output=>display( it_item ).
  cl_demo_output=>display(  it_bill ).
*  cl_demo_output=>display( it_bnkh ).
*  cl_demo_output=>display( it_bnkI ).
*  cl_demo_output=>display( it_t001 ).
*  cl_demo_output=>display( it_adrc ).
*  cl_demo_output=>display( it_tt ).
*  cl_demo_output=>display( it_reguh ).
*  cl_demo_output=>display( it_bnka ).
*  cl_demo_output=>display( it_acdoca ).
