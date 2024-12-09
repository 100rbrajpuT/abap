@AbapCatalog.sqlViewName: 'ZVEND_TRAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'VENDOR_TRAIL_REPORT'
@Metadata.ignorePropagatedAnnotations: true
define view ZVENDOR_TRAIL with parameters 
    p_company  : abap.char(10),
    p_from_date: abap.dats,
    p_to_date  : abap.dats
    /*
 as select from lfb1  as K
 left outer join lfa1 C on K.lifnr=C.lifnr
    left outer join adrc ADR on ADR.addrnumber=C.adrnr
    left outer join but000 M on C.lifnr=M.partner
    left outer join  t005u t5 on t5.bland=C.regio and t5.spras='E' and t5.land1='IN' */
    as select from lfb1 as K
  left outer join lfa1 as C
    on K.lifnr = C.lifnr
  left outer join adrc as ADR
    on ADR.addrnumber = C.adrnr
  left outer join but000 as M
    on C.lifnr = M.partner
  left outer join t005u as T5
    on T5.bland = C.regio
   and T5.spras = 'E'
   and T5.land1 = 'IN'
  left outer join acdoca as A
    on K.lifnr = A.lifnr
   and K.bukrs = A.rbukrs
  left outer join skat as G
    on K.akont = G.saknr
   and G.ktopl = 'LGIN'
    left outer join ZCDS_OPENING_BALANCE(p_company: :p_company, p_from_date: :p_from_date) as OB
    on K.bukrs = OB.company_code
   and K.lifnr = OB.vendor
{
      K.mandt                                 as client,
    K.bukrs                                 as company,
    K.lifnr                                 as vendor,
    C.name1                                 as vendor_name ,
     ADR.street                              as address_line_1, 
    ADR.str_suppl1                          as address_line_2,
    ADR.str_suppl2                          as address_line_3,
    ADR.location                            as district,
    ADR.city1                               as city,
    ADR.post_code1                          as postal_code,
    ADR.city2                               as region ,
    C.stcd3                              as gst_number,
    C.j_1ipanno                             as pan_number,
    C.regio                                 as vendor_state_code,
    T5.bezei                                as vendor_state_name , 
    coalesce(OB.opening_balance, 0.00)      as opening_balance,
    sum(case when A.hsl > 0 then A.hsl else 0.00 end) as debit_amount,
    sum(case when A.hsl < 0 then -A.hsl else 0.00 end) as credit_amount,
    coalesce(OB.opening_balance, 0.00) + 
     sum(case when A.hsl > 0 then A.hsl else 0.00 end) - 
     sum(case when A.hsl < 0 then -A.hsl else 0.00 end) as closing_balance,
    

    K.akont                                 as recon_account,
    G.txt50                                 as recon_account_name,
    max(A.budat)                            as last_transaction_date   ,
      /* Indicators for HAVING Clause  */
    coalesce(OB.opening_balance, 0.00)      as calc_opening_balance,
    sum(case when A.hsl > 0 then A.hsl else 0.00 end) as calc_debit_amount,
    sum(case when A.hsl < 0 then -A.hsl else 0.00 end) as calc_credit_amount
       
}
where K.bukrs = :p_company
  and A.budat between :p_from_date and :p_to_date
group by
    K.mandt, K.bukrs, K.lifnr, C.name1,
    ADR.street, ADR.str_suppl1, ADR.str_suppl2,
    ADR.location, ADR.city1, ADR.post_code1, ADR.city2,
    C.stcd3, C.j_1ipanno, C.regio, T5.bezei,
    K.akont, G.txt50, OB.opening_balance
having OB.opening_balance is not null and OB.opening_balance <> 0 
 ;
   /*
where K.bukrs = :p_company
  and A.budat between :p_from_date and :p_to_date
group by
 K.mandt, K.bukrs, K.lifnr, C.name1 ,
   ADR.street, ADR.str_suppl1, ADR.str_suppl2,
    ADR.location, ADR.city1, ADR.post_code1, ADR.city2,
    C.stcd3, C.j_1ipanno, C.regio, T5.bezei,
    K.akont, G.txt50, OB.opening_balance 
 having coalesce(OB.opening_balance, 0.00) <> 0 
   or sum(case when A.hsl > 0 then A.hsl else 0.00 end) <> 0 
   or abs(sum(case when A.hsl < 0 then A.hsl else 0.00 end)) <> 0 ;
 order by K.lifnr; */

