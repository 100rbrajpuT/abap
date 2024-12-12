@AbapCatalog.sqlViewName: 'ZSAP_VEND_TRIAL_BAL'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SAP Vendor Trial Balance'
define view ZSAP_VENDOR_TRIAL_BALANCE 
  with parameters 
    @PARAMETER: COMPANY: String(4) default '%' 
    @PARAMETER: PLANT: String(4) default '%'
    @PARAMETER: FROM_DATE: Dats 
    @PARAMETER: TO_DATE: Dats
    @PARAMETER: GLACCNO: String(50) default '%'
as select from lfb1 as k
  left join lfa1 as c on k.lifnr = c.lifnr
  left join adrc as adr on adr.addrnumber = c.adrnr
  left join but000 as m on c.lifnr = m.partner
  left join t005u as t5 on t5.bland = c.regio and t5.spras = 'E' and t5.land1 = 'IN' and t5.mandt = '900'
  left join acdoca as a on k.lifnr = a.lifnr and a.rbukrs = k.bukrs
  left join (select lifnr, rbukrs, sum(hsl) as opnamt
             from acdoca
             where belnr not like 'B%' and koart = 'K'
             group by lifnr, rbukrs) as b on k.lifnr = b.lifnr and k.bukrs = b.rbukrs
  left join (select lifnr, rbukrs, max(budat) as last_tran_dat
             from acdoca
             where belnr not like 'B%' and koart = 'K'
             and blart not in ('AB')
             group by lifnr, rbukrs) as ac on k.lifnr = ac.lifnr and k.bukrs = ac.rbukrs
  left join t001w as p on p.werks = :PLANT
  left join skat as g on k.akont = g.saknr
  left join skat as ga on ga.saknr = :GLACCNO
where k.bukrs like :COMPANY
and a.budat between :FROM_DATE and :TO_DATE
group by k.lifnr, b.opnamt, ac.last_tran_dat, k.akont, g.txt50
having (ifnull(b.opnamt, 0.00) <> 0 or sum(case when a.hsl > 0 then a.hsl else 0.00 end) <> 0 or abs(sum(case when a.hsl < 0 then a.hsl else 0.00 end)) <> 0)
