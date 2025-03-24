@AbapCatalog.sqlViewName: 'ZPRODUCTION'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View for Production Order Report'
@Metadata.ignorePropagatedAnnotations: true
define view ZPP_FG_Production_Order 
 with parameters
    p_plant  : werks_d,   // Plant parameter
    p_matnr  : matnr_d     // Material parameter
as select from aufm as A
  left join afpo as P on A.mandt = P.mandt and A.werks = P.dwerk and A.aufnr = P.aufnr
  left join afko as KO on P.mandt = KO.mandt and P.aufnr = KO.aufnr
  inner join afvc as AF on AF.mandt = KO.mandt and KO.aufpl = AF.aufpl
  inner join crhd as CH on AF.mandt = CH.mandt and AF.arbid = CH.objid
  inner join crtx as CR on CR.mandt = CR.mandt and CR.objid = CH.objid
  left join (select * from mcha where lifnr <> '') as MC on A.mandt = P.mandt and A.charg = MC.charg and A.werks = MC.werks
  left join (select * from ausp where atinn = '0000000909') as AU on MC.mandt = AU.mandt and MC.cuobj_bm = AU.objek
  left join (select * from qpct where katalogart = '1' and sprache = 'E') as QP on QP.mandt = AU.mandt and QP.codegruppe = left(AU.atwrt, 7) and QP.code = right(AU.atwrt, 2)
  left join (select row_number() over (partition by werks, aufnr order by werks, aufnr, isdd desc) as srno, * from afru where stokz = '' and stzhl = '00000000') as AFA on AFA.mandt = A.mandt and AFA.werks = A.werks and AFA.aufnr = A.aufnr and AFA.srno = 1
  left join zibatchcharinfo as Z on Z.mandt = A.mandt and Z.werks = A.werks and Z.matnr = A.matnr and Z.charg = A.charg and A.bwart in ('101', '531')
  left join qals as QA on A.aufnr = QA.aufnr
  left join qave as QE on QA.prueflos = QE.prueflos
  left join plpo as L on KO.plnnr = L.plnnr and KO.zaehl = L.zaehl
{
  A.werks as plant,
  max(P.matnr) as productname,
  A.aufnr as productionorder,
  max(CH.arbpl) as workcentercode,
  max(CR.ktext) as workcentername,
  to_char(A.bldat, 'DD/MM/YYYY') as postingdate,
  sum(case when A.charg like 'W%' and substring(A.matnr, 13, 6) like '%SD%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as solid,
  sum(case when A.charg like 'W%' and substring(A.matnr, 13, 6) like '%SS%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as semisolid,
  sum(case when A.charg like 'W%' and substring(A.matnr, 13, 6) like '%LQ%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as liquid,
  sum(case when A.charg like 'W%' and substring(A.matnr, 13, 6) like '%AQ%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as aqs,
  sum(case when A.matnr like '000000%' and A.bwart in ('261', '262') and A.erfme = 'MT' then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as binder,
  max(Z.charg) as batch,
  max(Z.physicalstate) as physicalstate,
  max(Z.texture) as texture,
  max(Z.color) as color,
  max(Z.odour) as odour,
  max(Z.ph) as ph,
  max(Z.gcv) as gcv,
  max(Z.loi) as loi,
  max(Z.cl) as cl,
  max(Z.sulphur) as sulphur,
  max(Z.moi) as moi,
  max(Z.ash) as ash,
  max(Z.pkgmode) as pkgmode,
  max(Z.contno) as contno,
  max(Z.compatibilitygroup) as compatibilitygroup,
  max(Z.safetycategory) as safetycategory,
  max(Z.processcategory) as processcategory,
  max(Z.processroute) as processroute,
  max(Z.preprocessroute) as preprocessroute,
  to_char(AFA.isdd, 'DD/MM/YYYY') || ' ' || to_time(AFA.isdz, 'HH24MISS') as startdate,
  to_char(AFA.isdd, 'YYYY-MM-DD') || ' ' || to_time(AFA.isdz, 'HH24MISS') as startdate1,
  to_char(AFA.iedd, 'DD/MM/YYYY') || ' ' || to_time(AFA.iedz, 'HH24MISS') as finishdate,
  to_char(AFA.iedd, 'YYYY-MM-DD') || ' ' || to_time(AFA.iedz, 'HH24MISS') as finishdate1,
  cast(sum(case when A.charg like 'W%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as waste,
  cast(sum(case when A.charg like 'I%' and A.matnr not like 'RESIDUE%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as intermediate,
  cast(sum(case when A.charg like 'F%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as fgreconsumed,
  cast(sum(case when A.matnr like 'RESIDUE%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as residueconsumed,
  cast(sum(case when A.matnr like '000000%' and A.bwart in ('261', '262') and A.erfme = 'MT' then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as binders,
  cast(sum(case when A.charg like 'W%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.charg like 'I%' and A.matnr not like 'RESIDUE%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.charg like 'F%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.matnr like 'RESIDUE%' and A.bwart in ('261', '262') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.matnr like '000000%' and A.bwart in ('261', '262') and A.erfme = 'MT' then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as totalinput,
  cast(sum(case when A.bwart in ('101', '102') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as fgoutput,
  cast(sum(case when A.matnr like 'RESIDUE%' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as residuereceipt,
  cast(sum(case when A.bwart in ('101', '102') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.matnr like 'RESIDUE%' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as totaloutput,
  cast(sum(case when A.matnr like '000000%' and A.bwart in ('261') and A.meins = 'EA' then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as jumbobag,
  cast(sum(case when A.matnr not like 'BG%' and A.meins = 'EA' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as packingscrapdrums,
  cast(sum(case when A.matnr like 'BG%' and A.meins = 'EA' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 3)) as packingscrapbags,
  cast(sum(case when A.matnr not like 'BG%' and A.meins = 'EA' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) +
       sum(case when A.matnr like 'BG%' and A.meins = 'EA' and A.bwart in ('531', '532') then case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end else 0 end) as numeric(18, 2)) as totalscrap,
  max(QP.kurztext) as materialeasiness,
  QE.vcode,
  case when QE.vcode = 'A1' then 'ACCEPTED'
       when QE.vcode = 'A2' then 'ACCEPTED WITH DEVIATION'
       when QE.vcode = 'A3' then 'ACCEPTED PARTIAL QUANTITY'
       when QE.vcode = 'R' then 'REJECTED'
       else ' ' end as code_description
}
where ( A.bwart in ('261', '262', '531', '532', '101', '102') )
  and ( :p_plant is null or A.werks = :p_plant ) -- Fetch all data if p_plant is null
  and ( :p_matnr is null or P.matnr = :p_matnr ) -- Fetch all data if p_matnr is null
group by A.werks, A.aufnr, A.bldat, QE.vcode, A.matnr