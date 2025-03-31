@AbapCatalog.sqlViewName: 'ZPRODUCTION'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog.dataMaintenance : true
@EndUserText.label: 'CDS View for Production Order Report'
@Metadata.ignorePropagatedAnnotations: true
define view ZPP_FG_Production_Order 
/* with parameters
    p_plant  : werks_d,   // Plant parameter
    p_matnr  : matnr_d     // Material parameter
    */
as select from aufm as A
  left outer join afpo as P
    on A.mandt = P.mandt and A.werks = P.dwerk and A.aufnr = P.aufnr  
  left outer join afko as KO
    on P.mandt = KO.mandt and P.aufnr = KO.aufnr
  inner join afvc as AF
    on AF.mandt = KO.mandt and KO.aufpl = AF.aufpl
  inner join crhd as CH
    on AF.mandt = CH.mandt and AF.arbid = CH.objid
  inner join crtx as CR
    on CR.mandt = CH.mandt and CR.objid = CH.objid
  left outer join mcha as MC
    on A.mandt = MC.mandt and A.charg = MC.charg and A.werks = MC.werks
  left outer join ausp as AU
    on MC.mandt = AU.mandt and MC.cuobj_bm = AU.objek
  left outer join qpct as QP
    on QP.mandt = AU.mandt and QP.codegruppe = substring(AU.atwrt, 1, 7) and QP.code = substring(AU.atwrt, 7, 2)
  left outer join afru as AFA
    on AFA.mandt = A.mandt and AFA.werks = A.werks and AFA.aufnr = A.aufnr
  left outer join zibatchcharinfo as Z
    on Z.mandt = A.mandt and Z.werks = A.werks and Z.matnr = A.matnr and Z.charg = A.charg
  left outer join qals as QA
    on A.aufnr = QA.aufnr
  left outer join qave as QE
    on QA.prueflos = QE.prueflos
{
  key A.werks as Plant,
  max(P.matnr) as ProductName,
  A.aufnr as ProductionOrder,
  max(CH.arbpl) as WorkCenterCode,
  max(CR.ktext) as WorkCenterName,
  A.bldat as PostingDate, 
--  'DD/MM/YYYY'  as PostingDate,
    substring(A.matnr, 13, 6) as Matnr_Substring, // Calculated field for substring
  sum(
    case 
      when A.charg like 'W%' 
           and A.matnr like '%SD%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg = 'S' 
        then A.erfmg
      when A.charg like 'W%' 
           and A.matnr like '%SD%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg <> 'S' 
        then A.erfmg * -1
      else 0 
    end
  ) as Solid,
  sum(
    case 
      when A.charg like 'W%' 
           and A.matnr like '%SS%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg = 'S' 
        then A.erfmg
      when A.charg like 'W%' 
           and A.matnr like '%SS%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg <> 'S' 
        then A.erfmg * -1
      else 0 
    end
  ) as SemiSolid,
  sum(
    case 
      when A.charg like 'W%' 
           and A.matnr like '%LQ%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg = 'S' 
        then A.erfmg
      when A.charg like 'W%' 
           and A.matnr like '%LQ%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg <> 'S' 
        then A.erfmg * -1
      else 0 
    end
  ) as Liquid,
  sum(
    case 
      when A.charg like 'W%' 
           and A.matnr like '%AQ%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg = 'S' 
        then A.erfmg
      when A.charg like 'W%' 
           and A.matnr like '%AQ%' // Use the calculated field here
           and (A.bwart = '261' or A.bwart = '262') 
           and A.shkzg <> 'S' 
        then A.erfmg * -1
      else 0 
    end
  ) as Aqs,
  max(Z.charg) as Batch,
  max(Z.physicalstate) as PhysicalState,
  max(Z.texture) as Texture,
  max(Z.color) as Color,
  max(Z.odour) as Odour,
  max(Z.ph) as PH,
  max(Z.gcv) as GCV,
  max(Z.loi) as LOI,
  max(Z.cl) as CL,
  max(Z.sulphur) as Sulphur,
  max(Z.moi) as MOI,
  max(Z.ash) as Ash,
  max(Z.pkgmode) as PkgMode,
  max(Z.contno) as ContNo,
  max(Z.compatibilitygroup) as CompatibilityGroup,
  max(Z.safetycategory) as SafetyCategory,
  max(Z.processcategory) as ProcessCategory,
  max(Z.processroute) as ProcessRoute,
  max(Z.preprocessroute) as PreProcessRoute,
    cast(
        sum(
          case 
            when A.charg like 'W%' and (A.bwart = '261'or A.bwart = '262') 
            then 
              case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end
            else 0 
          end
        ) as  abap.int4(10)
      ) as WASTE,
   cast(
    sum(
      case 
        when A.charg like 'I%' 
             and A.matnr not like 'RESIDUE%' 
             and (A.bwart = '261' or A.bwart = '262') 
        then 
          case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end
        else 0 
      end
    ) as abap.dec(18,3)
) as INTERMEDIATE,
cast(
    sum(
      case 
        when A.charg like 'F%' 
             and (A.bwart = '261' or A.bwart = '262') 
        then 
          case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end
        else 0 
      end
    ) as abap.dec(18,3)
) as FGRECONSUMED,

cast(
    sum(
      case 
        when A.matnr like 'RESIDUE%' 
             and (A.bwart = '261' or A.bwart = '262') 
        then 
          case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end
        else 0 
      end
    ) as abap.dec(18,3)
) as RESIDUECONSUMED,
cast(
    sum(
      case 
        when A.matnr like '000000%' 
             and (A.bwart = '261' or A.bwart = '262') 
             and A.erfme = 'MT' 
        then 
          case when A.shkzg = 'S' then A.erfmg else A.erfmg * -1 end
        else 0 
      end
    ) as abap.dec(18,3)
) as BINDERS ,

  QE.vcode as VCode,
  case QE.vcode
    when 'A1' then 'ACCEPTED'
    when 'A2' then 'ACCEPTED WITH DEVIATION'
    when 'A3' then 'ACCEPTED PARTIAL QUANTITY'
    when 'R' then 'REJECTED'
    else '' end as CodeDescription
}
where 
( A.bwart ='261' or  A.bwart = '262' or  A.bwart = '531' or  A.bwart = '532' or  A.bwart =  '101' or  A.bwart = '102')
group by A.werks, A.aufnr, A.bldat, QE.vcode , A.matnr ;
