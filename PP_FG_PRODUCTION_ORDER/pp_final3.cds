@AbapCatalog.sqlViewName: 'ZPRODUCTION'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
--@OData.publish: true
@EndUserText.label: 'CDS View for Production Order Report'
@Metadata.ignorePropagatedAnnotations: true
define view ZPP_FG_cds_prod_Order 
with parameters p_werks : werks_d , p_date_from : bldat, 
  p_date_to   : bldat,   p_matnr : matnr
 as select from  aufm as A
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
  cast(Z.ph as abap.dec(5,3)) as PH_Cast,  // Cast the value of Z.ph
  max(cast(Z.ph as abap.dec(5,3))) as PH,  // Corrected casting for PH
  cast(Z.gcv as abap.dec(5,3)) as GCV_Cast, // Cast the value of Z.gcv
  max(cast(Z.gcv as abap.dec(5,3))) as GCV, // Corrected casting for GCV
  cast(Z.loi as abap.dec(5,3)) as LOI_Cast, // Cast the value of Z.loi
  max(cast(Z.loi as abap.dec(5,3))) as LOI, // Corrected casting for LOI
  cast(Z.cl as abap.dec(5,3)) as CL_Cast,   // Cast the value of Z.cl
  max(cast(Z.cl as abap.dec(5,3))) as CL,   // Corrected casting for CL
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

max(QP.kurztext) as MATERIALEASINESS ,
  /*
  case 
    when total_input = 0 then 0
    else ROUND(((total_input - total_output) / total_input) * 100, 2)
  end as scrap_percentage,

  // KPI 2: BINDERS_PERCENTAGE = BINDERS / OUTPUT * 100
  case 
    when total_output = 0 then 0
    else ROUND((binders / total_output) * 100, 2)
  end as binders_percentage,

  // KPI 3: MOISTURE_LOSS = (INPUT - INPUT_WO_WATER) / INPUT * 100
  case 
    when total_input = 0 then 0
    else ROUND(((total_input - total_input_without_water) / total_input) * 100, 2)
  end as moisture_loss,
  */

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
 and A.werks = :p_werks
  and A.matnr = :p_matnr
 and A.bldat between :p_date_from and :p_date_to
group by 
  A.werks, 
  A.aufnr, 
  A.bldat, 
  QE.vcode, 
  A.matnr ;
