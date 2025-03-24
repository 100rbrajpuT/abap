@AbapCatalog.sqlViewName: 'ZCTE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Orders with Manifest Details'
@Metadata.ignorePropagatedAnnotations: true
define view zcds_cte as select from vbak as vb
    inner join zparamast as zp
      on zp.zvalue = vb.auart
         and zp.ztype = 'DOC_MANIFEST'
         and zp.zstatus = 'T'
         and zp.zsalorg = vb.vkorg
    left outer join lips as ps
      on ps.vgbel = vb.vbeln
    left outer join vttp as vt
      on vt.vbeln = ps.vbeln
    left outer join vttk as vk
      on vk.tknum = vt.tknum
{
  key vb.vkorg,
  key vb.vbeln
}
where
  vb.gbstk = 'A' and
     (vb.faksk is null or vb.faksk = '') and
    (vk.exti1 is not null and vk.exti1 <> '') and
     (vb.auart != 'ZMAD')
group by
  vb.vkorg,
  vb.vbeln;



  /* ******************************************** */
  @AbapCatalog.sqlViewName: 'ZCTE1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Stock Quantities CDS View'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_CTE1 as select from zstock_material as m
    left outer join ZCDS_CTE1_sub as k on k.plant = m.werks and k.location = m.lgort
{
    // Plant Code
    key m.werks as PlantCode,

    // Stock Quantities
    sum(m.labst) as StockUnrestricted,
    sum(m.insme) as StockQuality
}
where
    1 = 1  and
  --  k.status = '2' and --aready used in sub cds view ZCDS_CTE1_sub
 --   k.location = '' and
   (m.mtart = 'Z911' or m.mtart = 'Z921' or m.mtart = 'Z931') and
    m.meins = 'MT'
group by
    m.werks


    /* ******************************************* */
    @AbapCatalog.sqlViewName: 'ZCTE1SUB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZCDS_CTE1_zpp_location'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_CTE1_sub as select from zpp_location
{
    plant,
    location
}
where
    status = '2'

group by
    plant,
    location


    /* *************************** */

    @AbapCatalog.sqlViewName: 'ZCDS2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZCDS_CTE2_ZPARAMAST'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_CTE2 as select from zparamast {
    key zsalorg as SalesOrg,
    max(case when ztype = 'STOCK_RESTRICTION_QTY_VA01' then zvalue end) as RevisePlantCapacity,
    max(case when ztype = 'PLANT_STOCK_QTY' then zvalue end) as OriginalPlantCapacity
}
where
   (ztype = 'STOCK_RESTRICTION_QTY_VA01' or ztype = 'PLANT_STOCK_QTY')
    and zstatus = 'T'
group by
    zsalorg;

/* *****************************/





@AbapCatalog.sqlViewName: 'ZCDS_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SD_STOCK_CHECK'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_SD_STOCK_CHECK_MAIN as select from zcds_cte as c
    association [0..1] to vbap as _vbap
        on _vbap.vbeln = c.vbeln
       -- and ifnull(_vbap.FAKSP, '') = ''
      -- and coalesce(_vbap.faksp, '') = ''
    /*  and case
         when _vbap.faksp is null then ''
            else _vbap.faksp
        end = ''
        */
    association [0..1] to matdoc as _matdoc
        on _matdoc.ebeln = c.vbeln
    association [0..1] to ZCDS_CTE1 as _cte2
        on _cte2.PlantCode = c.vkorg    --werks
    association [0..1] to ZCDS_CTE2 as _cte3
        on _cte3.SalesOrg = c.vkorg
    association [0..1] to t001w as _t001w
        on _t001w.werks = c.vkorg
        and _t001w.mandt = '900'
{
    // Fields from the first subquery (x)
    key c.vkorg as Plant_Code,
       // Fields from T001W
    _t001w.name1 as Plant_Name,
    
    sum(_vbap.kwmeng) as Open_Manifest_Veh_Assignment,

    // Fields from the second subquery (y)
  --  sum(_matdoc.stock_qty) as Open_Manifest_Qty_Post,
  cast(sum(_matdoc.stock_qty) as abap.dec(15, 3)) as Open_Manifest_Qty_Post,

    // Fields from CTE2
    _cte2.StockUnrestricted as Stock_Unrestricted,   --StockUnrestricted
    _cte2.StockQuality as Stock_Quality,

    // Fields from CTE3
    _cte3.RevisePlantCapacity as Revise_Plant_Capacity,  --RevisePlantCapacity
    _cte3.OriginalPlantCapacity as Original_Plant_Capacity

 
}
group by
    c.vkorg,
    _cte2.StockUnrestricted,
    _cte2.StockQuality,
    _cte3.RevisePlantCapacity ,
    _cte3.OriginalPlantCapacity ,
    _t001w.name1;



    *****************************************************


    @AbapCatalog.sqlViewName: 'ZCDS_TESTMAIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'zcds_test_main'
@Metadata.ignorePropagatedAnnotations: true
define view zcds_test1 as select from zcds_test as c
     inner join vbap as vp 
        on vp.vbeln = c.vbeln
        and (vp.faksp is null or vp.faksp = '')
        {
         c.vkorg as PlantCode,
   -- tw.NAME1 as PlantName,
   -- x.OPEN_MANIFEST_VEH_ASSIGNMENT
    sum(case when vp.faksp is null or vp.faksp = '' then vp.kwmeng else 0 end) as Open_Manifest_Veh_Assignment
        }
    group by 
        c.vkorg




*********************************************************

@AbapCatalog.sqlViewName: 'ZCTE_MAIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Test SD stock'
@Metadata.ignorePropagatedAnnotations: true
define view zcds_test as select from vbak
  inner join zparamast as zp 
    on  zp.zvalue = vbak.auart
    and zp.ztype  = 'DOC_MANIFEST'
    and zp.zstatus = 'T'
    and zp.zsalorg = vbak.vkorg
  left outer join lips as ps 
    on ps.vgbel = vbak.vbeln
  left outer join vttp as vt 
    on vt.vbeln = ps.vbeln
  left outer join vttk as vk 
    on vk.tknum = vt.tknum
{
  key vbak.vkorg,
  key vbak.vbeln
}
where
  vbak.gbstk = 'A'
  and (vbak.faksk is null or vbak.faksk = '')
  and (vk.exti1 is not null and vk.exti1 <> '')
  and vbak.auart <> 'ZMAD'
group by
  vbak.vkorg,
  vbak.vbeln








  


