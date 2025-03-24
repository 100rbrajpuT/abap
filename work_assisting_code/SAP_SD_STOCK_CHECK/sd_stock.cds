@AbapCatalog.sqlViewName: 'ZCDS_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SD_STOCK_CHECK'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_SD_STOCK_CHECK_MAIN as select from zcds_cte as c
  /*  inner join  vbap as _vbap
  -- association [0..1] to vbap as _vbap
        on _vbap.vbeln = c.vbeln  
        and (_vbap.faksp is  null or _vbap.faksp = '' )
        */
     --   inner join zcds_vbak as vbap
      association [0..1] to zcds_vbak as _vbap
         on _vbap.Plant_Code =   c.vkorg
      
            
      -- left outer join  matdoc as _matdoc
      -- inner join  matdoc as _matdoc
  association [0..1] to matdoc as _matdoc
        on _matdoc.ebeln = c.vbeln
    association [0..1] to ZCDS_CTE1 as _cte2
        on _cte2.PlantCode = c.vkorg    --werks
    association [0..1] to ZCDS_CTE2 as _cte3
        on _cte3.SalesOrg = c.vkorg
    association [0..1] to t001w as _t001w
        on _t001w.werks = c.vkorg
     --  and _t001w.mandt = '900'
       
       
{
    // Fields from the first subquery (x)
    key c.vkorg as Plant_Code,
       // Fields from T001W
    _t001w.name1 as Plant_Name,
    
    --sum(_vbap.kwmeng) as Open_Manifest_Veh_Assignment,
--   sum(case when _vbap.faksp is null or _vbap.faksp = '' then _vbap.kwmeng else 0 end) as Open_Manifest_Veh_Assignment  ,
   _vbap.Open_Manifest_Veh_Assignment ,
   // Fields from the second subquery (y)
  --  sum(_matdoc.stock_qty) as Open_Manifest_Qty_Post,
   cast(sum(_matdoc.stock_qty) as abap.dec(15, 3)) as Open_Manifest_Qty_Post ,
       -- sum(_matdoc.stock_qty)  as Open_Manifest_Qty_Post --,

    // Fields from CTE2
    _cte2.StockUnrestricted as Stock_Unrestricted,   --StockUnrestricted
    _cte2.StockQuality as Stock_Quality,
    
     _cte2.StockUnrestricted + _cte2.StockQuality as Projected_Stock,

    // Fields from CTE3
    _cte3.RevisePlantCapacity as Revise_Plant_Capacity,  --RevisePlantCapacity
    _cte3.OriginalPlantCapacity as Original_Plant_Capacity


 
}

   -- _vbap.faksp is null
  -- (_vbap.faksp is null or _vbap.faksp = '')
group by
    c.vkorg,
 --   _vbap.kwmeng,
    _vbap.Open_Manifest_Veh_Assignment,
    _cte2.StockUnrestricted,
    _cte2.StockQuality,
    _cte3.RevisePlantCapacity ,
    _cte3.OriginalPlantCapacity ,
    _t001w.name1;



************************************************
@AbapCatalog.sqlViewName: 'ZCDSVBAK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'zcte_vbak'
@Metadata.ignorePropagatedAnnotations: true
define view zcds_vbak as select from zcds_cte as c
inner join  vbap as _vbap
  -- association [0..1] to vbap as _vbap
        on _vbap.vbeln = c.vbeln  
        and (_vbap.faksp is  null or _vbap.faksp = '' )
            
{
      key c.vkorg as Plant_Code,
       // Fields from T001W
    --_t001w.name1 as Plant_Name,
    
    --sum(_vbap.kwmeng) as Open_Manifest_Veh_Assignment,
   sum(case when _vbap.faksp is null or _vbap.faksp = '' then _vbap.kwmeng else 0 end) as Open_Manifest_Veh_Assignment  
}
group by
   c.vkorg;
