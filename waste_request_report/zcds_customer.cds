@AbapCatalog.sqlViewName: 'ZCDS_CUSTOMER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer ref and Log'
@Metadata.ignorePropagatedAnnotations: true
define view zcds_customer_reflog as select from  zcust_ref_log 
{
    item_id,
    emp_id,
    emp_name,
    sap_plant_code,
    customer_code,
    customer_name,
    region,
    waste_type,
    waste_name,
    waste_physical_status,
    packaging as packaging,
    texture,
    trucktype,
    remarks,
    request_dta,
    request_time,
    quentity,
    created_on,
    created_by
   
}
union 
select from zcustomer_ref
{
    item_id,
    emp_id,
    emp_name,
    sap_plant_code,
    customer_code,
    customer_name,
    region,
    waste_type,
    waste_name,
    waste_physical_status,
    packaging as packaging,
    texture,
    trucktype,
    remarks,
    request_dta,
    request_time,
    quentity,
    created_on,
    created_by
};
