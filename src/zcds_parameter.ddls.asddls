@AbapCatalog.sqlViewName: 'zsql_parameter'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS view to perform parameter function'
define view zcds_parameter with parameters p_partner1: BU_PARTNER, 
                                           p_partner2 : BU_PARTNER
as select from but000 
{
 key partner as business_partner,
 bpkind as partner_type,
 name_first as first_name,
 name_last as last_name,
 
 case bpkind
 when '0003' then 'Employee'
 when '0004' then 'Broker'
 when '9001' then 'Manager'
 else 'Others' 
 end as BP_Type,
 
 case 
 when partner <> :p_partner2 then 'X'
 else ''
 end as Active
 
}
where partner = $parameters.p_partner1 or
      partner = $parameters.p_partner2 