@AbapCatalog.sqlViewName: 'zsql_join'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS view to show join'
define view Zcds_join as select from but000 as a
left outer join fkkvkp as b on b.gpart = a.partner
 {
    key a.partner as business_partner,
        b.vkont as contract_account
}
where a.partner = '0100000142' or 
      a.partner = '0100000123'
