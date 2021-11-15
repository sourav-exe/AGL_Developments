@AbapCatalog.sqlViewName: 'zsql_association'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS view to perform Association'
define view zcds_association as select from but000 as bp
association[0..1] to fkkvkp as _zfkkvkp on
_zfkkvkp.gpart = bp.partner
 {
    key bp.partner as business_partner,
    _zfkkvkp.vkont //Exposed Association
        
}
where bp.partner = '0100000142' or 
      bp.partner = '0100000123'