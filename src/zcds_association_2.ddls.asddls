@AbapCatalog.sqlViewName: 'ZSQL_ASSOC_2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
//@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Association'
define view zcds_association_2
  as select from scarr as carrier
  association [1..*] to spfli   as _schedule on carrier.carrid = _schedule.carrid
  association [1..*] to sflight as _flights  on carrier.carrid = _flights.carrid

{
  key carrier.carrid as carrier_id,
      //  key _schedule.connid as connection_id,
      //  key _flights.fldate  as flight_date,

      // Make association public
      _schedule,
      _flights
}
where
     carrid = 'DL'
  or carrid = 'AZ'
  or carrid = 'AA'
