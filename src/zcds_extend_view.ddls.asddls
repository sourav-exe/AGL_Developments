@AbapCatalog.sqlViewAppendName: 'ZSQL_EXTEND_VIEW'
@EndUserText.label: 'CDS Extend View'
extend view zcds_association_2 with ZCDS_Extend_view
  association [*] to sbook as _booking on carrier.carrid = _booking.carrid
  //                                         carrier_id    = _booking.carrid
  //                                     and connection_id = _booking.connid
  //                                     and flight_date   = _booking.fldate
{
  carrier.carrname,
  _booking
}
