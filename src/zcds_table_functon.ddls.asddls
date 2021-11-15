@EndUserText.label: 'CDS Table Function'
//@ClientHandling.type: #CLIENT_INDEPENDENT
define table function zcds_table_functon
with parameters p_select_options :abap.char( 1000 )
returns {
  client                    :abap.clnt;
  purchasing_document       :ebeln;
  company_code              :bukrs;
  document_category         :bstyp;
  document_type             :bsart;
  status                    :statu;
  creation_date             :aedat;
  creation_date_formatted   :abap.char( 10 );
  creator                   :ernam;
  vendor_name               :bu_nameor1;
  
}
implemented by method zcl_amdp=>get_po_head_table_funct;