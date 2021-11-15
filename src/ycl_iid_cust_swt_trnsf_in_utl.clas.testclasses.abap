
CLASS ltc_iid_cust_swt_trnsf_in_utl DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: go_cust_swt_trnsf_in_util_cut TYPE REF TO zcl_iid_cust_swt_trnsf_in_utl,  "class under test
          go_cust_swt_trnsf_in_dao_td   TYPE REF TO zif_iid_cust_swt_trnsf_in_dao,
          go_ft_dao_td                  TYPE REF TO zif_iid_faster_transfer_dao.

    METHODS: setup,
      assert_condition_true,
      assert_condition_false,
      get_current_pve_period FOR TESTING.

ENDCLASS.       "ltc_Iid_Cust_Swt_Trnsf_In_Utl


CLASS ltc_iid_cust_swt_trnsf_in_utl IMPLEMENTATION.

  METHOD setup.

    go_ft_dao_td = CAST #( cl_abap_testdouble=>create( 'zif_iid_faster_transfer_dao' ) ).
    go_cust_swt_trnsf_in_dao_td = CAST #( cl_abap_testdouble=>create( 'zif_iid_cust_swt_trnsf_in_dao' ) ).
    go_cust_swt_trnsf_in_util_cut = NEW zcl_iid_cust_swt_trnsf_in_utl( io_ft_unit_dao       = go_ft_dao_td
                                                                       io_cust_swt_unit_dao = go_cust_swt_trnsf_in_dao_td ).
  ENDMETHOD.

  METHOD assert_condition_true.

    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_true ).

  ENDMETHOD.

  METHOD assert_condition_false.

    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_false ).

  ENDMETHOD.

  METHOD get_current_pve_period.

    DATA(lt_pve_period) = VALUE ziiddt_pve_period( ( jurisdiction = 'VIC' fuel = '01' effective_date = '20210925' )
                                                   ( jurisdiction = 'VIC' fuel = '01' effective_date = '20220925' )
                                                   ( jurisdiction = 'VIC' fuel = '01' effective_date = '20230925' ) ).

    cl_abap_testdouble=>configure_call( go_cust_swt_trnsf_in_dao_td  )->ignore_all_parameters( )->returning( lt_pve_period  ).
    go_cust_swt_trnsf_in_dao_td->get_pve_period( iv_jurisdiction =     'VIC' " Jurisdiction
                                                 iv_division     =     '01' ). " Division'


    "Determine Current PVE period
    DATA(lv_current_pve_period) = go_cust_swt_trnsf_in_util_cut->zif_iid_cust_swt_trnsf_in_utl~get_current_pve_period( iv_jurisdiction = 'VIC'
                                                                                                                       iv_division     = '01' ).

    IF lv_current_pve_period IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
