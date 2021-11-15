*"* use this source file for your ABAP unit test classes

CLASS lcl_iid_cust_swt_misc_utl DEFINITION FINAL FOR TESTING
                                DURATION SHORT
                                RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: go_cust_swt_misc_utl_cut TYPE REF TO zcl_iid_cust_swt_misc_utl,
          go_cust_swt_misc_td_dao  TYPE REF TO zif_iid_cust_swt_misc_dao.


    METHODS: setup,
      assert_condition_true,
      assert_condition_false,
      send_standard_letter FOR TESTING,
      set_supply_scn_new_tmd_success FOR TESTING,
      set_supply_scn_new_tmd_fail FOR TESTING.
ENDCLASS.

CLASS lcl_iid_cust_swt_misc_utl IMPLEMENTATION.
  METHOD setup.

    go_cust_swt_misc_td_dao ?= cl_abap_testdouble=>create( 'zif_iid_cust_swt_misc_dao' ).
    go_cust_swt_misc_utl_cut = NEW  zcl_iid_cust_swt_misc_utl( go_cust_swt_misc_td_dao ).

  ENDMETHOD.

  METHOD assert_condition_true.
    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_true ).
  ENDMETHOD.

  METHOD assert_condition_false.
    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_false ).
  ENDMETHOD.

  METHOD send_standard_letter.

    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( '0123456789'  ).
    go_cust_swt_misc_td_dao->get_contract_pod( 'internal_pod' ).

* Send Debt Object letter
    DATA(lv_letter_sent) = go_cust_swt_misc_utl_cut->zif_iid_cust_swt_misc_utl~send_standard_letter( iv_business_partner = '0000000001'
                                                                                                     iv_tempid           = 'DO'
                                                                                                     iv_contract         = ''
                                                                                                     iv_int_ui           = 'internal_pod' ).

    IF lv_letter_sent = abap_true.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_supply_scn_new_tmd_success.

    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( VALUE iederegscenario_ana( ( scenario = '005'
                                                                                                                                      datefrom = '20211031' ) ) ).
    go_cust_swt_misc_td_dao->get_supply_scenario( iv_int_pod = 'internal_pod' ).


    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( abap_true ).
    go_cust_swt_misc_td_dao->delete_non_billable_services( it_services = VALUE iederegscenarioserv( ( vertrag = 'contract'
                                                                                                      service = 'ST' "Service Type
                                                                                                      serviceid = 'SP' ) ) )." Service Provider


    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( abap_true ).
    go_cust_swt_misc_td_dao->modify_services( iv_int_pod = 'internal_pod'
                                              iv_supply_scenario = '005'
                                              iv_from_date       = '00000000'
*                                             iv_to_date         =
                                              it_serviceid       =  VALUE iederegservprov( (  ) ) ).

    DATA(lv_success) = go_cust_swt_misc_utl_cut->zif_iid_cust_swt_misc_utl~set_supply_scenario_new_tmd( iv_int_pod      = 'internal_pod'  " Internal key for point of delivery
                                                                                                        iv_move_in_date = '00000000' ).   " System Date

    cl_abap_unit_assert=>assert_equals( act   = lv_success
                                        exp  = abap_true ).
  ENDMETHOD.

  METHOD set_supply_scn_new_tmd_fail.
    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( VALUE iederegscenario_ana( ( scenario = '005'
                                                                                                                                      datefrom = '00000000' ) ) ).
    go_cust_swt_misc_td_dao->get_supply_scenario( iv_int_pod = 'internal_pod' ).


    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( abap_true ).
    go_cust_swt_misc_td_dao->delete_non_billable_services( it_services = VALUE iederegscenarioserv( ( vertrag = 'contract'
                                                                                                      service = 'ST' "Service Type
                                                                                                      serviceid = 'SP' ) ) )." Service Provider


    cl_abap_testdouble=>configure_call( go_cust_swt_misc_td_dao  )->ignore_all_parameters( )->returning( abap_true ).
    go_cust_swt_misc_td_dao->modify_services( iv_int_pod = 'internal_pod'
                                              iv_supply_scenario = '005'
                                              iv_from_date       = '00000000'
*                                             iv_to_date         =
                                              it_serviceid       =  VALUE iederegservprov( (  ) ) ).

    DATA(lv_success) = go_cust_swt_misc_utl_cut->zif_iid_cust_swt_misc_utl~set_supply_scenario_new_tmd( iv_int_pod      = 'internal_pod'  " Internal key for point of delivery
                                                                                                        iv_move_in_date = '00000000' ).   " System Date

    cl_abap_unit_assert=>assert_equals( act   = lv_success
                                        exp  = 'Y' ).
  ENDMETHOD.
ENDCLASS.
