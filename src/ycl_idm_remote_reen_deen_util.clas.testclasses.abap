*"* use this source file for your ABAP unit test classes

CLASS lcl_idm_remote_reen_deen_util DEFINITION FINAL FOR TESTING
                                 DURATION SHORT
                                 RISK LEVEL HARMLESS
                                 FRIENDS zcl_idm_remote_reen_deen_util.


  PRIVATE SECTION.

    METHODS: setup,
      assert_condition_true,
      assert_condition_false,
      check_ell_4_deen FOR TESTING,
      get_data_mpb_false FOR TESTING,
      get_data_mpb_false_1 FOR TESTING,
      add_activity_in_sn FOR TESTING,
      add_activity_in_sn_1 FOR TESTING,
      set_mpb_for_reen_deen_1 FOR TESTING,
      set_mpb_for_reen_deen_2 FOR TESTING,
      set_mpb_for_reen_deen_3 FOR TESTING,
      set_mpb_for_reen_deen_4 FOR TESTING,
      set_mpb_for_reen_deen_5 FOR TESTING,
      create_contact_note_test FOR TESTING,
      determine_sn_det FOR TESTING,
      determine_sn_det_1 FOR TESTING,
      determine_sn_det_2 FOR TESTING,
      determine_sn_det_3 FOR TESTING,
      determine_sn_det_4 FOR TESTING,
      determine_sn_det_5 FOR TESTING,
      determine_sn_det_6 FOR TESTING,
      check_2nd_sn_move_in FOR TESTING,
      check_2nd_sn_transfer FOR TESTING,
      check_dereg_satatus FOR TESTING,
      append_text_in_spcl_instr FOR TESTING,
      determ_nmi_mtr_stat FOR TESTING.

    DATA: go_reen_deen_util TYPE REF TO zcl_idm_remote_reen_deen_util,
          go_reen_deen_dao  TYPE REF TO zif_idm_remote_reen_deen_dao,
          go_sntl_dao       TYPE REF TO zif_iid_sntl_dao,
          go_crr_dao        TYPE REF TO zif_iid_crr_db_process.

ENDCLASS.

CLASS lcl_idm_remote_reen_deen_util IMPLEMENTATION.
  METHOD setup.
    go_reen_deen_util = NEW  zcl_idm_remote_reen_deen_util( ).
    go_reen_deen_dao ?= cl_abap_testdouble=>create( 'zif_idm_remote_reen_deen_dao' ).
    go_sntl_dao ?= cl_abap_testdouble=>create( 'zif_iid_sntl_dao' ).
    go_crr_dao ?= cl_abap_testdouble=>create( 'zif_iid_crr_db_process' ).

  ENDMETHOD.
  METHOD assert_condition_true.
    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_true ).
  ENDMETHOD.

  METHOD assert_condition_false.
    cl_abap_unit_assert=>assert_true( EXPORTING act = abap_false ).
  ENDMETHOD.

  METHOD check_ell_4_deen.

    DATA: lt_deen_data    TYPE zicatt318,
          lt_activity_tab TYPE tab_wqmma,
          ls_viqmel       TYPE viqmel.

    ls_viqmel-zzanlage =  '1234567898' .
    ls_viqmel-qmgrp = 'ELE-'.
    ls_viqmel-qmcod = '0028'.
    ls_viqmel-zzreceiver = 'MPB'.

    APPEND VALUE #( fieldvalue1 = 'ELE-' fieldvalue2 = '0028' ) TO lt_deen_data[].

    APPEND VALUE #( mngrp = 'ELE-' mncod = '0234' ) TO lt_activity_tab[].

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_deen_data ) .

    go_reen_deen_dao->fetch_deen_type_brf( EXPORTING iv_trans_id = 'DEEN_SN' ).

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'ghkkhha7689' ) .
    go_sntl_dao->get_internal_ui( EXPORTING iv_installation = '1234567898' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->set_parameter(
                                             name = 'EV_CODE_GRP' value = 'ELE-' )->set_parameter(
                                               name = 'EV_TYPE_CODE' value = '0027' )->set_parameter(
                                               name = 'EV_RECI_CODE' value = 'LNSP' )->set_parameter(
                                               name = 'EV_EXCEPTION' value = ' ').


    go_reen_deen_dao->get_deen_type_code( EXPORTING iv_anlage = '300987665'
                                                    iv_int_ui = 'hjguubbs768n'
                                                     iv_jurs = 'VIC' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'LNSP' ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui = 'hjguubbs768n'
                                                       iv_service_type = 'ABCD' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~check_elligibile_4_remote_deen(
                                                                    EXPORTING is_viqmel         = ls_viqmel
                                                                              it_activity_tab   = lt_activity_tab
                                                                              iv_jurisdiction   = 'VIC'
                                                                              io_reen_deen_dao  = go_reen_deen_dao
                                                                              io_sntl_dao       = go_sntl_dao
                                                                    IMPORTING ev_error          = DATA(lv_err) ).

    IF lv_err IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD get_data_mpb_false .

    DATA: lt_deen_data TYPE zidmtt_deen_meter.

    APPEND VALUE #( state = 'VIC' mpb_active = ' ' ord_type = 'ELE-' ) TO lt_deen_data[].

    go_reen_deen_util->zif_idm_remote_reen_deen_util~get_data_with_mpb_false( EXPORTING it_deen_det = lt_deen_data
                                                                                        iv_state    = 'VIC'
                                                                              IMPORTING ev_code_grp = DATA(lv_code_grp) ).

    IF lv_code_grp IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD get_data_mpb_false_1.

    DATA: lt_deen_data TYPE zidmtt_deen_meter.

    APPEND VALUE #( state = '*' mpb_active = ' ' ord_type = 'ELE-' ) TO lt_deen_data[].

    go_reen_deen_util->zif_idm_remote_reen_deen_util~get_data_with_mpb_false( EXPORTING it_deen_det = lt_deen_data
                                                                                        iv_state    = 'VIC'
                                                                              IMPORTING ev_code_grp = DATA(lv_code_grp) ).

    IF lv_code_grp IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.
  METHOD add_activity_in_sn.

    DATA: ls_sn_det      TYPE zidmst_sn_det,
          lt_sn_activity TYPE zidmdt_sn_activity,
          lt_return      TYPE bapiret2_t,
          lt_notifactv   TYPE alm_me_bapi2080_notactvi_t.

    ls_sn_det-qmnum = '0282229'.
    ls_sn_det-objnr = 'fdghh6'.

    APPEND VALUE #( qmnum = '09119' manum = '001' mngrp = 'ELE-' mncod = '0234' ) TO lt_sn_activity[].

    APPEND VALUE #( type = 'S' id = '245' message = 'Success' ) TO lt_return.
    "Fetch SN details
    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( ls_sn_det ) .
    go_reen_deen_dao->fetch_sn_details( EXPORTING iv_sn = '23456666' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_sn_activity ) .
    go_reen_deen_dao->fetch_activity_no( EXPORTING iv_sn = '7626781' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_return ).
    go_reen_deen_dao->add_activity( EXPORTING iv_serv_notif = '78791791'
                                    CHANGING  ct_notif_activity = lt_notifactv ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( abap_true ) .
    go_reen_deen_dao->save_service_notification( EXPORTING iv_sn = '7272829' ).

    DATA(lv_success) = go_reen_deen_util->zif_idm_remote_reen_deen_util~add_activity_in_serv_notif( EXPORTING iv_child_sn = '2992020'
                                                                                                              iv_parent_sn = '9288292'
                                                                                                              iv_copy_activity = abap_true
                                                                                                              iv_qmgrp = 'ELE-'
                                                                                                              iv_qmcode = '0234'
                                                                                                              io_reen_deen_dao = go_reen_deen_dao ).
    IF lv_success IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD add_activity_in_sn_1 .

    DATA: ls_sn_det      TYPE zidmst_sn_det,
          lt_sn_activity TYPE zidmdt_sn_activity,
          lt_return      TYPE bapiret2_t,
          lt_notifactv   TYPE alm_me_bapi2080_notactvi_t.

    ls_sn_det-qmnum = '0282229'.
    ls_sn_det-objnr = 'fdghh6'.

    APPEND VALUE #( type = 'S' id = '245' message = 'Success' ) TO lt_return.
    "Fetch SN details
    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( ls_sn_det ) .
    go_reen_deen_dao->fetch_sn_details( EXPORTING iv_sn = '23456666' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_sn_activity ) .
    go_reen_deen_dao->fetch_activity_no( EXPORTING iv_sn = '7626781' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_return ).
    go_reen_deen_dao->add_activity( EXPORTING iv_serv_notif = '78791791'
                                    CHANGING  ct_notif_activity = lt_notifactv ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( abap_true ) .
    go_reen_deen_dao->save_service_notification( EXPORTING iv_sn = '7272829' ).

    DATA(lv_success) = go_reen_deen_util->zif_idm_remote_reen_deen_util~add_activity_in_serv_notif( EXPORTING iv_child_sn = '2992020'
                                                                                                              iv_parent_sn = '9288292'
                                                                                                              iv_qmgrp = 'ELE-'
                                                                                                              iv_qmcode = '0234'
                                                                                                              io_reen_deen_dao = go_reen_deen_dao ).
    IF lv_success IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_mpb_for_reen_deen_1 .

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'LARGE' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    DATA(lv_mpb_active) = go_reen_deen_util->zif_idm_remote_reen_deen_util~set_mpb_for_reen_move_in( EXPORTING io_crr_dao         = go_crr_dao
                                                                                                               io_reen_deen_dao   = go_reen_deen_dao
                                                                                                               iv_int_ui          = 'shjhsh'
                                                                                                               iv_mpb_active      = abap_true
                                                                                                               iv_primary_qmcod   = '0017'
                                                                                                               iv_primary_qmgrp   = 'ELE'
                                                                                                               iv_secon_qmcod     = '0015'
                                                                                                               iv_secon_qmgrp     = 'ELE'
                                                                                                               iv_serv_prov_prim  = '1422561'
                                                                                                               iv_serv_prov_secon = '272792'
                                                                                                               iv_scenario        = 'MOVE-IN' ).
    IF lv_mpb_active IS INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_mpb_for_reen_deen_2.

    DATA: lt_mpb_brf TYPE t_serviceid,
          lv_mpb_brf TYPE service_prov.

    lv_mpb_brf = 'ACTIVEMP'.
    APPEND lv_mpb_brf TO lt_mpb_brf.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_crr_dao )->ignore_all_parameters( )->returning( 'VIC' ) .
    go_crr_dao->get_jurisdiction( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    DATA(lv_mpb_active) = go_reen_deen_util->zif_idm_remote_reen_deen_util~set_mpb_for_reen_move_in( EXPORTING io_crr_dao         = go_crr_dao
                                                                                                                  io_reen_deen_dao   = go_reen_deen_dao
                                                                                                                  iv_int_ui          = 'shjhsh'
                                                                                                                  iv_mpb_active      = abap_true
                                                                                                                  iv_primary_qmcod   = '0015'
                                                                                                                  iv_primary_qmgrp   = 'ELE'
                                                                                                                  iv_secon_qmcod     = '0017'
                                                                                                                  iv_secon_qmgrp     = 'ELE'
                                                                                                                  iv_serv_prov_prim  = 'ACTIVEMP'
                                                                                                                  iv_serv_prov_secon = 'LNSP'
                                                                                                                  iv_scenario        = 'MOVE-IN' ).
    IF lv_mpb_active IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_mpb_for_reen_deen_3.

    DATA: lt_mpb_brf TYPE t_serviceid,
          lv_mpb_brf TYPE service_prov.

    lv_mpb_brf = 'ACTIVE'.
    APPEND lv_mpb_brf TO lt_mpb_brf.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_crr_dao )->ignore_all_parameters( )->returning( 'VIC' ) .
    go_crr_dao->get_jurisdiction( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    DATA(lv_mpb_active) = go_reen_deen_util->zif_idm_remote_reen_deen_util~set_mpb_for_reen_move_in( EXPORTING io_crr_dao         = go_crr_dao
                                                                                                                  io_reen_deen_dao   = go_reen_deen_dao
                                                                                                                  iv_int_ui          = 'shjhsh'
                                                                                                                  iv_mpb_active      = abap_true
                                                                                                                  iv_primary_qmcod   = '0015'
                                                                                                                  iv_primary_qmgrp   = 'ELE'
                                                                                                                  iv_secon_qmcod     = '0017'
                                                                                                                  iv_secon_qmgrp     = 'ELE'
                                                                                                                  iv_serv_prov_prim  = 'ACTIVEMP'
                                                                                                                  iv_serv_prov_secon = 'LNSP'
                                                                                                                  iv_scenario        = 'MOVE-IN' ).
    IF lv_mpb_active IS INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.


  ENDMETHOD.

  METHOD set_mpb_for_reen_deen_4.

    DATA: lt_mpb_brf TYPE t_serviceid,
          lv_mpb_brf TYPE service_prov.

    lv_mpb_brf = 'ACTIVEMP'.
    APPEND lv_mpb_brf TO lt_mpb_brf.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_crr_dao )->ignore_all_parameters( )->returning( 'VIC' ) .
    go_crr_dao->get_jurisdiction( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    DATA(lv_mpb_active) = go_reen_deen_util->zif_idm_remote_reen_deen_util~set_mpb_for_reen_move_in( EXPORTING io_crr_dao         = go_crr_dao
                                                                                                                  io_reen_deen_dao   = go_reen_deen_dao
                                                                                                                  iv_int_ui          = 'shjhsh'
                                                                                                                  iv_mpb_active      = abap_true
                                                                                                                  iv_primary_qmcod   = '0017'
                                                                                                                  iv_primary_qmgrp   = 'ELE'
                                                                                                                  iv_secon_qmcod     = '0015'
                                                                                                                  iv_secon_qmgrp     = 'ELE'
                                                                                                                  iv_serv_prov_prim  = 'ACTIVEMP'
                                                                                                                  iv_serv_prov_secon = 'LNSP'
                                                                                                                  iv_scenario        = 'TRANSFER' ).
    IF lv_mpb_active IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_mpb_for_reen_deen_5.

    DATA: lt_mpb_brf TYPE t_serviceid,
          lv_mpb_brf TYPE service_prov.

    lv_mpb_brf = 'ACTIVEMP'.
    APPEND lv_mpb_brf TO lt_mpb_brf.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_crr_dao )->ignore_all_parameters( )->returning( 'VIC' ) .
    go_crr_dao->get_jurisdiction( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    DATA(lv_mpb_active) = go_reen_deen_util->zif_idm_remote_reen_deen_util~set_mpb_for_reen_move_in( EXPORTING io_crr_dao         = go_crr_dao
                                                                                                                  io_reen_deen_dao   = go_reen_deen_dao
                                                                                                                  iv_int_ui          = 'shjhsh'
                                                                                                                  iv_mpb_active      = abap_true
                                                                                                                  iv_primary_qmcod   = '0019'
                                                                                                                  iv_primary_qmgrp   = 'ELE'
                                                                                                                  iv_secon_qmcod     = '0020'
                                                                                                                  iv_secon_qmgrp     = 'ELE'
                                                                                                                  iv_serv_prov_prim  = 'ACTIVEMP'
                                                                                                                  iv_serv_prov_secon = 'LNSP'
                                                                                                                  iv_scenario        = 'TRANSFER' ).
    IF lv_mpb_active IS INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.


  ENDMETHOD.

  METHOD create_contact_note_test.

    DATA: lt_lines TYPE comt_text_lines_t.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( '1233456' ) .
    go_reen_deen_dao->fetch_contract_account( '92020802' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->set_parameter( name = 'EV_DATE' value = '20200929' )->set_parameter(
                                                                                                     name = 'EV_RECEIVER' value = 'LNSP' ).
    go_reen_deen_dao->fetch_reqd_start_date( '92020802' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'LNSP' ) .
    go_reen_deen_dao->fetch_receiver( '92020802' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( '123456' ) .
    go_reen_deen_dao->create_contact_note( EXPORTING it_lines = lt_lines
                                                     iv_cont_acc = '162772'
                                                     iv_description = 'ABCD'
                                                     iv_partner = '98765445' ).



    DATA(lv_contact_id) = go_reen_deen_util->zif_idm_remote_reen_deen_util~create_contact_note( EXPORTING iv_partner   = '25255226'
                                                                                                          iv_prim_sn   = '121812'
                                                                                                          iv_linked_sn = '289792'
                                                                                                          iv_qmcod     = '0015'
                                                                                                          iv_qmgrp     = 'ELE-'
                                                                                                          io_reen_deen_dao = go_reen_deen_dao ).

    IF lv_contact_id IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.


  ENDMETHOD.

  METHOD determine_sn_det .

    DATA: lt_deen_det TYPE zidmdt_deen_meter,
          lt_mpb_brf  TYPE t_serviceid,
          lv_mpb_brf  TYPE service_prov.

    lv_mpb_brf = 'ACTIVEMP'.
    APPEND lv_mpb_brf TO lt_mpb_brf.

    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = abap_true ) TO lt_deen_det.
    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'ACTIVEMP'  ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui       = 'SHJHSK'
                                                        iv_service_type = 'LNSP' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_code = DATA(lv_code) ).

    IF lv_code IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_1.

    DATA: lt_deen_det TYPE zidmdt_deen_meter,
          lt_mpb_brf  TYPE t_serviceid.

    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = abap_true ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'ACTIVEMP'  ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui       = 'SHJHSK'
                                                        iv_service_type = 'LNSP' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_exception = DATA(lv_exception) ).

    IF lv_exception IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_2.

    DATA: lt_deen_det TYPE zidmdt_deen_meter,
          lt_mpb_brf  TYPE t_serviceid.


    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = abap_true ) TO lt_deen_det.
    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'ACTIVEMP'  ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui       = 'SHJHSK'
                                                        iv_service_type = 'LNSP' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_code = DATA(lv_code) ).

    IF lv_code IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_3.

    DATA: lt_deen_det TYPE zidmdt_deen_meter,
          lt_mpb_brf  TYPE t_serviceid.

    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'ACTIVEMP'  ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui       = 'SHJHSK'
                                                        iv_service_type = 'LNSP' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_code = DATA(lv_code) ).

    IF lv_code IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_4.

    DATA: lt_deen_det TYPE zidmdt_deen_meter,
          lt_mpb_brf  TYPE t_serviceid.

    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.
    APPEND VALUE #( metertype = 'M1' type_code = '00157' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'ACTIVEMP'  ) .
    go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui       = 'SHJHSK'
                                                        iv_service_type = 'LNSP' ).


    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning(  lt_mpb_brf  ) .
    go_reen_deen_dao->check_state_mpb_brf( 'VIC' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_code = DATA(lv_code) ).

    IF lv_code IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_5.

    DATA: lt_deen_det TYPE zidmdt_deen_meter.

    APPEND VALUE #( metertype = 'M1' type_code = '0015' state = 'VIC' mpb_active = ' ' ) TO lt_deen_det.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'SMALL' ) .
    go_reen_deen_dao->fetch_nmi_class_brf( ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'LARGE' ) .
    go_reen_deen_dao->fetch_nmi_class_db( 'hshhjjksiwiwi' ).


    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_code = DATA(lv_code) ).

    IF lv_code IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_sn_det_6.

    DATA: lt_deen_det TYPE zidmdt_deen_meter.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( abap_false ) .
    go_sntl_dao->get_internal_ui( '2272' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_sn_details( EXPORTING it_deen_det = lt_deen_det
                                                                                     iv_anlage = '17719'
                                                                                     iv_state = 'VIC'
                                                                                     io_reen_deen_dao = go_reen_deen_dao
                                                                                     io_sntl_dao     = go_sntl_dao
                                                                           IMPORTING ev_exception = DATA(lv_exception) ).

    IF lv_exception IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD check_2nd_sn_move_in.

    cl_abap_testdouble=>configure_call( go_sntl_dao )->ignore_all_parameters( )->returning( 'abacfsgjk7' ) .
    go_sntl_dao->get_internal_ui( '266287' ).

    go_reen_deen_util->zif_idm_remote_reen_deen_util~check_2nd_sn_elligible_move_in( EXPORTING iv_anlage = '62626'
                                                                                               iv_ext_ui = 'sjhshks'
                                                                                               io_sntl_dao = go_sntl_dao
                                                                                     IMPORTING ev_mpb_active = DATA(lv_mpb_active) ).
    IF lv_mpb_active IS INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD check_2nd_sn_transfer.

    go_reen_deen_util->zif_idm_remote_reen_deen_util~check_2nd_sn_elligbl_transf_in( EXPORTING iv_anlage = '82689122'
                                                                                               iv_ext_ui = 'ahjhjah'
                                                                                               iv_prim_notif = '267279'
                                                                                      IMPORTING ev_mpb_active = DATA(lv_mpb_active) ).
    IF lv_mpb_active IS INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.
  METHOD check_dereg_satatus.

    DATA: lt_eastl_logiknr TYPE zidmtt_eastl_logiknr,
          ls_egerr         TYPE zidmst_egerr_io_grp,
          lt_egerr         TYPE zidmtt_egerr_io_grp.

    ls_egerr-equnr = '12345'.
    ls_egerr-bis = '99991231'.
    ls_egerr-logiknr = '000000000000000016'.
    ls_egerr-eagruppe = '12345678'.

    APPEND VALUE #( anlage = '12345' logiknr = '000000000000000016' bis = '99991231' ) TO lt_eastl_logiknr[].
    APPEND ls_egerr TO lt_egerr.

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_eastl_logiknr ).
    go_reen_deen_dao->fetch_active_eastl_entries('12345').

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_egerr ) .
    go_reen_deen_dao->fetch_io_group( lt_eastl_logiknr ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( 'M21' ).
    go_reen_deen_dao->fetch_dereg_stat_from_zcrt( ls_egerr-eagruppe ).

    DATA(lv_dereg_status) = go_reen_deen_util->zif_idm_remote_reen_deen_util~check_dereg_satatus_4_io_grp( EXPORTING iv_anlage = '12345'
                                                                                            io_reen_deen_dao = go_reen_deen_dao ).

    IF lv_dereg_status IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.
  ENDMETHOD.
  METHOD append_text_in_spcl_instr.

    DATA: lt_lines TYPE comt_text_lines_t.

    lt_lines = VALUE #( ( tdformat = '>X' tdline = '* Hi This is For Testing' )
                        ( tdformat = '>X' tdline = '* Hi This is For Testing' )
                        ( tdformat = '>X' tdline = '* Hi This is For Testing' ) ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( abap_true ).
    go_reen_deen_dao->append_text_in_spcl_instr( iv_id = 'LTXT'
                                                 iv_lang = 'E'
                                                 iv_name = '123456789'
                                                 iv_object = 'QMEL'
                                                 iv_update_type = 'M'
                                                 it_line = lt_lines
                                                 is_line_bapi = VALUE #( objtype = 'QMEL'
                                                                         format_col = '*'
                                                                         text_line = 'MONCONON' ) ).

    DATA(lv_return_code) = go_reen_deen_util->zif_idm_remote_reen_deen_util~append_text_in_spcl_instr(
                           iv_id            = 'LTXT'
                           iv_lang          = 'E'
                           iv_name          = '123456789'
                           iv_object        = 'QMEL'
                           iv_index         = 2
                           iv_update_type   = 'M'
                           iv_text          = 'MONCONON'
                           it_line          = lt_lines
                           io_reen_deen_dao = go_reen_deen_dao ).


    IF lv_return_code = abap_true.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.

  ENDMETHOD.

  METHOD determ_nmi_mtr_stat.

    DATA: lt_dereg_stat TYPE zidmdt_meter_nmi_stat.

    lt_dereg_stat = VALUE #( ( state = 'QLD' deregstat = 'M1' ami_type = '*'  meter_status = ' ' nmi_status = 'D' ) ).

    cl_abap_testdouble=>configure_call( go_crr_dao )->ignore_all_parameters( )->returning( 'QLD' ) .
    go_crr_dao->get_jurisdiction( 'hshhjjksiwiwi' ).

    cl_abap_testdouble=>configure_call( go_reen_deen_dao )->ignore_all_parameters( )->returning( lt_dereg_stat ) .
    go_reen_deen_dao->check_reen_elig_4_trans_mov_in(  ).

    DATA(ls_dereg_stat) = go_reen_deen_util->zif_idm_remote_reen_deen_util~determine_elig_nmi_meter_stat( EXPORTING io_crr_dao = go_crr_dao
                                                                                                                    io_reen_deen_dao = go_reen_deen_dao
                                                                                                                    iv_int_ui = 'Gjhkhkjjl'
                                                                                                                    iv_deregstat = 'M1'
                                                                                                                    iv_zami_type = 'BC' ).
    IF ls_dereg_stat IS NOT INITIAL.
      assert_condition_true( ).
    ELSE.
      assert_condition_false( ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
