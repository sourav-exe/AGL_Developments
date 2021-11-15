class YCL_IDM_REMOTE_REEN_DEEN_UTIL definition
  public
  final
  create public .

public section.

  interfaces ZIF_IDM_REMOTE_REEN_DEEN_UTIL .

  constants GC_SCEN_TRANSFER type CHAR10 value 'TRANSFER' ##NO_TEXT.

  methods CONSTRUCTOR .
protected section.
private section.

  data GO_REEN_DEEN_DAO type ref to ZIF_IDM_REMOTE_REEN_DEEN_DAO .
  data GO_SNTL_DAO type ref to ZIF_IID_SNTL_DAO .
  data GO_CRR_DB_PROCESS type ref to ZIF_IID_CRR_DB_PROCESS .
  constants GC_ELE type CHAR4 value 'ELE-' ##NO_TEXT.
  constants GC_ELE_ONLY type CHAR3 value 'ELE' ##NO_TEXT.
  constants GC_0233 type CHAR4 value '0233' ##NO_TEXT.
  constants GC_DEEN_SN type Z_TRANS_ID value 'DEEN_SN' ##NO_TEXT.
  constants GC_STAR type CHAR1 value '*' ##NO_TEXT.
  constants GC_0027 type CHAR4 value '0027' ##NO_TEXT.
  constants GC_0015 type CHAR4 value '0015' ##NO_TEXT.
  constants GC_LAST_DATE type BISZEITSCH value '99991231' ##NO_TEXT.
  constants GC_MNGRP_ELE type MFGRP value 'ELE-' ##NO_TEXT.
  constants GC_MNCOD_0234 type MFCOD value '0234' ##NO_TEXT.
  constants GC_MOVE_IN type CHAR2 value 'MI' ##NO_TEXT.
  constants GC_TRANSFER_IN type CHAR2 value 'TI' ##NO_TEXT.
ENDCLASS.



CLASS YCL_IDM_REMOTE_REEN_DEEN_UTIL IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    go_reen_deen_dao = NEW zcl_idm_remote_reen_deen_dao( ).
    go_sntl_dao = NEW zcl_iid_sntl_dao( ).
    go_crr_db_process = NEW zcl_iid_crr_db_process( ).

  ENDMETHOD.


METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~ADD_ACTIVITY_IN_SERV_NOTIF.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 21/08/2020  Shalmali Choudhury  CMI-8752    Remote Re-en/De-en:Re-energisation Service Notifications
*
*======================================================================================================*

  DATA: lt_notifactv TYPE alm_me_bapi2080_notactvi_t,
        ls_notiftask TYPE bapi2080_notactvi.

  " If reference supplied then use that
  IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
    go_reen_deen_dao ?= io_reen_deen_dao.
  ENDIF.

  IF iv_parent_sn IS NOT INITIAL AND iv_child_sn IS NOT INITIAL.
    "Fetch SN details
    DATA(ls_sn_det) = go_reen_deen_dao->fetch_sn_details( EXPORTING iv_sn = iv_parent_sn ).

    IF iv_copy_activity IS NOT INITIAL.
      DATA(ls_child_sn_det) = go_reen_deen_dao->fetch_sn_details( EXPORTING iv_sn = iv_child_sn ).
      DATA(lv_dele_flag) = go_reen_deen_dao->is_sn_marked_for_deletion( ls_child_sn_det-objnr ).
    ENDIF.

    IF ls_sn_det IS NOT INITIAL.
      IF lv_dele_flag EQ abap_false.    " restricting addition of deleted SN to attributes
        "Fetch Activity details
        DATA(lt_sn_activity) = go_reen_deen_dao->fetch_activity_no( EXPORTING iv_sn = iv_parent_sn ).

        IF lt_sn_activity[] IS NOT INITIAL.
          SORT lt_sn_activity BY manum DESCENDING.
          READ TABLE lt_sn_activity INTO DATA(ls_activity) INDEX 1.
          IF sy-subrc = 0.
            ls_notiftask-act_sort_no  = ls_activity-manum + 1.
          ENDIF.
        ELSE.
          ls_notiftask-act_sort_no  = '0001'.
        ENDIF.

        ls_notiftask-refobjectkey = ls_sn_det-objnr.
        ls_notiftask-item_sort_no = ls_notiftask-act_sort_no .
        ls_notiftask-act_code     = iv_qmcode.
        ls_notiftask-act_codegrp  = iv_qmgrp.
        ls_notiftask-acttext      = iv_child_sn .

        APPEND ls_notiftask TO lt_notifactv .
      ENDIF.

      IF iv_copy_activity IS NOT INITIAL.
        "Copy child activities to parent ( Copy activities present in 0017 to 0015 )
        "Fetch the child activity details ( which is to be linked with parent )
        DATA(lt_sn_child_activity) = go_reen_deen_dao->fetch_activity_no( EXPORTING iv_sn = iv_child_sn ).
        DELETE lt_sn_child_activity WHERE  mngrp = gc_mngrp_ele  AND mncod = gc_mncod_0234  AND matxt = iv_parent_sn .
        DATA(lv_sort_no) = COND #( WHEN ls_notiftask-act_sort_no IS NOT INITIAL THEN ls_notiftask-act_sort_no
                                   ELSE '0001' ).
        LOOP AT lt_sn_child_activity INTO DATA(ls_child_act).
          lv_sort_no = lv_sort_no + 1.
          ls_notiftask-refobjectkey = ls_sn_det-objnr.
          ls_notiftask-item_sort_no = lv_sort_no .
          ls_notiftask-act_code     = ls_child_act-mncod.
          ls_notiftask-act_codegrp  = ls_child_act-mngrp.
          ls_notiftask-acttext      = ls_child_act-matxt .
          APPEND ls_notiftask TO lt_notifactv .
        ENDLOOP.
      ENDIF.

      IF lt_notifactv[] IS NOT INITIAL.
        et_return = go_reen_deen_dao->add_activity( EXPORTING iv_serv_notif = iv_parent_sn
                                                    CHANGING  ct_notif_activity = lt_notifactv ).

        READ TABLE et_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          rv_success = go_reen_deen_dao->save_service_notification( EXPORTING iv_sn = iv_parent_sn ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~APPEND_TEXT_IN_SPCL_INSTR.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 06/11/2020  Sourav Roy          CMI-9936    Phase 2 Remote Re-en/De-en: Auto Special Instructions on SN
*
*======================================================================================================*

    DATA: lt_line TYPE comt_text_lines_t.
    CLEAR: rv_return_code.

    " If reference supplied then use that
    IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
      go_reen_deen_dao ?= io_reen_deen_dao.
    ENDIF.

    lt_line = it_line.

    CASE iv_update_type .
      WHEN 'I'. "Inserts new line with change time history line
        DATA(ls_line_bapi) =  VALUE bapi2080_notfulltxti(  objtype = iv_object
                                                           format_col = '*'
                                                           text_line = CONV #(  iv_text  ) ).

      WHEN 'M'.   "Modifies existing line
        DATA(ls_line) =  VALUE tline(  tdformat = '>X'
                                       tdline = CONV #( |* { iv_text } { VALUE #( it_line[ iv_index ]-tdline+2 DEFAULT '' ) } | ) ).

        IF line_exists( it_line[ iv_index ] ).
          MODIFY lt_line FROM ls_line INDEX iv_index.
        ENDIF.

      WHEN 'A'.     "Appends new line at the end of existing lines
        ls_line =  VALUE tline(  tdformat = '>X'
                                 tdline = CONV #(  |* { iv_text }| ) ).
        INSERT ls_line INTO TABLE lt_line.
      WHEN OTHERS.
    ENDCASE.





    rv_return_code = go_reen_deen_dao->append_text_in_spcl_instr( iv_id = iv_id
                                                                  iv_lang = iv_lang
                                                                  iv_name = iv_name
                                                                  iv_object = iv_object
                                                                  iv_update_type = iv_update_type
                                                                  it_line = lt_line
                                                                  is_line_bapi = ls_line_bapi ).


  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~CHECK_2ND_SN_ELLIGBL_TRANSF_IN.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 21/08/2020  Shalmali Choudhury  CMI-9089    Remote Re-en/De-en: Manual re-en SN enhancements
*
*======================================================================================================*
    CLEAR: ev_qmcod_dual , ev_recipient_dual , ev_mpb_active.

    CALL FUNCTION 'Z_ISU_CHECK_CORRECT_SN'
      EXPORTING
        iv_scenario      = gc_transfer_in
        iv_ext_ui        = iv_ext_ui
        iv_anlage        = iv_anlage
        iv_qmnum         = iv_prim_notif
        iv_skip_deletion = abap_true
      IMPORTING
        ev_reen_qmcod    = ev_qmcod_dual
        ev_reen_mpb      = ev_recipient_dual.
    IF ev_recipient_dual IS NOT INITIAL AND ev_qmcod_dual IS NOT INITIAL.
      ev_mpb_active = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~CHECK_2ND_SN_ELLIGIBLE_MOVE_IN.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 21/08/2020  Shalmali Choudhury  CMI-9089    Remote Re-en/De-en: Manual re-en SN enhancements
*
*======================================================================================================*
    DATA: lv_mpb_active     TYPE flag,
          lv_qmgrp_dual     TYPE qmgrp,
          lv_qmcod_dual     TYPE qmcod,
          lv_recipient_dual TYPE service_prov,
          lv_recipient_prim TYPE service_prov,
          lv_qmcod_prim     TYPE qmcod,
          lv_qmgrp_prim     TYPE qmgrp.


    " If reference supplied then use that
    IF io_sntl_dao IS SUPPLIED AND io_sntl_dao IS BOUND.
      go_sntl_dao ?= io_sntl_dao.
    ENDIF.

    CLEAR: ev_qmcod_dual,
    ev_qmgrp_dual,
    ev_recipient_dual,
    ev_mpb_active.

    IF iv_anlage IS NOT INITIAL.
      DATA(lv_intui) = go_sntl_dao->get_internal_ui( iv_anlage ).
      IF lv_intui IS NOT INITIAL.
        CALL FUNCTION 'ZICA346_DATA_FOR_SOTYPE'
          EXPORTING
            iv_scen           = gc_move_in
            iv_intui          = lv_intui
            iv_anlage         = iv_anlage
          IMPORTING
            rv_qmcod          = lv_qmcod_prim
            rv_qmgrp          = lv_qmgrp_prim
            rv_recipient      = lv_recipient_prim
            rv_qmcod_dual     = lv_qmcod_dual
            rv_qmgrp_dual     = lv_qmgrp_dual
            rv_recipient_dual = lv_recipient_dual
            ev_mpb_active     = lv_mpb_active.

        IF lv_mpb_active IS NOT INITIAL .
          IF lv_recipient_dual IS NOT INITIAL AND lv_qmgrp_dual IS NOT INITIAL AND lv_qmcod_dual IS NOT INITIAL.
            ev_qmcod_dual = lv_qmcod_dual.
            ev_qmgrp_dual = lv_qmgrp_dual.
            ev_recipient_dual = lv_recipient_dual.
            ev_mpb_active = abap_true.
          ELSEIF lv_recipient_prim IS NOT INITIAL AND lv_qmgrp_prim IS NOT INITIAL AND lv_qmcod_prim IS NOT INITIAL.
            ev_qmcod_dual = lv_qmcod_prim.
            ev_qmgrp_dual = lv_qmgrp_prim.
            ev_recipient_dual = lv_recipient_prim.
            ev_mpb_active = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~CHECK_DEREG_SATATUS_4_IO_GRP.
*==============================================================================================*
* CHANGE HISTORY
* Date        Name                Reference   Description
* 24/08/2020  Swagata Mukherjee   CMI-8845    Remote Re-en/De-en: Dereg status update when meter is installed
*
*==============================================================================================*
    CHECK iv_anlage IS NOT INITIAL.

    CLEAR: rv_dereg_status.

    " If reference supplied then use that
    IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
      go_reen_deen_dao ?= io_reen_deen_dao.
    ENDIF.

    DATA(lt_eastl) = go_reen_deen_dao->fetch_active_eastl_entries( iv_anlage = iv_anlage ).
    DATA(lt_egerr) = go_reen_deen_dao->fetch_io_group( it_eastl_logiknr = lt_eastl ).

    LOOP AT lt_eastl INTO DATA(ls_eastl).
      READ TABLE lt_egerr INTO DATA(ls_egerr) WITH KEY logiknr = ls_eastl-logiknr
                                                       bis = gc_last_date.

      IF sy-subrc = 0.
        DATA(lv_iogroup) = ls_egerr-eagruppe.
        EXIT.
      ENDIF.
      CLEAR: ls_egerr,ls_eastl.
    ENDLOOP.

    rv_dereg_status = go_reen_deen_dao->fetch_dereg_stat_from_zcrt( iv_io_grp = lv_iogroup ).
    CLEAR: lv_iogroup.

  ENDMETHOD.


METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~CHECK_ELLIGIBILE_4_REMOTE_DEEN.
*==============================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 24/08/2020  Shalmali Choudhury  CMI-8846    Remote Re-en/De-en: Manual de-en SN enhancements
*
*==============================================================================================*

  CLEAR : ev_error,
          ev_sn_type,
          ev_recipient,
          ev_recipient_name.

  " If reference supplied then use that
  IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
    go_reen_deen_dao ?= io_reen_deen_dao.
  ENDIF.

  IF io_sntl_dao IS SUPPLIED AND io_sntl_dao IS BOUND.
    go_sntl_dao ?= io_sntl_dao.
  ENDIF.

  IF is_viqmel-zzanlage IS NOT INITIAL AND is_viqmel-qmgrp IS NOT INITIAL AND is_viqmel-qmcod IS NOT INITIAL.
*Check if SN type is DE-EN
    DATA(lt_deen_data) =  go_reen_deen_dao->fetch_deen_type_brf( gc_deen_sn ).
    READ TABLE lt_deen_data WITH KEY fieldvalue1 = is_viqmel-qmgrp
                                     fieldvalue2 = is_viqmel-qmcod TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
*If additional activity
      READ TABLE it_activity_tab WITH KEY mngrp =  gc_ele
                                          mncod =  gc_0233 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
*Get DE-EN type code
*---- Determine internal ui
        DATA(lv_int_ui) = go_sntl_dao->get_internal_ui( EXPORTING iv_installation = is_viqmel-zzanlage ).
        go_reen_deen_dao->get_deen_type_code( EXPORTING iv_anlage = is_viqmel-zzanlage
                                                        iv_int_ui = lv_int_ui
                                                        iv_jurs = iv_jurisdiction
                                              IMPORTING ev_code_grp = DATA(lv_code_grp)
                                                        ev_type_code = DATA(lv_type_code)
                                                        ev_reci_code = DATA(lv_recipient)
                                                        ev_exception = DATA(lv_exception) ).

        IF lv_exception IS INITIAL AND lv_code_grp = gc_ele AND lv_type_code = gc_0027.
*      Get the recipient details from supply scenario
          DATA(lv_serv_prov) = go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui = lv_int_ui
                                                                                   iv_service_type = lv_recipient ).

          IF is_viqmel-qmgrp <> lv_code_grp OR is_viqmel-qmcod <> lv_type_code OR is_viqmel-zzreceiver <> lv_serv_prov.

            ev_error = abap_true.
            CONCATENATE lv_code_grp lv_type_code INTO ev_sn_type.
            ev_recipient = lv_recipient.
            ev_recipient_name = lv_serv_prov.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~CREATE_CONTACT_NOTE.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 21/08/2020  Shalmali Choudhury  CMI-8752    Remote Re-en/De-en:Re-energisation Service Notifications
*
*======================================================================================================*
    DATA:lt_lines       TYPE comt_text_lines_t,
         ls_lines       TYPE tline,
         lv_description TYPE char40,
         lv_text(132)   TYPE c,
         lv_partner     TYPE bu_partner.

    CONSTANTS: lc_dot TYPE char1 VALUE '.'.

    " If reference supplied then use that
    IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
      go_reen_deen_dao ?= io_reen_deen_dao.
    ENDIF.

    CLEAR:  rv_contact_id.

    IF iv_partner IS NOT INITIAL AND iv_prim_sn IS NOT INITIAL AND iv_linked_sn IS NOT INITIAL.
      lv_partner = iv_partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_partner
        IMPORTING
          output = lv_partner.
      "Fetch Contract account
      DATA(lv_cont_acc) =  go_reen_deen_dao->fetch_contract_account( lv_partner ).
      IF lv_cont_acc IS NOT INITIAL.
        "Fetch Required start date of SN
        go_reen_deen_dao->fetch_reqd_start_date( EXPORTING iv_serv_notif = iv_prim_sn
                                                 IMPORTING ev_date = DATA(lv_reqd_date)
                                                           ev_receiver = DATA(lv_mp) ).
        IF lv_reqd_date IS NOT INITIAL.
          DATA(lv_year) = lv_reqd_date(4).
          DATA(lv_month) = lv_reqd_date+4(2).
          DATA(lv_day)  = lv_reqd_date+6(2).
          CONCATENATE lv_day lv_month lv_year INTO DATA(lv_reqd_st_date) SEPARATED BY lc_dot.
        ENDIF.
        "Add contact note text
        CONCATENATE iv_qmgrp iv_qmcod INTO DATA(lv_sn_type).
        CONCATENATE 'Auto Service Order'(001) lv_sn_type 'Raised'(002) INTO lv_description SEPARATED BY space.

        CONCATENATE 'Auto Remote Service Notification'(003)
                   lv_sn_type
                   iv_prim_sn
                   INTO lv_text SEPARATED BY space .
        ls_lines-tdline = lv_text.
        APPEND ls_lines TO lt_lines.
        CLEAR: lv_text,ls_lines.

        CONCATENATE 'Linked Service Notification'(004)
                     iv_linked_sn
                      INTO lv_text SEPARATED BY space .
        ls_lines-tdline = lv_text.
        APPEND ls_lines TO lt_lines.
        CLEAR: lv_text,ls_lines.

        CONCATENATE 'Required Start Date'(005)
                     lv_reqd_st_date
                      INTO lv_text SEPARATED BY space.
        ls_lines-tdline = lv_text.
        APPEND ls_lines TO lt_lines.
        CLEAR: lv_text,ls_lines.

        CONCATENATE 'MP:'
                    lv_mp
                  INTO lv_text SEPARATED BY space.
        ls_lines-tdline = lv_text.
        APPEND ls_lines TO lt_lines.
        CLEAR: lv_text,ls_lines.

        "Create Contact Note in CRM
        IF lt_lines[] IS NOT INITIAL.
          rv_contact_id =   go_reen_deen_dao->create_contact_note( EXPORTING it_lines      = lt_lines
                                                                            iv_partner     = lv_partner
                                                                            iv_cont_acc    = lv_cont_acc
                                                                            iv_description = lv_description ).

        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~DETERMINE_ELIG_NMI_METER_STAT.

    CONSTANTS:  lc_all       TYPE zmtr_status VALUE '*'.

    CLEAR:  rs_mtr_nmi_details.

    IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
      go_reen_deen_dao ?= io_reen_deen_dao.
    ENDIF.

    IF io_crr_dao IS SUPPLIED AND io_crr_dao IS BOUND.
      go_crr_db_process ?= io_crr_dao.
    ENDIF.

    "Fetch state
    DATA(lv_state) =   go_crr_db_process->get_jurisdiction( EXPORTING iv_int_ui =  iv_int_ui ).
    "Fetch the state, derestat ami type combination from brf+
    DATA(lt_state_dereg_comb) = go_reen_deen_dao->check_reen_elig_4_trans_mov_in( ).
    "First search with exact combination
    READ TABLE lt_state_dereg_comb INTO DATA(ls_state_dereg) WITH KEY state     = lv_state
                                                                      deregstat = iv_deregstat
                                                                      ami_type  = iv_zami_type.
    IF ls_state_dereg IS INITIAL.
      "Not found in previous search- then search with *( ALL ) combination for ami type
      READ TABLE lt_state_dereg_comb INTO ls_state_dereg WITH KEY state     = lv_state
                                                                  deregstat = iv_deregstat
                                                                  ami_type  = lc_all.
    ENDIF.

    rs_mtr_nmi_details = ls_state_dereg.

  ENDMETHOD.


 METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~DETERMINE_SN_DETAILS.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 21/08/2020  Shalmali Choudhury  CMI-8753    Remote Re-en/De-en:De-energisation Service Notifications
*
*======================================================================================================*
   CLEAR: ev_code_grp , ev_code , ev_recipient , ev_exception.

   " If reference supplied then use that
   IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
     go_reen_deen_dao ?= io_reen_deen_dao.
   ENDIF.

   IF io_sntl_dao IS SUPPLIED AND io_sntl_dao IS BOUND.
     go_sntl_dao ?= io_sntl_dao.
   ENDIF.
   "    Determine internal ui
   DATA(lv_int_ui) = go_sntl_dao->get_internal_ui( EXPORTING iv_installation = iv_anlage ).
   IF lv_int_ui IS INITIAL.
     ev_exception = abap_true.
     RETURN.
   ENDIF.
   "  Fetch NMI Class--
   DATA(lv_nmi_class_brf) = go_reen_deen_dao->fetch_nmi_class_brf( ).
   DATA(lv_nmi_class_db) = go_reen_deen_dao->fetch_nmi_class_db( lv_int_ui ).

   IF lv_nmi_class_db <> lv_nmi_class_brf AND lv_nmi_class_db IS NOT INITIAL.
     me->zif_idm_remote_reen_deen_util~get_data_with_mpb_false( EXPORTING  it_deen_det  = it_deen_det
                                                                           iv_state     = iv_state
                                                                IMPORTING  ev_code_grp  = ev_code_grp
                                                                           ev_recipient = ev_recipient
                                                                           ev_code      =  ev_code ).
   ELSE.
     IF lines( it_deen_det ) = 1. "Single entry found
       READ TABLE it_deen_det INTO DATA(ls_deen) INDEX 1.
       IF sy-subrc = 0.
         IF ls_deen-mpb_active IS INITIAL. "MPB not active
           ev_code_grp    = ls_deen-ord_type .
           ev_code        = ls_deen-type_code .
           ev_recipient   = ls_deen-recipient.
         ELSE.
           DATA(lv_sing_active) = abap_true.
         ENDIF.
       ENDIF.
     ELSE.
       DATA(lv_multiple) = abap_true.
     ENDIF.
     IF lv_multiple IS NOT INITIAL OR lv_sing_active IS NOT INITIAL.
       "Find the entry for which MPB is active
       CLEAR ls_deen.
       READ TABLE it_deen_det INTO ls_deen WITH KEY mpb_active = abap_true.
       IF sy-subrc = 0.
         "Get the recipient details from Supply scenario
         DATA(lv_serv_prov) = go_reen_deen_dao->fetch_service_provider( EXPORTING iv_int_ui = lv_int_ui
                                                                                  iv_service_type = ls_deen-recipient ).
         "Check the prsence in BRF+
         DATA(lt_mpb_brf) =  go_reen_deen_dao->check_state_mpb_brf( EXPORTING iv_state = iv_state ).
         READ TABLE lt_mpb_brf WITH KEY table_line = lv_serv_prov TRANSPORTING NO FIELDS.
         IF sy-subrc = 0.
           " Found-
           "Get the details for which MPB is active.
           ev_code_grp  = ls_deen-ord_type.
           ev_recipient = ls_deen-recipient.
           ev_code      = ls_deen-type_code.

         ELSE.
           "Not found in brf
           "Check for multiple entries
           IF lv_multiple IS NOT INITIAL.
             "  Multiple - Get the details for which MPB is inactive.
             me->zif_idm_remote_reen_deen_util~get_data_with_mpb_false( EXPORTING  it_deen_det  = it_deen_det
                                                                                   iv_state     = iv_state
                                                                        IMPORTING  ev_code_grp  = ev_code_grp
                                                                                   ev_recipient = ev_recipient
                                                                                   ev_code      =  ev_code ).
           ELSE.
             "End with exception
             ev_exception = abap_true.
           ENDIF.
         ENDIF.
       ELSE.
         "  Get the details for which MPB is inactive.
         me->zif_idm_remote_reen_deen_util~get_data_with_mpb_false( EXPORTING  it_deen_det  = it_deen_det
                                                                               iv_state     = iv_state
                                                                    IMPORTING  ev_code_grp  = ev_code_grp
                                                                               ev_recipient = ev_recipient
                                                                               ev_code      =  ev_code ).
       ENDIF.
     ENDIF.
   ENDIF.
 ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~GET_DATA_WITH_MPB_FALSE.

    CLEAR: ev_code_grp,ev_recipient,ev_code.
    READ TABLE it_deen_det INTO DATA(ls_deen) WITH KEY state      = iv_state
                                                       mpb_active = abap_false.
    IF sy-subrc = 0.
      ev_code_grp  = ls_deen-ord_type.
      ev_recipient = ls_deen-recipient.
      ev_code      = ls_deen-type_code.
    ELSE.
      READ TABLE it_deen_det INTO ls_deen WITH KEY state     = gc_star
                                                   mpb_active = abap_false.
      IF sy-subrc = 0.
        ev_code_grp  = ls_deen-ord_type.
        ev_recipient = ls_deen-recipient.
        ev_code      = ls_deen-type_code.
      ENDIF.
    ENDIF.

  ENDMETHOD.


 METHOD ZIF_IDM_REMOTE_REEN_DEEN_UTIL~SET_MPB_FOR_REEN_MOVE_IN.
*=======================================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 26/08/2020  Shalmali Choudhury  CMI-8752    Remote Re-en/De-en : Re-energisation Service Notifications - Non Transfer
*
*=======================================================================================================================*
   " If reference supplied then use that
   IF io_reen_deen_dao IS SUPPLIED AND io_reen_deen_dao IS BOUND.
     go_reen_deen_dao ?= io_reen_deen_dao.
   ENDIF.

   IF io_crr_dao IS SUPPLIED AND io_crr_dao IS BOUND.
     go_crr_db_process ?= io_crr_dao.
   ENDIF.

   rv_mpb_active = iv_mpb_active.

   IF iv_mpb_active IS NOT INITIAL.
     "  Fetch NMI Class--
     DATA(lv_nmi_class_brf) = go_reen_deen_dao->fetch_nmi_class_brf( ).
     DATA(lv_nmi_class_db) = go_reen_deen_dao->fetch_nmi_class_db( iv_int_ui ).
     IF lv_nmi_class_db <> lv_nmi_class_brf AND lv_nmi_class_db IS NOT INITIAL.
       "If NMI Class not small, then existing process, clear mpb active flag
       CLEAR rv_mpb_active.
       RETURN.
     ENDIF.
     "Primary SN type is 'ELE-0015
     IF iv_primary_qmgrp = gc_ele_only AND iv_primary_qmcod = gc_0015.
       DATA(lv_found) = abap_true.
       DATA(lv_serv_prov) = iv_serv_prov_prim.
     ELSE.
       "No- Check if secondary SN type is ELE-0015
       IF iv_secon_qmgrp = gc_ele_only AND iv_secon_qmcod = gc_0015.
         lv_found = abap_true.
         lv_serv_prov = iv_serv_prov_secon.
       ELSE.
         "No - clear MPB
         CLEAR rv_mpb_active.
       ENDIF.
     ENDIF.
   ENDIF.
   IF lv_found IS NOT INITIAL.
     "Check if MPB is active as per BRF+
     DATA(lv_state) = go_crr_db_process->get_jurisdiction( EXPORTING iv_int_ui = iv_int_ui ).
     DATA(lt_mpb_brf) =  go_reen_deen_dao->check_state_mpb_brf( EXPORTING iv_state = lv_state ).
     IF iv_scenario EQ gc_scen_transfer AND lt_mpb_brf[] IS NOT INITIAL.
       rv_mpb_active = abap_true.
     ELSE.
       READ TABLE lt_mpb_brf WITH KEY table_line = lv_serv_prov TRANSPORTING NO FIELDS .
       IF sy-subrc = 0.
         "Set MPB
         rv_mpb_active = abap_true.
       ELSE.
         CLEAR rv_mpb_active.
       ENDIF.
     ENDIF.
   ENDIF.
 ENDMETHOD.
ENDCLASS.
