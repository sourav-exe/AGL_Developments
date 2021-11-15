class YCL_IDM_REMOTE_REEN_DEEN_DAO definition
  public
  final
  create public .

public section.

  interfaces ZIF_IDM_REMOTE_REEN_DEEN_DAO .

  constants GC_DEL_STAT type J_STATUS value 'I0076' ##NO_TEXT.
  constants GC_CRM_RFC type CHAR7 value 'CRM_RFC' ##NO_TEXT.
  constants GC_SWTTYPE_MI type EIDESWTTYPE value '70' ##NO_TEXT.
  constants GC_SWTVW_TRNSF_IN type EIDESWTVIEW value '70' ##NO_TEXT.
  constants GC_MSG_CLASS type SY-MSGID value 'ZIDM_REEN_DEEN' ##NO_TEXT.
  constants GC_MSGNO_003 type SY-MSGNO value '003' ##NO_TEXT.
  constants GC_MSGTY_ERROR type SY-MSGTY value 'E' ##NO_TEXT.
  constants GC_CONT_STDATECHNG_ACT type EIDESWTACT value 'Y10' ##NO_TEXT.
  constants GC_MNGRP_ELE type MFGRP value 'ELE-' ##NO_TEXT.
  constants GC_MNCOD_LINKED_SN type MFCOD value '0234' ##NO_TEXT.
protected section.
private section.
ENDCLASS.



CLASS YCL_IDM_REMOTE_REEN_DEEN_DAO IMPLEMENTATION.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~ADD_ACTIVITY.

    IF iv_serv_notif IS NOT INITIAL.
      CALL FUNCTION 'BAPI_SERVNOT_ADD_DATA'
        EXPORTING
          number    = iv_serv_notif
        TABLES
          notifactv = ct_notif_activity
          return    = rt_return.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~ADD_SN_TO_SWITCH_ACTIVITY.

    DATA: ls_objectreference TYPE eideswtdocrefstruc,
          lt_objectreference TYPE teideswtdocrefstruc.

    CLEAR: es_return.
    IF iv_installation IS NOT INITIAL AND iv_notif_no IS NOT INITIAL.
**Get the switch doc number
      SELECT  a~switchnum,
              a~pod,
              a~status
            FROM eideswtdoc AS a INNER JOIN euiinstln AS b
            ON a~pod = b~int_ui
            INTO TABLE @DATA(lt_euiinstln_div)
            WHERE a~switchtype = @gc_swttype_mi
             AND  a~swtview = @gc_swtvw_trnsf_in
             AND b~anlage = @iv_installation.

      IF sy-subrc = 0.
        DELETE lt_euiinstln_div WHERE status EQ: 'Y4', 'Y5', 'Z8', 'Z9' .
        SORT lt_euiinstln_div BY switchnum DESCENDING.
        READ TABLE lt_euiinstln_div INTO DATA(ls_switch) INDEX 1.
        IF ls_switch IS NOT INITIAL.

          ls_objectreference-object = 'ISUSMNOTIF' .
          ls_objectreference-objectkey = CONV #( iv_notif_no ) .
          APPEND ls_objectreference TO lt_objectreference.

          DATA(lv_cont_st_date_chngd) = COND #( WHEN iv_hold EQ abap_true THEN zif_idm_remote_reen_deen_dao~is_contract_start_date_changed( ls_switch-switchnum )
                                                ELSE abap_false ).

          CALL METHOD cl_isu_switchdoc=>s_set_activity_status
            EXPORTING
              x_switchnum     = ls_switch-switchnum
              x_activity      = COND #( WHEN lv_cont_st_date_chngd EQ abap_true THEN 'Y11' ELSE 'Z09' )
              x_status        = '01'
              x_act_var1      = CONV #( iv_notif_no )
              xt_objectref    = lt_objectreference
            EXCEPTIONS
              not_found       = 1
              parameter_error = 2
              general_fault   = 3
              foreign_lock    = 4
              not_authorized  = 5
              OTHERS          = 6.
          IF sy-subrc EQ 0.
            es_return = VALUE #( type       = sy-msgty
                                 id         = sy-msgid
                                 message_v1 = sy-msgv1
                                 message_v2 = sy-msgv2
                                 message_v3 = sy-msgv3
                                 message_v4 = sy-msgv4 ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: lv_cont_st_date_chngd.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~APPEND_TEXT_IN_SPCL_INSTR.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference   Description
* 06/11/2020  Sourav Roy          CMI-9936    Phase 2 Remote Re-en/De-en: Auto Special Instructions on SN
*
*======================================================================================================*

    DATA: lt_line_bapi TYPE STANDARD TABLE OF bapi2080_notfulltxti,
          lt_return    TYPE STANDARD TABLE OF bapiret2,
          ls_return    TYPE bapiret2.

    CLEAR: rv_return_code.



    CASE iv_update_type.
      WHEN 'I'.
        APPEND is_line_bapi TO lt_line_bapi.

        CALL FUNCTION 'BAPI_SERVNOT_ADD_DATA'
          EXPORTING
            number     = CONV qmnum( iv_name )
          TABLES
            notfulltxt = lt_line_bapi
            return     = lt_return.

        IF lt_return[] IS INITIAL.
          CALL FUNCTION 'BAPI_SERVNOT_SAVE'
            EXPORTING
              number = CONV qmnum( iv_name )
            TABLES
              return = lt_return.

          IF lt_return[] IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = abap_true
              IMPORTING
                return = ls_return.

            IF ls_return IS INITIAL.
              rv_return_code = abap_true. "successful
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'M' OR 'A'.
        "Update text in special instruction in SN
        CALL FUNCTION 'CREATE_TEXT'
          EXPORTING
            fid         = iv_id
            flanguage   = iv_lang
            fname       = iv_name
            fobject     = iv_object
            save_direct = 'X'
            fformat     = ''
          TABLES
            flines      = it_line
          EXCEPTIONS
            no_init     = 1
            no_save     = 2
            OTHERS      = 3.
        IF sy-subrc = 0.
          rv_return_code = abap_true. "successful
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~CHECK_REEN_ELIG_4_TRANS_MOV_IN.


    CONSTANTS:lc_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEBAE9257754E2CEBCF'.
    DATA:lv_timestamp  TYPE timestamp,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt,
         lt_name_value TYPE abap_parmbind_tab,
         lv_cr_code    TYPE if_fdt_types=>element_text.
    FIELD-SYMBOLS <lv_any> TYPE any.

    REFRESH: rt_state_dereg_comb[].

    GET TIME STAMP FIELD lv_timestamp.


    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lc_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <lv_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lc_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rt_state_dereg_comb
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS NOT INITIAL.
          REFRESH:  rt_state_dereg_comb[].
        ENDIF.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~CHECK_STATE_MPB_BRF.

    CONSTANTS:lc_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEAB790573C3C404AE9'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt,
         lv_state      TYPE if_fdt_types=>element_text.
    FIELD-SYMBOLS <lv_any> TYPE any.

    GET TIME STAMP FIELD lv_timestamp.

    ls_name_value-name = 'STATE'.
    lv_state = iv_state.
    GET REFERENCE OF lv_state INTO lo_data.
    ls_name_value-value = lo_data.
    INSERT ls_name_value INTO TABLE lt_name_value.

    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lc_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <lv_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lc_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rt_mpb
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS NOT INITIAL.
          CLEAR rt_mpb.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~CHK_FUTURE_PAST_REMOTE_DEEN_SN.
*======================================================================================================*
*
* CHANGE HISTORY
*
* Date        Name                Reference    Description
* 01/12/2020  Sourav Roy          CMI-10311    Phase 2 Remote Re-en/De-en: Re-energisation SN - Non-Transfer
*
*======================================================================================================*

    CONSTANTS: lc_ele    TYPE qmgrp VALUE 'ELE-',
               lc_0027   TYPE qmcod VALUE '0027',
               lc_5_days TYPE dlydy VALUE 5.

    DATA: lv_past_five_dt TYPE sy-datum.

    CLEAR: rv_remote_deen_exists.

    "Get the date of past 5 days
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lc_5_days
        months    = '00'
        signum    = '-'
        years     = '00'
      IMPORTING
        calc_date = lv_past_five_dt.

    SELECT qmnum,
           strmn,
           qmgrp,
           qmcod
    FROM qmel
    INTO TABLE @DATA(lt_sn_remote_deen)
    WHERE zzanlage = @iv_installation.

    IF sy-subrc = 0.
      LOOP AT lt_sn_remote_deen ASSIGNING FIELD-SYMBOL(<ls_sn_remote_dn>) WHERE strmn >= lv_past_five_dt AND
                                                                                qmgrp = lc_ele AND
                                                                                qmcod = lc_0027.

        rv_remote_deen_exists = abap_true.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~CREATE_CONTACT_NOTE.

    DATA:lv_rfc TYPE rfcdest.

    CLEAR: rv_contact_id .

    IF it_lines[] IS NOT INITIAL.
      CALL FUNCTION 'ISM_CRM_RFC_DESTINATION_GET'
        IMPORTING
          pv_destination           = lv_rfc
        EXCEPTIONS
          no_destination_available = 1
          OTHERS                   = 2.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'ZCSS001_CREATE_CRM_ACTIVITY'
          DESTINATION lv_rfc
          EXPORTING
            x_partner             = iv_partner
            x_cont_acct           = iv_cont_acc
            x_description         = iv_description
            iv_reen               = abap_true
          IMPORTING
            y_contact_id          = rv_contact_id
          TABLES
            xt_text_lines         = it_lines[]
          EXCEPTIONS
            system_failure        = 1
            communication_failure = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          CLEAR rv_contact_id.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~CREATE_DUAL_SERVICE_NOTIF.

    DATA: ls_notifheader_r      TYPE bapi2080_nothdre,
          ls_notifheader_export TYPE bapi2080_nothdre,
          ls_notifheader_save   TYPE bapi2080_nothdre,
          lt_notifactv_r        TYPE STANDARD TABLE OF bapi2080_notactve,
          lt_notifpartnr_r      TYPE STANDARD TABLE OF bapi2080_notpartnre,
          lt_notif_actv_c       TYPE alm_me_bapi2080_notactvi_t,
          ls_notif_actv_c       TYPE bapi2080_notactvi,
          ls_notif_partnr_c     TYPE bapi2080_notpartnri,
          lt_notif_partnr_c     TYPE alm_me_bapi2080_notpartnri_t,
          lv_var_lock_key       TYPE rstable-varkey,
          lt_messtab            TYPE STANDARD TABLE OF bdcmsgcoll,
          ls_messtab            TYPE bdcmsgcoll,
          ls_status             TYPE bapi_status,
          lt_return             TYPE bapiret2_t,
          ls_return             TYPE bapiret2.

    REFRESH: et_return[].

    IF iv_parent_sn IS NOT INITIAL.
      CALL FUNCTION 'BAPI_SERVNOT_GET_DETAIL'
        EXPORTING
          number      = iv_parent_sn
        IMPORTING
          notifheader = ls_notifheader_r
        TABLES
          notifactv   = lt_notifactv_r
          notifpartnr = lt_notifpartnr_r.

      DATA(ls_notifheader_c) = VALUE bapi2080_nothdri( code_group = iv_code_group
                                                       coding = iv_coding
                                                       priority = COND #( WHEN ls_notifheader_r-priority IS NOT INITIAL THEN ls_notifheader_r-priority
                                                                          ELSE '3' )
                                                       desstdate = iv_start_date
                                                       short_text =  'Re-energisation (Remote)'(001)
                                                       zzanlage = iv_installation
                                                       zzextui  = iv_ext_ui
                                                       zzbrand = iv_brand
                                                       zzreceiver = iv_recipient_dual ).

      SET PARAMETER ID: 'ANL'             FIELD iv_installation,
                        'ZBD'             FIELD iv_brand,
                        'ZALTABL'         FIELD abap_true,
                        'ZZRECEIVER_AUTO' FIELD iv_recipient_dual.

      DATA(ls_par_sn_det) = zif_idm_remote_reen_deen_dao~fetch_sn_details( EXPORTING iv_sn = iv_parent_sn ).
      DATA(lv_dele_flag)  = zif_idm_remote_reen_deen_dao~is_sn_marked_for_deletion( ls_par_sn_det-objnr ).
      IF lv_dele_flag IS INITIAL.
        ls_notif_actv_c = VALUE #( act_key = '0001'
                                   act_sort_no = '0001'
                                   acttext = iv_parent_sn
                                   act_codegrp = 'ELE-'
                                   act_code = '0234'
                                   item_sort_no = '0001' ).
        APPEND ls_notif_actv_c TO lt_notif_actv_c.
        DATA(lv_sort_no) = '0001'.
      ENDIF.

      LOOP AT lt_notifactv_r INTO DATA(ls_notifactv_r).
        ls_notif_actv_c = VALUE #( act_key = lv_sort_no + 1
                                   act_sort_no = lv_sort_no + 1
                                   acttext = ls_notifactv_r-acttext
                                   act_codegrp = ls_notifactv_r-act_codegrp
                                   act_code = ls_notifactv_r-act_code
                                   start_date = ls_notifactv_r-start_date
                                   start_time = ls_notifactv_r-start_time
                                   end_date = ls_notifactv_r-end_date
                                   end_time = ls_notifactv_r-end_time
                                   item_sort_no = lv_sort_no + 1 ).
        APPEND ls_notif_actv_c TO lt_notif_actv_c.
        lv_sort_no = lv_sort_no + 1.
        CLEAR: ls_notif_actv_c,
               ls_notifactv_r.
      ENDLOOP.

      LOOP AT lt_notifpartnr_r INTO DATA(ls_notifpartnr_r).
        ls_notif_partnr_c = VALUE #( partn_role = ls_notifpartnr_r-partn_role
                                     partner    = ls_notifpartnr_r-partner ).
        APPEND ls_notif_partnr_c TO lt_notif_partnr_c.
        CLEAR: ls_notif_partnr_c,
               ls_notifpartnr_r.
      ENDLOOP.

      CALL FUNCTION 'BAPI_SERVNOT_CREATE'
        EXPORTING
          notif_type         = iv_notif_type
          notifheader        = ls_notifheader_c
        IMPORTING
          notifheader_export = ls_notifheader_export
        TABLES
          notifactv          = lt_notif_actv_c
          notifpartnr        = lt_notif_partnr_c
          return             = lt_return.

      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_SERVNOT_SAVE'
          EXPORTING
            number      = ls_notifheader_export-notif_no
          IMPORTING
            notifheader = ls_notifheader_save
          TABLES
            return      = lt_return.

        LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.

          IF ls_notifheader_save-notif_no IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
            IF iv_quicksn IS INITIAL.
              DO 2 TIMES.
                SELECT SINGLE *
                  FROM qmel
                  INTO @DATA(ls_qmel)
                  WHERE qmnum = @ls_notifheader_save-notif_no.
                IF sy-subrc EQ 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 5 SECONDS.                "#EC CI_WAITLOOP
                ENDIF.
              ENDDO.
              IF ls_qmel-ernam = gc_crm_rfc OR ls_qmel-aenam = gc_crm_rfc.
                lv_var_lock_key = |{ sy-mandt }{ ls_qmel-qmnum }|.
                ls_qmel-ernam = COND #( WHEN ls_qmel-ernam = gc_crm_rfc THEN iv_currentuser ELSE ls_qmel-ernam ).
                ls_qmel-aenam = COND #( WHEN ls_qmel-aenam = gc_crm_rfc THEN iv_currentuser ELSE ls_qmel-aenam ).
*   Enqueue source table
                CALL FUNCTION 'ENQUEUE_E_TABLE'
                  EXPORTING
                    mode_rstable   = 'E'
                    tabname        = 'QMEL'
                    varkey         = lv_var_lock_key
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.
                IF sy-subrc = 0.

*     Update table QMEL with Sales Order
                  CALL FUNCTION 'ISU_DB_QMEL_UPDATE'
                    EXPORTING
                      x_qmel     = ls_qmel
                      x_upd_mode = 'U'.

*     Dequeue table
                  CALL FUNCTION 'DEQUEUE_E_TABLE'
                    EXPORTING
                      mode_rstable = 'E'
                      tabname      = 'QMEL'
                      varkey       = lv_var_lock_key.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.


          rv_dual_sn = ls_notifheader_save-notif_no.

          IF iv_hold EQ abap_true.
            ls_status-intern = 'E0014'.
            CALL FUNCTION 'BAPI_ISUSMNOTIF_USERSTATUSSET'
              EXPORTING
                number = ls_notifheader_export-notif_no
                status = ls_status
              IMPORTING
                return = ls_return.

            IF ls_return-type CA 'EAX'.
              APPEND ls_return TO lt_return.
            ENDIF.

            CLEAR ls_return.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = abap_true
              IMPORTING
                return = ls_return.
            IF ls_return-type CA 'EAX'.
              APPEND ls_return TO lt_return.
            ENDIF.
          ELSE.

            CALL FUNCTION 'ZISS343_DETERMINETASK_SAVE'
              EXPORTING
                x_qmnum    = ls_notifheader_save-notif_no
              TABLES
                yt_messtab = lt_messtab.

            LOOP AT lt_messtab INTO ls_messtab WHERE msgtyp CA 'EAX'.
              CLEAR: ls_return.
              ls_return = VALUE #( type        = ls_messtab-msgtyp
                                   id          = ls_messtab-msgid
                                   number      = ls_messtab-msgnr
                                   message_v1  = ls_messtab-msgv1
                                   message_v2  = ls_messtab-msgv2
                                   message_v3  = ls_messtab-msgv3
                                   message_v4  = ls_messtab-msgv4 ).
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.

          CALL FUNCTION 'IQS1_DELETE_NOTIF_FROM_BUFFER'
            EXPORTING
              i_qmnum                    = ls_notifheader_save-notif_no
            EXCEPTIONS
              notification_not_in_buffer = 1
              number_initial             = 2
              OTHERS                     = 3.

          IF sy-subrc <> 0.
            CLEAR: ls_return.
            ls_return = VALUE #( type        = sy-msgty
                                 id          = sy-msgid
                                 number      = sy-msgno
                                 message_v1  = sy-msgv1
                                 message_v2  = sy-msgv2
                                 message_v3  = sy-msgv3
                                 message_v4  = sy-msgv4 ).
            APPEND ls_return TO lt_return.
          ENDIF.
          et_return[] = lt_return[].
        ELSE.
          APPEND LINES OF lt_return TO et_return.
        ENDIF.
      ELSE.
        APPEND LINES OF lt_return TO et_return.
      ENDIF.
    ELSE.
      CLEAR: ls_return.
      ls_return = VALUE #( type        = gc_msgty_error
                           id          = gc_msg_class
                           number      = gc_msgno_003
                           message     = 'Primary SN is not provided'(002) ).
      APPEND ls_return TO lt_return.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~DELINK_PARENT_CHILD_SNS.

    DATA: lt_return TYPE bapiret2_t.

    IF iv_parent_sn IS NOT INITIAL.
      DATA(lv_linked_sn) = zif_idm_remote_reen_deen_dao~get_linked_sn( iv_parent_sn ).
      IF lv_linked_sn IS NOT INITIAL.
        lt_return = zif_idm_remote_reen_deen_dao~remove_linked_sn( iv_parent_sn ).
        IF lt_return[] IS NOT INITIAL.
          APPEND LINES OF lt_return TO rt_return.
        ENDIF.
        WAIT UP TO 2 SECONDS.
        REFRESH: lt_return[].
        lt_return =  zif_idm_remote_reen_deen_dao~remove_linked_sn( lv_linked_sn ).
        IF lt_return[] IS NOT INITIAL.
          APPEND LINES OF lt_return TO rt_return.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_ACTIVE_EASTL_ENTRIES.
*======================================================================*
* CHANGE HISTORY
*-----------------------------------------------------------------------
* Date         Name                Reference   Description
*-----------------------------------------------------------------------
* 24/08/2020  Swagata Mukherjee   CMI-8845   Remote Re-en/De-en:
*                                 Dereg status update when meter is installed
*
*======================================================================*

    CONSTANTS: lc_last_date TYPE biszeitsch VALUE '99991231'.
    CLEAR : rt_eastl_logiknr[].
    CHECK iv_anlage IS NOT INITIAL.

    SELECT anlage,
      logiknr,
      bis
      FROM eastl
      INTO TABLE @rt_eastl_logiknr
      WHERE anlage = @iv_anlage
      AND bis = @lc_last_date.
    IF sy-subrc NE 0.
      CLEAR : rt_eastl_logiknr[].
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_ACTIVITY_NO.

    IF iv_sn IS NOT INITIAL.
      SELECT qmnum
             manum
             mngrp
             mncod
             matxt
             kzloesch
      FROM qmma
      INTO TABLE rt_activity
      WHERE qmnum = iv_sn.
      IF sy-subrc EQ 0.
        DELETE rt_activity WHERE kzloesch EQ abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_BUSINESS_PARTNER.

    IF iv_contract_account IS NOT INITIAL.
      SELECT gpart
        FROM fkkvkp
        UP TO 1 ROWS
        INTO rv_business_partner
        WHERE vkont = iv_contract_account.
      ENDSELECT.
      IF sy-subrc <> 0.
        CLEAR rv_business_partner.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_CONTRACT_ACCOUNT.

    IF iv_partner IS NOT INITIAL.
      SELECT vkont
        FROM fkkvkp
        UP TO 1 ROWS
        INTO rv_cont_account
        WHERE gpart = iv_partner.
      ENDSELECT.
      IF sy-subrc <> 0.
        CLEAR rv_cont_account.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_DEEN_TYPE_BRF.

    CALL FUNCTION 'ZICA321_RETRIEVE_CUSTOM_DATA'
      EXPORTING
        x_trans_id = iv_trans_id
      TABLES
        t_data     = rt_deen_data.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_DERAGSTATUS_INSTALL_TYPE.

    CLEAR: ev_install_type , ev_deragstatus.
    IF iv_anlage IS NOT INITIAL.
      SELECT SINGLE anlart
                   deregstat
          INTO ( ev_install_type , ev_deragstatus )
          FROM eanl
          WHERE anlage = iv_anlage.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_DEREG_STAT_FROM_ZCRT.
*==============================================================================================*
* CHANGE HISTORY
* Date        Name                Reference   Description
* 24/08/2020  Swagata Mukherjee   CMI-8845    Remote Re-en/De-en: Dereg status update when meter is installed
*
*==============================================================================================*

    CONSTANTS: lc_trans_id TYPE z_trans_id VALUE 'NMDDEREGSTAT',
               lc_elec     TYPE sparte     VALUE '01'.
    DATA:      lt_dereg       TYPE ziiddt020.

    CLEAR: rv_dereg_status.
    CHECK iv_io_grp IS NOT INITIAL.

    CALL FUNCTION 'ZICA002_RETRIEVE_CUSTOM_DATA'
      EXPORTING
        x_trans_id           = lc_trans_id
      TABLES
        yt_values            = lt_dereg
      EXCEPTIONS
        invalid_trans_id     = 1
        deactivated_trans_id = 2
        OTHERS               = 3.
    IF sy-subrc = 0.
      READ TABLE lt_dereg INTO DATA(ls_dereg) WITH KEY value2 = iv_io_grp
                                                       value3 = lc_elec.
      IF sy-subrc = 0.
        rv_dereg_status = ls_dereg-value1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_IO_GROUP.
*======================================================================*
* CHANGE HISTORY
*-----------------------------------------------------------------------
* Date         Name                Reference   Description
*-----------------------------------------------------------------------
* 24/08/2020  Swagata Mukherjee   CMI-8845   Remote Re-en/De-en:
*                                 Dereg status update when meter is installed
*
*======================================================================*
     CLEAR rt_egerr_io_grp[].
     CHECK it_eastl_logiknr[] IS NOT INITIAL.

    SELECT equnr,
        bis,
        logiknr,
        eagruppe
        FROM egerr
        INTO TABLE @rt_egerr_io_grp
        FOR ALL ENTRIES IN @it_eastl_logiknr
        WHERE logiknr = @it_eastl_logiknr-logiknr
        AND bis = @it_eastl_logiknr-bis.
    IF sy-subrc NE 0.
      CLEAR: rt_egerr_io_grp[].
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_NMI_CLASS_BRF.

    CONSTANTS:lc_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEAB8A39616EADD2AEB'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt.
    FIELD-SYMBOLS <lv_any> TYPE any.

    GET TIME STAMP FIELD lv_timestamp.

    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lc_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <lv_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lc_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rv_nmi_class
                                          CHANGING  ct_name_value  = lt_name_value ).

      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS NOT INITIAL.
          CLEAR rv_nmi_class.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_NMI_CLASS_DB.

    IF iv_int_ui IS NOT INITIAL.
      SELECT SINGLE ui_cls_code
        INTO rv_nmi_class
        FROM euihead
        WHERE int_ui = iv_int_ui.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_RECEIVER.

    IF iv_sn IS NOT INITIAL.
      SELECT SINGLE zzreceiver
        FROM qmel
        INTO rv_receiver
        WHERE qmnum = iv_sn.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_REQD_START_DATE.

    CLEAR: ev_date, ev_receiver.

    IF iv_serv_notif IS NOT INITIAL.
      SELECT SINGLE strmn
                   zzreceiver
        FROM qmel
        INTO ( ev_date , ev_receiver )
        WHERE qmnum = iv_serv_notif.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_SERVICE_PROVIDER.

    CLEAR rv_service_prov.

    IF iv_service_type IS NOT INITIAL AND iv_int_ui IS NOT INITIAL.
      SELECT  serviceid,
              service_start,
              service_end
        FROM eservice
        INTO TABLE @DATA(lt_service_prov)
       WHERE int_ui = @iv_int_ui AND
             service = @iv_service_type .
      IF sy-subrc = 0.
        DELETE lt_service_prov WHERE ( service_start GT sy-datum OR service_end LT sy-datum ).
        IF lt_service_prov[] IS NOT INITIAL.
          READ TABLE lt_service_prov INTO DATA(ls_serv) INDEX 1.
          IF sy-subrc = 0.
            rv_service_prov = ls_serv-serviceid.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_SERVICE_TYPE.

    CLEAR  rv_service_type.
    IF iv_serv_prov IS NOT INITIAL.
      SELECT SINGLE service
        FROM eservprov
        INTO rv_service_type
        WHERE serviceid = iv_serv_prov.
      IF sy-subrc <> 0.
        CLEAR rv_service_type.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_SERVID_FROM_EXTID_ROLE.

    CLEAR: rv_serviceid.
    SELECT SINGLE serviceid                             "#EC CI_NOORDER
      FROM eservprov
      INTO rv_serviceid
      WHERE service = iv_service
      AND externalid = iv_externalid.
    IF sy-subrc <> 0.
      CLEAR: rv_serviceid.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~FETCH_SN_DETAILS.

    CLEAR rs_sn_det.

    IF iv_sn IS NOT INITIAL.
      SELECT SINGLE qmnum
                    objnr
        FROM qmel
        INTO rs_sn_det
        WHERE qmnum = iv_sn.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~GET_DEEN_TYPE_CODE.

    CLEAR: ev_code_grp,ev_type_code,ev_reci_code,ev_exception.
*- Deen Type Code From BRF
    CALL FUNCTION 'Z_IDM_DEEN_TYPE_CODE'
      EXPORTING
        iv_anlage    = iv_anlage
        iv_jurs      = iv_jurs
      IMPORTING
        rv_ord_type  = ev_code_grp
        rv_type_code = ev_type_code
        rv_rt        = ev_exception
        rv_reci_code = ev_reci_code.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~GET_LINKED_SN.
    CLEAR: rv_linked_sn.

    IF iv_source_sn IS NOT INITIAL.
      SELECT mngrp,
             mncod,
             matxt,
             erdat,
             kzloesch
        FROM qmma
        INTO TABLE @DATA(lt_matxt)
        WHERE qmnum = @iv_source_sn.
      IF sy-subrc EQ 0.
        DELETE lt_matxt WHERE ( mngrp NE gc_mngrp_ele OR mncod NE gc_mncod_linked_sn ) OR kzloesch EQ abap_true.
        SORT lt_matxt BY erdat DESCENDING.
        READ TABLE lt_matxt INTO DATA(ls_matxt) INDEX 1.
        IF sy-subrc EQ 0.
          CONDENSE: ls_matxt-matxt.
          rv_linked_sn = ls_matxt-matxt.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~IS_CONTRACT_START_DATE_CHANGED.

    CLEAR: rv_date_changed_flag.
    IF iv_switchnum IS NOT INITIAL.
      SELECT switchnum,
             activity
        FROM eideswtdocstep
        INTO TABLE @DATA(lt_eideswtdocstep)
        WHERE switchnum = @iv_switchnum.
      IF sy-subrc EQ 0 AND lt_eideswtdocstep[] IS NOT INITIAL.
        DELETE lt_eideswtdocstep WHERE activity NE gc_cont_stdatechng_act.
        IF lt_eideswtdocstep[] IS NOT INITIAL.
          rv_date_changed_flag = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~IS_SN_MARKED_FOR_DELETION.

    CLEAR: rv_marked_for_deletion.

    SELECT COUNT(*)
      FROM jest
      WHERE objnr = iv_objnr
      AND stat = gc_del_stat
      AND inact EQ abap_false.
    IF sy-dbcnt NE 0.
      rv_marked_for_deletion = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~REMOVE_LINKED_SN.

    DATA: lt_notifactv TYPE STANDARD TABLE OF bapi2080_notactve,
          lt_notactvi  TYPE STANDARD TABLE OF bapi2080_notactvi,
          ls_notactvi  TYPE bapi2080_notactvi,
          lt_return    TYPE bapiret2_t.

    IF iv_sn IS NOT INITIAL.
      CALL FUNCTION 'BAPI_SERVNOT_GET_DETAIL'
        EXPORTING
          number    = iv_sn
        TABLES
          notifactv = lt_notifactv.

      DELETE lt_notifactv WHERE act_codegrp NE 'ELE-' OR act_code NE '0234'.
      LOOP AT lt_notifactv INTO DATA(ls_notifactv).
        ls_notactvi = CORRESPONDING #( ls_notifactv ).
        ls_notactvi-refobjectkey  = iv_sn.
        APPEND ls_notactvi TO lt_notactvi.
        CLEAR: ls_notactvi,
               ls_notifactv.
      ENDLOOP.

      CALL FUNCTION 'BAPI_SERVNOT_DEL_DATA'
        EXPORTING
          number    = iv_sn
        TABLES
          notifactv = lt_notactvi
          return    = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_SERVNOT_SAVE'
          EXPORTING
            number = iv_sn
          TABLES
            return = lt_return.
        LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          rt_return[] = lt_return[].
        ENDIF.
      ELSE.
        rt_return[] = lt_return[].
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IDM_REMOTE_REEN_DEEN_DAO~SAVE_SERVICE_NOTIFICATION.

    DATA: lt_return TYPE bapiret2_t,
          ls_return TYPE bapiret2.

    IF iv_sn IS NOT INITIAL.
      CALL FUNCTION 'BAPI_SERVNOT_SAVE'
        EXPORTING
          number = iv_sn
        TABLES
          return = lt_return.
      READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: ls_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = abap_true
          IMPORTING
            return = ls_return.
        IF ls_return-type NE 'E' AND ls_return-type NE 'A'.
          rv_succ = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
