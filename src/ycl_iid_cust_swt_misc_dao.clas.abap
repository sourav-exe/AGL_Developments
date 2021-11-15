class YCL_IID_CUST_SWT_MISC_DAO definition
  public
  final
  create public .

public section.

  interfaces ZIF_IID_CUST_SWT_MISC_DAO .
protected section.
private section.
ENDCLASS.



CLASS YCL_IID_CUST_SWT_MISC_DAO IMPLEMENTATION.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~ADD_ACTIVITY.

    DATA: lv_step_key TYPE eideswtstepkey.
    CALL FUNCTION 'Z_IID_ADD_SWITCH_ACTIVITY'
      EXPORTING
        x_switch        = iv_switch_no
        x_activity      = iv_activity
        x_act_status    = iv_status
        x_act_text_var1 = iv_act_text_var1
        x_act_text_var2 = iv_act_text_var2
        x_act_text_var3 = iv_act_text_var3
        x_act_text_var4 = iv_act_text_var4
        x_refobj_type   = CONV swo_objtyp( 'USER' )
        x_refobj_key    = CONV text50( sy-uname )
*       X_ATTRIBUTEVAL  =
      IMPORTING
        ev_stepkey      = lv_step_key
      EXCEPTIONS
        not_found       = 1
        general_fault   = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~DELETE_NON_BILLABLE_SERVICES.

    DATA : ls_auto         TYPE isuedi_nbservice_auto,
           lv_success_temp TYPE flag VALUE 'X'.

    CLEAR: rv_successful.
    IF it_services IS NOT INITIAL.
      "Check whether services are non-billable
      SELECT vertrag FROM eservice
                     INTO TABLE @DATA(lt_service)
                     FOR ALL ENTRIES IN @it_services
                     WHERE vertrag = @it_services-vertrag.

      IF lines( it_services ) = lines( lt_service ).

        LOOP AT it_services ASSIGNING FIELD-SYMBOL(<fs_service>).

          CALL FUNCTION 'ISU_S_NBSERVICE_PROVIDE'
            EXPORTING
              x_vertrag      = <fs_service>-vertrag
              x_wmode        = '2' " CO_CHANGE
            IMPORTING
              y_auto         = ls_auto
            EXCEPTIONS
              not_found      = 1
              foreign_lock   = 2
              general_fault  = 3
              not_authorized = 4
              invalid_wmode  = 5
              OTHERS         = 6.
          IF sy-subrc EQ 0.
            ls_auto-eserviced_use = 'X'.
            ls_auto-eserviced-loevm = 'X'.
            ls_auto-eserviced-service_start =  '00000000'.
            ls_auto-eserviced-service_end =  '00000000'.

            CALL FUNCTION 'ISU_S_NBSERVICE_CHANGE'
              EXPORTING
                x_vertrag      = <fs_service>-vertrag
                x_upd_online   = 'X'
                x_no_dialog    = 'X'
                x_auto         = ls_auto
              EXCEPTIONS
                not_found      = 1
                foreign_lock   = 2
                general_fault  = 3
                input_error    = 4
                not_authorized = 5
                OTHERS         = 6.
            IF sy-subrc <> 0.
              CLEAR lv_success_temp.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lv_success_temp = abap_true.
          rv_successful = abap_true.
          COMMIT WORK.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~FETCH_INVALID_STATUS_INFO.

    CONSTANTS:lc_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEBA4A67E8F05DA4BA5'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lr_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt.
    FIELD-SYMBOLS <la_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lc_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lr_data ).
    ASSIGN lr_data->* TO <la_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lc_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = <la_any>
                                          CHANGING  ct_name_value  = lt_name_value ).
        rt_invalid_status_info = CORRESPONDING #( <la_any> ).
      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS BOUND.
          CLEAR: rt_invalid_status_info.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_ACTIVE_CONTRACTS_CRM.

    DATA: lv_rfcdest TYPE rfcdest.
    "GET the rfc DESTINATION
    CALL FUNCTION 'ISM_CRM_RFC_DESTINATION_GET'
      IMPORTING
        pv_destination           = lv_rfcdest
      EXCEPTIONS
        no_destination_available = 1
        OTHERS                   = 2.

    IF sy-subrc EQ 0.
      "get active ISU contracts from CRM
      CALL FUNCTION 'Z_CSV_CRM_CONTRACT_EXISTENCE' DESTINATION lv_rfcdest
        EXPORTING
          iv_pod                 = iv_pod
          iv_keydate             = sy-datum
        IMPORTING
          et_active_isu_contract = rt_contract
        EXCEPTIONS
          communication_failure  = 1.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_CONTRACT_DETAILS.

    DATA: lv_int_ui TYPE int_ui.
    CLEAR: et_ever, ev_division, ev_installation.

    IF iv_contract IS NOT INITIAL.
      SELECT vertrag
             bukrs
             sparte
             kofiz
             serviceid
             anlage
        FROM ever
        INTO TABLE et_ever
        WHERE vertrag = iv_contract.

      IF sy-subrc = 0.
        "Get Division
        ev_division = VALUE #( et_ever[ 1 ]-sparte OPTIONAL ).
        "Get Installation
        SELECT SINGLE anlage
                FROM eeinv
                INTO ev_installation
                WHERE vertrag = iv_contract.
        "Get POD
        IF ev_installation IS NOT INITIAL.
          SELECT SINGLE int_ui
                    FROM euiinstln
                    INTO ev_pod
                    WHERE anlage = ev_installation.
        ENDIF.

        et_ever[ 1 ]-anlage = ev_installation.
        et_ever[ 1 ]-int_ui = ev_pod.
      ELSE.
*In-flight Contract
        IF iv_in_flight_contract = abap_true.
          "Get RFC Destination
          DATA(lv_rfc_dest) = zcl_isu_coll_strat_util=>determine_crm_rfc_dest( ).

          IF lv_rfc_dest IS NOT INITIAL.
            "Get POD
            CALL FUNCTION 'ZCCS579_GET_ISU_POD' DESTINATION lv_rfc_dest
              EXPORTING
                iv_object_id = iv_contract
              IMPORTING
                ev_int_ui    = lv_int_ui.
            ev_pod = lv_int_ui.
          ENDIF.
          IF lv_int_ui IS NOT INITIAL.
* Get Installation
            SELECT SINGLE anlage
                    FROM euiinstln
                    INTO ev_installation
                    WHERE int_ui = lv_int_ui.
            IF sy-subrc = 0.
* Get Division
              SELECT SINGLE sparte
                      FROM eanl
                      INTO ev_division
                      WHERE anlage = ev_installation.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_CONTRACT_POD.

    CONSTANTS : lc_eod(8) TYPE c VALUE '99991231'.
    DATA: lv_contract          TYPE vertrag,
          lv_crm_contract      TYPE vertrag,
          lv_crm_move_out_date TYPE auszdat,
          lv_installation      TYPE anlage,
          lv_rfcdest           TYPE crmrfcpar-rfcdest.

    CLEAR: rv_contract.
    CALL FUNCTION 'ISU_INT_UI_DETERMINE'
      EXPORTING
        x_int_pod         = iv_int_ui
      IMPORTING
        y_contract        = lv_contract
        y_anlage          = lv_installation
      EXCEPTIONS
        not_found         = 1
        programming_error = 2
        system_error      = 3
        OTHERS            = 4.

    IF sy-subrc = 0 AND
       lv_contract IS INITIAL.

      SELECT  vertrag, auszdat
      FROM ever INTO TABLE @DATA(lt_ever)
      WHERE anlage  = @lv_installation.

      IF sy-subrc = 0.
        SORT lt_ever BY auszdat DESCENDING.
        lv_contract = lt_ever[ 1 ]-vertrag.
        DATA(lv_move_out_date) = lt_ever[ 1 ]-auszdat.
      ENDIF.

      IF lv_move_out_date NE lc_eod.
        "GET the rfc DESTINATION
        CALL FUNCTION 'ISM_CRM_RFC_DESTINATION_GET'
          IMPORTING
            pv_destination           = lv_rfcdest
          EXCEPTIONS
            no_destination_available = 1
            OTHERS                   = 2.
        IF sy-subrc = 0.
          CALL FUNCTION 'ZCSV329_GET_CRM_CONTRACT_ID' DESTINATION lv_rfcdest
            EXPORTING
              x_pod_guid  = iv_int_ui
            IMPORTING
              y_object_id = lv_crm_contract
              y_end_date  = lv_crm_move_out_date.

          IF ( lv_move_out_date IS NOT INITIAL AND lv_move_out_date LE lv_crm_move_out_date )
             OR  lv_contract IS INITIAL.
            lv_contract =  lv_crm_contract.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    rv_contract = lv_contract.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_STATUS_TEXT.

    SELECT * FROM eideswtstatust
             INTO TABLE rt_status_text
             WHERE spras = 'EN'.
    IF sy-subrc <> 0.
      CLEAR: rt_status_text.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_SUPPLY_SCENARIO.

    "Get supply scenario
    CALL METHOD cl_isu_ide_drgscen_ana_pod=>create
      EXPORTING
        im_int_ui           = iv_int_pod
        im_bypassing_buffer = space
      IMPORTING
        ex_ref              = DATA(lo_supply_scenario)
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc = 0.
      rt_scenario = lo_supply_scenario->iscenario.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_SWITCH_CURRENT_STATUS.

    CLEAR: rt_switch_status.
    IF it_switch_no IS NOT INITIAL.
      SELECT switchnum
             status
        FROM eideswtdoc
        INTO TABLE rt_switch_status
        WHERE switchnum IN it_switch_no.

      IF sy-subrc <> 0.
        CLEAR: rt_switch_status.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~GET_SWITCH_DOCUMENT_STEP.

    CLEAR: rt_switch_doc_step.
    IF it_switch_no IS NOT INITIAL.
      SELECT *
        FROM eideswtdocstep
        INTO TABLE rt_switch_doc_step
        WHERE switchnum IN it_switch_no.

      IF sy-subrc <> 0.
        CLEAR: rt_switch_doc_step.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_DAO~MODIFY_SERVICES.

    DATA lv_loghandle TYPE balloghndl.

    CLEAR: rv_successful.

    CALL METHOD cl_isu_ide_drgscen_gen_pod=>process
      EXPORTING
        im_int_ui          = iv_int_pod
        im_scenario        = iv_supply_scenario
        im_datefrom        = iv_from_date
*       im_switchnum       = switchnum
*       im_iparamcont      = parametercontainer
        im_iserviceid      = it_serviceid
*       im_gpart           = p_bp
*       im_vstelle         = vstelle
* START OF SIR#3549
        im_only_services   = ''  "onlyservices
* END OF SIR#3549
*       im_only_noscenario = onlynoscenario
*       im_activity_create = activitycreate
*       im_activity_stop   = activitystop
*       im_activity_cancel = activitycancel
*       im_activity_change = activitychange
*       im_activity_shift  = activityshift
*       im_status_failed   = statusfailed
*       im_status_done     = statusdone
        im_no_dialog       = 'X'
        im_commit_log      = 'X'
        im_services_dateto = iv_to_date
      CHANGING
        ch_log_handle      = lv_loghandle
      EXCEPTIONS
        OTHERS             = 1.


    IF sy-subrc = 0.
      CALL FUNCTION 'ZICS020_SET_SUPPSCEN_PROC'
        EXPORTING
          x_process = 'X'.
      rv_successful = abap_true.
    ELSE.
      CALL FUNCTION 'ZICS020_SET_SUPPSCEN_PROC'
        EXPORTING
          x_process = ''.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
