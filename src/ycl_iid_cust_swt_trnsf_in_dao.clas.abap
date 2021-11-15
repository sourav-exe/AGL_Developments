class YCL_IID_CUST_SWT_TRNSF_IN_DAO definition
  public
  final
  create public .

public section.

  interfaces ZIF_IID_CUST_SWT_TRNSF_IN_DAO .

  types:
    BEGIN OF  ty_meter_status ,
        status TYPE z_createmeter_status,
      END OF ty_meter_status .
  types:
    BEGIN OF ty_read_quality,
        read_quality TYPE z_quality_method,
      END OF ty_read_quality .
  types:
    tty_meter_status TYPE TABLE OF ty_meter_status .
  types:
    tty_read_quality TYPE TABLE OF ty_read_quality .

  class-methods MATCH_PATTERNS
    importing
      !IV_PATTERNS type STRING
      !IV_STRING type STRING
    returning
      value(RV_FLAG) type ABAP_BOOL .
  class-methods GET_NMI_CLASS_FRM_SEGMENT
    importing
      !IV_IDOC_NO type EDI_DOCNUM optional
      !IV_IDOC_DATA type IDOC_DATA
    returning
      value(RV_NMI_CLASS) type Z_UICLSCODE .
  class-methods GET_IDOC_DATA
    importing
      !IV_IDOC_NO type EDI_DOCNUM
    returning
      value(RT_IDOC_DATA) type IDOC_DATA .
  class-methods GET_ZMETERNMI_SEG_DETAILS
    importing
      !IV_IDOC_DATA type IDOC_DATA
      !IT_METER_STATUS type TTY_METER_STATUS
    returning
      value(RT_METERNMI) type ZICSDT524 .
  class-methods GET_INST_READ_TYPE_CODE
    importing
      !IT_PFC_DT type ZIDMTT_PFC_DECISION_TABLE
      !IT_ZMETERNMI_SEGMENT type ZICSDT524
    returning
      value(RT_PFC_RESULT) type ZIDMTT_PFC_DECISION_TABLE .
  class-methods GET_SPID_RESPONSE
    importing
      !IV_INT_POD type INT_UI
      !IV_JURISDICTION type Z_JURISDICTION optional
      !IV_DATE_TO type DATUM default SY-DATUM
      !IV_DATE_FROM type DATUM optional
      !IV_SPID_BD type NUM4 optional
    returning
      value(RV_IDOC_NO) type EDI_DOCNUM .
  class-methods GET_METER_TYPE_AND_DATE
    importing
      !IV_INT_POD type INT_UI
      !IV_DATE type DATUM
      !IT_READ_QUALITY_DT type TTY_READ_QUALITY
      !IV_JURISDICTION type Z_JURISDICTION
      !IT_PFC_RESULT type ZIDMTT_PFC_DECISION_TABLE
      !IT_IDOC_DATA type IDOC_DATA
    returning
      value(RS_PFC_VALID_RESULT) type ZIDMST_PFC_VALID_RULE_RESULT .
protected section.
private section.
ENDCLASS.



CLASS YCL_IID_CUST_SWT_TRNSF_IN_DAO IMPLEMENTATION.


  METHOD GET_IDOC_DATA.

    IF iv_idoc_no IS NOT INITIAL .
      CALL FUNCTION 'IDOC_READ_COMPLETELY'
        EXPORTING
          document_number         = iv_idoc_no
        TABLES
          int_edidd               = rt_idoc_data
        EXCEPTIONS
          document_not_exist      = 1
          document_number_invalid = 2
          OTHERS                  = 3.
    ENDIF.
  ENDMETHOD.


  METHOD GET_INST_READ_TYPE_CODE.

    DATA: lv_inst_flag TYPE abap_bool,
          lv_read_flag TYPE abap_bool.

    LOOP AT it_zmeternmi_segment ASSIGNING FIELD-SYMBOL(<fs_zmeternmi_seg>).

      LOOP AT it_pfc_dt ASSIGNING FIELD-SYMBOL(<fs_pfc_dt>).

        CLEAR: lv_inst_flag, lv_read_flag.
*       Installation Type Code
        lv_inst_flag = match_patterns( iv_patterns = CONV #( <fs_pfc_dt>-meter_type )
                                       iv_string = CONV #( <fs_zmeternmi_seg>-installation_type_code ) ).

*       Read Type Code
        lv_read_flag = match_patterns( iv_patterns = CONV #( <fs_pfc_dt>-read_type_code )
                                       iv_string = CONV #( <fs_zmeternmi_seg>-readtypecode ) ).

        IF lv_inst_flag = abap_true AND
           lv_read_flag = abap_true.
          rt_pfc_result = VALUE #( BASE rt_pfc_result ( meter_type = <fs_zmeternmi_seg>-installation_type_code
                                                        read_type_code = <fs_zmeternmi_seg>-readtypecode
                                                        remotely_read = <fs_pfc_dt>-remotely_read
                                                        manually_read = <fs_pfc_dt>-manually_read
                                                        prospective_bd = <fs_pfc_dt>-prospective_bd
                                                        retrospective_bd = <fs_pfc_dt>-retrospective_bd ) ).
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT rt_pfc_result BY meter_type read_type_code.
    DELETE ADJACENT DUPLICATES FROM rt_pfc_result.

  ENDMETHOD.


  METHOD GET_METER_TYPE_AND_DATE.

    CONSTANTS: lc_msgid              TYPE symsgid VALUE 'ZIID_CUSTSWT_TRNSFIN',
               lc_msgno_002          TYPE symsgno VALUE '002',
               lc_msgno_005          TYPE symsgno VALUE '005',
               lc_pve_exception_flag TYPE char1 VALUE 'P'.


    DATA: lt_prev_read_dt_qlty TYPE TABLE OF zpreviousreaddt.

    CHECK it_pfc_result IS NOT INITIAL.

    DEFINE populate_error.
      rs_pfc_valid_result-exception_flag = &1.
      rs_pfc_valid_result-msg_id = &2 .
      rs_pfc_valid_result-msg_no = &3.
    END-OF-DEFINITION.



    DATA(lt_pfc_result) = it_pfc_result.
    SORT lt_pfc_result BY remotely_read manually_read.
    DELETE ADJACENT DUPLICATES FROM lt_pfc_result COMPARING remotely_read manually_read.

    IF lines( it_pfc_result ) > 1.
      populate_error abap_true lc_msgid lc_msgno_002.
    ELSE.
      DATA(lv_calendar) = NEW zcl_idm_ls_gen_process( )->zif_idm_ls_gen_process~get_calendar( x_jurisdiction = iv_jurisdiction ).

      DATA(lv_retro_days) = CONV i( |-{ it_pfc_result[ 1 ]-retrospective_bd }| ).
      DATA(lv_start_date) = NEW zcl_iid_crr_process( )->zif_iid_crr_process~add_working_day( EXPORTING iv_calender = lv_calendar
                                                                                                       iv_date  = iv_date
                                                                                                       iv_days  = lv_retro_days ).
      DATA(lv_pros_days) = CONV i( |{ it_pfc_result[ 1 ]-prospective_bd }| ).
      DATA(lv_end_date) = NEW zcl_iid_crr_process( )->zif_iid_crr_process~add_working_day( EXPORTING iv_calender = lv_calendar
                                                                                                     iv_date  = iv_date
                                                                                                     iv_days  = lv_pros_days ).


      lv_start_date = COND #( WHEN lv_start_date IS INITIAL THEN iv_date
                              ELSE lv_start_date ).

      lv_end_date = COND #( WHEN lv_end_date IS INITIAL THEN iv_date
                            ELSE lv_end_date ).

      "Get supply scenario
      CALL METHOD cl_isu_ide_drgscen_ana_pod=>create
        EXPORTING
          im_int_ui           = iv_int_pod
          im_bypassing_buffer = space
        IMPORTING
          ex_ref              = DATA(lo_supply_scenario)
        EXCEPTIONS
          OTHERS              = 1.

      DATA(lt_supply_scenario_all) = lo_supply_scenario->iscenario.


      "create filter table
      DATA(lt_supply_scenario_filter) = VALUE isu00_range_tab(  sign = 'I' option ='EQ' ( low = '001')
                                                                                        ( low = '002')
                                                                                        ( low = '003')
                                                                                        ( low = '004') ).
      SORT lt_supply_scenario_all BY dateto DESCENDING.

      LOOP AT lt_supply_scenario_all ASSIGNING FIELD-SYMBOL(<fs_supply>).
        IF <fs_supply>-scenario NOT IN lt_supply_scenario_filter. "Exclude irrelevant supply scenarios
          DATA(lv_new_start_date) = CONV datum( <fs_supply>-datefrom ).
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lines( lt_supply_scenario_all ) = 1 AND
        ( lt_supply_scenario_all[ 1 ]-scenario = '008' OR
          lt_supply_scenario_all[ 1 ]-scenario = '005' ).
*        Skipping the new start date check since this a new TMD
      ELSE.
        lv_start_date = COND #( WHEN lv_start_date < lv_new_start_date OR lv_new_start_date IS INITIAL THEN lv_new_start_date
                                ELSE lv_start_date ).
      ENDIF.

      "Get header info
      DATA(ls_header) = VALUE zheader( it_idoc_data[ segnam = 'ZHEADER' ]-sdata OPTIONAL ).

      "Get current PVE period
      DATA(lv_pve_period) = NEW zcl_iid_cust_swt_trnsf_in_utl( )->zif_iid_cust_swt_trnsf_in_utl~get_current_pve_period( iv_jurisdiction = iv_jurisdiction
                                                                                                                        iv_division = ls_header-division  ).
      "Determine start date Excluding PVE period
      IF lv_start_date < lv_pve_period AND iv_date >= lv_pve_period AND lv_start_date IS NOT INITIAL.
        lv_start_date = lv_pve_period.
        DATA(lv_pve_scenario) = abap_true. "Indicator for PVE period being resoponsible for truncating start date
      ENDIF.

*Remote Read Type
      IF lt_pfc_result[ 1 ]-remotely_read = abap_true.
        rs_pfc_valid_result-meter_type = 'R'."Remote Type
        IF lv_start_date IS INITIAL.
          populate_error abap_true lc_msgid lc_msgno_002.
        ELSE.

          rs_pfc_valid_result-remote_date_range = VALUE #(   sign = 'I'
                                                             option = 'BT'
                                                             low = lv_start_date
                                                             high = lv_end_date  ).

          IF lv_start_date = lv_end_date.
            "Dont allow retro consent option
            CLEAR: lv_start_date, lv_end_date ,  rs_pfc_valid_result-remote_date_range .
          ENDIF.
        ENDIF.
*Manual Read Type
      ELSEIF lt_pfc_result[ 1 ]-manually_read = abap_true.
        rs_pfc_valid_result-meter_type = 'M'."Manual Type

*     Get Previous Read Date & Read Quality from IDOC
        LOOP AT it_idoc_data ASSIGNING FIELD-SYMBOL(<fs_idoc_data>).
          IF <fs_idoc_data>-segnam = 'ZPREVIOUSREADDT'.
            DATA(ls_zprevious) = CONV zpreviousreaddt( <fs_idoc_data>-sdata ).
            lt_prev_read_dt_qlty = VALUE #( BASE lt_prev_read_dt_qlty ( read_date = ls_zprevious-read_date
                                                                        read_quality = ls_zprevious-read_quality ) ).
          ENDIF.
          CLEAR: ls_zprevious.
        ENDLOOP.


        IF lt_prev_read_dt_qlty IS INITIAL OR
           lv_start_date IS INITIAL.
          populate_error abap_true lc_msgid lc_msgno_002.
        ELSE.
          LOOP AT lt_prev_read_dt_qlty ASSIGNING FIELD-SYMBOL(<fs_prev_read_dt_qlty>).
            IF <fs_prev_read_dt_qlty>-read_date >= lv_start_date AND
               <fs_prev_read_dt_qlty>-read_date <= lv_end_date AND
               line_exists( it_read_quality_dt[ read_quality = <fs_prev_read_dt_qlty>-read_quality ] ).

              rs_pfc_valid_result-manual_date_list = VALUE #( BASE  rs_pfc_valid_result-manual_date_list (   sign = 'I'
                                                                                                             option = 'EQ'
                                                                                                             low = <fs_prev_read_dt_qlty>-read_date
                                                                                                             high = space  ) ).
            ENDIF.
          ENDLOOP.
          IF  rs_pfc_valid_result-manual_date_list IS INITIAL.
            IF lv_pve_scenario = abap_true.
              populate_error lc_pve_exception_flag lc_msgid lc_msgno_005.
            ELSE.
              populate_error abap_true lc_msgid lc_msgno_002.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_NMI_CLASS_FRM_SEGMENT.

    CONSTANTS: lc_seg_zmasterdata_elec TYPE edilsegtyp VALUE 'ZMASTERDATA_ELEC'.

    DATA: lt_idoc_data TYPE STANDARD TABLE OF edidd.

    IF iv_idoc_data IS NOT INITIAL .
*         Read the IDOC data for the SPID type 2 response
      IF sy-subrc EQ 0.
        DATA(ls_zmasterdata_elec) = VALUE zmasterdata_elec( iv_idoc_data[ segnam = lc_seg_zmasterdata_elec ]-sdata OPTIONAL ).
        rv_nmi_class = ls_zmasterdata_elec-nmiclassificationcode.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_SPID_RESPONSE.


    CONSTANTS: lc_ty2res_ele TYPE e_dexproc VALUE 'ZNMIRES03E'.

    DATA: lt_iedextask_data TYPE iedextask_db_data,
          lt_edextask       TYPE TABLE OF edextask.


    DATA(lt_sel_int_ui) = VALUE isu00_range_tab( ( sign = 'I' option = 'EQ' low = iv_int_pod high = space ) ).

    DATA(lt_sel_dexproc) = VALUE isu00_range_tab( ( sign = 'I' option = 'EQ' low = lc_ty2res_ele high = space ) ). "Type 2 resp elec

* Get the status
    CALL METHOD cl_isu_datex_process=>get_dexstatus_all
      IMPORTING
        yt_dexstatus = DATA(lt_datexstatus)
      EXCEPTIONS
        OTHERS       = 0.

    DATA(lt_sel_dexstatus) = VALUE isu00_range_tab( FOR ls_status IN lt_datexstatus ( sign = 'I' option = 'EQ' low = ls_status high = space ) ).

    IF iv_date_from IS INITIAL.
      DATA(lv_bus_days) = CONV i( |-{ iv_spid_bd }| ).
      DATA(lv_calendar) = NEW zcl_idm_ls_gen_process( )->zif_idm_ls_gen_process~get_calendar( x_jurisdiction = iv_jurisdiction ).
      DATA(lv_bd_date) = NEW zcl_iid_crr_process( )->zif_iid_crr_process~add_working_day( EXPORTING iv_calender = lv_calendar
                                                                                                       iv_date  = iv_date_to
                                                                                                       iv_days  = CONV #( lv_bus_days ) ).
      lv_bd_date = COND #( WHEN lv_bd_date IS INITIAL THEN iv_date_to
                           ELSE lv_bd_date ).
    ELSE.
      lv_bd_date = iv_date_from.
    ENDIF.

    DATA(lt_sel_dexduedate) = VALUE isu00_range_tab( ( sign = 'I' option = 'BT' low = lv_bd_date high = iv_date_to ) ).

* Select datex data
    CALL METHOD cl_isu_datex_task=>select_task_for_pod
      EXPORTING
        x_max_records         = 0
        xt_sel_int_ui         = lt_sel_int_ui
        xt_sel_dexproc        = lt_sel_dexproc
*       xt_sel_dexservprov    = xt_sel_dexservprov
*       xt_sel_dexservprovself = xt_sel_dexservprovself
        xt_sel_dexstatus      = lt_sel_dexstatus
        xt_sel_dexduedate     = lt_sel_dexduedate
*       xt_sel_dexduetime     =
        x_no_corrected        = 'X'
        x_read_edextaskidoc   = 'X'
        x_read_edextaskref    = space
        x_read_edextaskerror  = 'X'
        x_read_edextaskwkflow = space
        x_check_authorization = space
      IMPORTING
        yt_edextask_data      = lt_iedextask_data
      EXCEPTIONS
        not_found             = 1
        too_many_selections   = 2
        OTHERS                = 3.
    IF sy-subrc = 0.
      lt_edextask = VALUE #( FOR ls_iedextadk_data IN lt_iedextask_data ( ls_iedextadk_data-edextask ) ).
      "Get the latest TaskID
      SORT lt_edextask BY dexduedate DESCENDING dexduetime DESCENDING.
      DATA(lv_edextask_id) = VALUE edextask( lt_edextask[ 1 ]-dextaskid OPTIONAL ).
      IF lv_edextask_id IS NOT INITIAL.
        SELECT SINGLE docnum INTO rv_idoc_no FROM edextaskidoc WHERE dextaskid = lv_edextask_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_ZMETERNMI_SEG_DETAILS.

    CONSTANTS: lc_zmeternmi_seg TYPE edilsegtyp VALUE 'ZMETERNMI'.

    LOOP AT iv_idoc_data ASSIGNING FIELD-SYMBOL(<fs_idoc_data>).
      IF <fs_idoc_data>-segnam = lc_zmeternmi_seg.
        DATA(ls_meternmi_seg) = CONV zmeternmi( <fs_idoc_data>-sdata ).
        IF it_meter_status IS NOT INITIAL.
          IF line_exists( it_meter_status[ status = ls_meternmi_seg-status ] ) .
            rt_meternmi = VALUE #( BASE rt_meternmi ( ls_meternmi_seg ) ).
          ENDIF.
        ELSE.
          rt_meternmi = VALUE #( BASE rt_meternmi ( ls_meternmi_seg ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD MATCH_PATTERNS.

    rv_flag = abap_false.

    IF  iv_patterns IS NOT INITIAL.
      IF iv_patterns CS '*'.
        SPLIT iv_patterns AT '*' INTO DATA(lv_pattern) DATA(lv_ignore).
        DATA(lv_regex_pattern) = |\\b{ lv_pattern }\\w*\\b|.
      ELSE.
        lv_regex_pattern = iv_patterns.
      ENDIF.

      FIND REGEX lv_regex_pattern IN iv_string IGNORING CASE.
      IF sy-subrc = 0.
        rv_flag = abap_true.
      ENDIF.
    ELSEIF iv_patterns IS INITIAL AND
           iv_string IS INITIAL.
      rv_flag = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~FETCH_BUSINESS_DAYS_TRNSF_IN.

    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEB91B870F684D72B5A'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt,
         lv_identifier TYPE if_fdt_types=>element_text.

    FIELD-SYMBOLS <lv_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.

    ls_name_value-name = 'IDENTIFIER'.
    lv_identifier = iv_identifier.
    GET REFERENCE OF lv_identifier INTO lo_data.
    ls_name_value-value = lo_data.
    INSERT ls_name_value INTO TABLE lt_name_value.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <lv_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rv_bd_days
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS NOT INITIAL.
          CLEAR rv_bd_days.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~FETCH_BUSINESS_DYS_FOR_CR_CODE.

    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEB94B0871E46E80B66'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt,
         lv_cr_code    TYPE if_fdt_types=>element_text.

    FIELD-SYMBOLS <lv_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.

    CLEAR rv_bd_dys.

    ls_name_value-name = 'CR_CODE'.
    lv_cr_code = iv_cr_code.
    GET REFERENCE OF lv_cr_code INTO lo_data.
    ls_name_value-value = lo_data.
    INSERT ls_name_value INTO TABLE lt_name_value.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <lv_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rv_bd_dys
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        IF lo_fdt IS NOT INITIAL.
          CLEAR rv_bd_dys.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~FETCH_RETRO_CON_FRM_CRM.

    DATA: lv_rfcdest       TYPE rfcdest,
          lv_retro_consent TYPE z_consent.

    CLEAR: lv_rfcdest ,
           lv_retro_consent.

    IF iv_moveindate IS NOT INITIAL
      AND iv_pod IS NOT INITIAL.

      "GET the rfc DESTINATION
      CALL FUNCTION 'ISM_CRM_RFC_DESTINATION_GET'
        IMPORTING
          pv_destination           = lv_rfcdest
        EXCEPTIONS
          no_destination_available = 1
          OTHERS                   = 2.

      IF sy-subrc EQ 0.

        CALL FUNCTION 'Z_CSV_GET_RETRO_CONSENT_FLAG' DESTINATION lv_rfcdest
          EXPORTING
            iv_int_ui             = iv_pod
            iv_date               = iv_moveindate
          IMPORTING
            ev_consent_flag       = lv_retro_consent
          EXCEPTIONS
            system_failure        = 1
            communication_failure = 2
            OTHERS                = 3.
        IF sy-subrc EQ 0.
          "
          CLEAR: ev_com_error.
          ev_retro_con = lv_retro_consent.
        ELSE.
          CLEAR: ev_retro_con.
          ev_com_error = abap_true.
        ENDIF.
      ELSE.
        CLEAR: ev_retro_con.
        ev_com_error = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_METER_STATUS_BRF.

    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEBA9E633666A12EBB9'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt.
    FIELD-SYMBOLS <la_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <la_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rt_meter_status
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        CLEAR: rt_meter_status.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_NMI_CLASS.

    CLEAR: rv_nmi_class.

    IF iv_int_pod IS NOT INITIAL.
      SELECT SINGLE ui_cls_code FROM euihead
                                INTO rv_nmi_class
                                WHERE int_ui = iv_int_pod.
      IF sy-subrc <> 0.
        CLEAR: rv_nmi_class.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_PFC_DECISION_TABLE_BRF.

    CLEAR: rt_pfc_dt.

    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEBAFF107648190CBD6'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lr_data       TYPE REF TO data,
         lx_fdt        TYPE REF TO cx_fdt.
    FIELD-SYMBOLS <la_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lr_data ).
    ASSIGN lr_data->* TO <la_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rt_pfc_dt
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lx_fdt.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_PVE_PERIOD.

    "Get PVE Period
    IF iv_jurisdiction IS NOT INITIAL.
      SELECT * FROM ziidtt_pve
        INTO TABLE rt_pve_period
        WHERE jurisdiction = iv_jurisdiction AND
              fuel = iv_division.

      SORT rt_pve_period BY effective_date ASCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_RETRO_CONSENT.

    CLEAR: rv_retro_consent.

    IF iv_switch_no IS NOT INITIAL.

      SELECT SINGLE zconsent FROM eideswtdoc
                             INTO rv_retro_consent
                             WHERE switchnum = iv_switch_no.

      IF sy-subrc <> 0.
        CLEAR: rv_retro_consent.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~GET_SPID_SEARCH_DAYS.

    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000D3AD1916A1EEBA9E61A5B3D97ABB9'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lo_data       TYPE REF TO data,
         lo_fdt        TYPE REF TO cx_fdt.
    FIELD-SYMBOLS <la_any> TYPE any.
    GET TIME STAMP FIELD lv_timestamp.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lo_data ).
    ASSIGN lo_data->* TO <la_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = rv_spid_bd
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lo_fdt.
        CLEAR: rv_spid_bd.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_DAO~UPDATE_RETRO_CON_SWCDOC.

    CONSTANTS: lc_consent TYPE char8 VALUE 'ZCONSENT'.

    IF iv_swdocnum IS NOT INITIAL
      AND iv_retro_con IS NOT INITIAL.

      CALL METHOD cl_isu_switchdoc=>s_set_property
        EXPORTING
          x_switchnum     = iv_swdocnum
          x_property      = lc_consent      "'ZCONSENT'
          x_value         = iv_retro_con
        EXCEPTIONS
          not_found       = 1
          parameter_error = 2
          general_fault   = 3
          not_authorized  = 4
          OTHERS          = 5.
      IF sy-subrc EQ 0.
        ev_swc_up_success = abap_true.
      ELSE.
        CLEAR: ev_swc_up_success.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
