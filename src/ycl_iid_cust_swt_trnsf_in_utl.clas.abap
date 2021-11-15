class YCL_IID_CUST_SWT_TRNSF_IN_UTL definition
  public
  final
  create public .

public section.

  interfaces ZIF_IID_CUST_SWT_TRNSF_IN_UTL .

  methods CONSTRUCTOR
    importing
      !IO_FT_UNIT_DAO type ref to ZIF_IID_FASTER_TRANSFER_DAO optional
      !IO_CUST_SWT_UNIT_DAO type ref to ZIF_IID_CUST_SWT_TRNSF_IN_DAO optional .
protected section.
private section.

  data GO_FT_DAO type ref to ZIF_IID_FASTER_TRANSFER_DAO .
  data GO_CUST_SWT_DAO type ref to ZIF_IID_CUST_SWT_TRNSF_IN_DAO .
ENDCLASS.



CLASS YCL_IID_CUST_SWT_TRNSF_IN_UTL IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    go_ft_dao =  COND #( WHEN io_ft_unit_dao IS BOUND THEN  io_ft_unit_dao
                         ELSE CAST #( NEW zcl_iid_faster_transfer_dao( ) ) ).

    go_cust_swt_dao = COND #( WHEN io_cust_swt_unit_dao IS BOUND THEN  io_cust_swt_unit_dao
                         ELSE CAST #( NEW zcl_iid_cust_swt_trnsf_in_dao( ) ) ).

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_UTL~FETCH_UPDATE_RETRO_CONSENT.

    DATA(lo_trnsf_in_dao) = NEW zcl_iid_cust_swt_trnsf_in_dao( ).

    IF lo_trnsf_in_dao IS BOUND
      AND iv_pod IS NOT INITIAL
      AND iv_moveindate IS NOT INITIAL
      AND iv_sw_doc_num IS NOT INITIAL.

      "Fetching
      lo_trnsf_in_dao->zif_iid_cust_swt_trnsf_in_dao~fetch_retro_con_frm_crm(
        EXPORTING
          iv_pod         = iv_pod
          iv_moveindate  = iv_moveindate
        IMPORTING
          ev_retro_con    = DATA(lv_retro_con)
          ev_com_error    = ev_commu_error
        ).

      IF ev_commu_error IS INITIAL.
        ev_retro_consent = lv_retro_con.
        "Fetching
        lo_trnsf_in_dao->zif_iid_cust_swt_trnsf_in_dao~update_retro_con_swcdoc(
          EXPORTING
            iv_swdocnum   = iv_sw_doc_num
            iv_retro_con  = lv_retro_con
          IMPORTING
            ev_swc_up_success    = ev_sw_up_success
          ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_UTL~GET_CURRENT_PVE_PERIOD.

    CLEAR: rv_pve_date.
    "Get all PVE period for the jurisdiction
    DATA(lt_pve_period) = go_cust_swt_dao->get_pve_period( iv_jurisdiction = iv_jurisdiction
                                                           iv_division = iv_division ).

    DATA(lt_current_pve_period)  = VALUE ziiddt_pve_period( FOR ls_pve IN lt_pve_period WHERE ( effective_date+0(4) = sy-datum+0(4) ) ( ls_pve ) ).
    "Determine the current PVE period
    rv_pve_date = VALUE #( lt_current_pve_period[ 1 ]-effective_date OPTIONAL ).

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_UTL~GET_ELIGIBLE_CODES_MTR_TYPE.

    CHECK go_ft_dao IS BOUND.

    DATA(lt_codes) =  go_ft_dao->get_cr_code_info_brf( ).
    "Determine Eligible CR codes based on Meter type
    IF iv_dereg_status IS NOT INITIAL .
      CASE iv_dereg_status.
          "Remote Type
        WHEN 'E1' OR 'E2' OR 'E3' OR 'E4' OR 'M1' OR 'M2'.
          rt_eligible_codes = VALUE #( FOR <fs_cr_codes> IN lt_codes WHERE ( transfer_type = 'IN' AND
                                                                             require_sn = '' AND
                                                                             remote_read = abap_true )
                                                                             ( <fs_cr_codes>  ) ).
        WHEN 'E5'."MRIM
          "Get Read Type code from SPID Response
          DATA(lv_read_type_code) =  zif_iid_cust_swt_trnsf_in_utl~get_read_type_code( iv_installation   = iv_installation
                                                                                       iv_installation_type_code = 'MRIM' ).
          IF lv_read_type_code CS 'RWD'.
            rt_eligible_codes = VALUE #( FOR <fs_cr_codes> IN lt_codes WHERE ( transfer_type = 'IN' AND
                                                                   require_sn = '' AND
                                                                   remote_read = abap_true )
                                                                   ( <fs_cr_codes>  ) ).
          ELSE.
            rt_eligible_codes = VALUE #( FOR <fs_cr_codes> IN lt_codes WHERE ( transfer_type = 'IN' AND
                                                                   require_sn = '' AND
                                                                   manual_read = abap_true )
                                                                   ( <fs_cr_codes>  ) ).
          ENDIF.

          "Manual Type
        WHEN OTHERS.
          rt_eligible_codes = VALUE #( FOR <fs_cr_codes> IN lt_codes WHERE ( transfer_type = 'IN' AND
                                                                             require_sn = '' AND
                                                                             manual_read = abap_true )
                                                                             ( <fs_cr_codes>  ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_TRNSF_IN_UTL~GET_READ_TYPE_CODE.

    CONSTANTS: lc_zmeternmi_seg TYPE edilsegtyp VALUE 'ZMETERNMI'.

    TRY.
        DATA(lo_installation) = zcl_isu_installation=>select( iv_installation ).
        "Get Internal POD
        DATA(lv_int_pod) = lo_installation->get_pod( )-int_ui.
      CATCH zcx_iinstallation.
        RETURN.
    ENDTRY.
    "Get SPID Idoc
    DATA(lv_idoc_spid) = zcl_iid_cust_swt_trnsf_in_dao=>get_spid_response( iv_int_pod      = lv_int_pod
                                                                           iv_date_to      = sy-datum
                                                                           iv_date_from    = '19000101' ).
    "Read Idoc Data
    DATA(lt_idoc_data) = zcl_iid_cust_swt_trnsf_in_dao=>get_idoc_data( iv_idoc_no = lv_idoc_spid ).

    "Get Applicable Meter Statuses
    DATA(lt_meter_status) = go_cust_swt_dao->get_meter_status_brf( ).

    LOOP AT lt_idoc_data ASSIGNING FIELD-SYMBOL(<fs_idoc_data>).
      IF <fs_idoc_data>-segnam = lc_zmeternmi_seg.
        DATA(ls_meternmi_seg) = CONV zmeternmi( <fs_idoc_data>-sdata ).
        IF lt_meter_status IS NOT INITIAL.
          IF line_exists( lt_meter_status[ status = ls_meternmi_seg-status ] ) AND
            ls_meternmi_seg-installation_type_code = iv_installation_type_code.
            rv_read_type_code = ls_meternmi_seg-readtypecode.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
