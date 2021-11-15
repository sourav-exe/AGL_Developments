class YCL_IID_CUST_SWT_MISC_UTL definition
  public
  final
  create public .

public section.

  interfaces ZIF_IID_CUST_SWT_MISC_UTL .

  data GO_CUST_SWT_MISC_DAO type ref to ZIF_IID_CUST_SWT_MISC_DAO .

  methods CONSTRUCTOR
    importing
      !IO_CUST_SWT_MISC_UNIT_DAO type ref to ZIF_IID_CUST_SWT_MISC_DAO optional .
protected section.
private section.
ENDCLASS.



CLASS YCL_IID_CUST_SWT_MISC_UTL IMPLEMENTATION.


  METHOD CONSTRUCTOR.


    go_cust_swt_misc_dao = COND #( WHEN io_cust_swt_misc_unit_dao IS BOUND THEN  io_cust_swt_misc_unit_dao
                           ELSE CAST #( NEW zcl_iid_cust_swt_misc_dao( ) ) ).

  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_UTL~SEND_STANDARD_LETTER.


    DATA: lt_rspar      TYPE TABLE OF rsparams,
          lv_ltr_status TYPE char1.

    CLEAR: rv_sent.
    IF iv_contract IS INITIAL.
      DATA(lv_contract) = go_cust_swt_misc_dao->get_contract_pod( iv_int_ui = iv_int_ui ).
    ELSE.
      lv_contract = iv_contract.
    ENDIF.

    lt_rspar = VALUE #( ( selname = 'P_PARTNR' kind = 'P' sign = 'I' option = 'EQ' low = iv_business_partner )
                        ( selname = 'P_TEMPID' kind = 'P' sign = 'I' option = 'EQ' low = iv_tempid )
                        ( selname = 'P_VERTRG' kind = 'P' sign = 'I' option = 'EQ' low = lv_contract ) ).

    SET PARAMETER ID 'CR64' FIELD abap_true.
    SUBMIT zicsrp001 WITH SELECTION-TABLE lt_rspar AND RETURN.
    "Import letter status from zicsrp001
    IMPORT gv_ltr_status = lv_ltr_status FROM MEMORY ID 'LTRSTATUS'.
    IF lv_ltr_status <> 'E'.
      rv_sent = abap_true.
    ENDIF.
    FREE MEMORY ID 'LTRSTATUS'.
  ENDMETHOD.


  METHOD ZIF_IID_CUST_SWT_MISC_UTL~SET_SUPPLY_SCENARIO_NEW_TMD.

    CONSTANTS: lc_service_drsp TYPE sercode VALUE 'DRSP'.

    "Get Supply Scenario
    DATA(lt_supply_scenario_all) = go_cust_swt_misc_dao->get_supply_scenario( iv_int_pod = iv_int_pod ).

    SORT lt_supply_scenario_all BY dateto DESCENDING.

    "Check Whether Site is a newly created TMD
    IF lines( lt_supply_scenario_all ) = 1 AND
      ( lt_supply_scenario_all[ 1 ]-scenario = '008' OR
        lt_supply_scenario_all[ 1 ]-scenario = '005' ).

      rv_success = 'Y'."New TMD
      "Check Move-in-date is in Past w.r.t Supply Scenario Start date
      IF iv_move_in_date < lt_supply_scenario_all[ 1 ]-datefrom.

        "Delete Current Supply Scenario and associated non-billable services
        DATA(lv_deletion_successful) = go_cust_swt_misc_dao->delete_non_billable_services( it_services = lt_supply_scenario_all[ 1 ]-iserv ).
        IF lv_deletion_successful = abap_true.
          "Select Supply Services,
          CALL METHOD cl_isu_ide_deregscenario=>select_scenario
            EXPORTING
              im_scenario    = lt_supply_scenario_all[ 1 ]-scenario
            IMPORTING
              ex_data        = DATA(ls_supply_scn_services)
              ex_text        = DATA(lv_supply_scn_text)
            EXCEPTIONS
              not_found      = 1
              not_consistent = 2
              OTHERS         = 3.
          IF sy-subrc = 0.
            "Get service data into service table
            DATA(lt_serviceid) = VALUE iederegservprov( FOR ls_service IN ls_supply_scn_services-iservice
                                                      ( service = ls_service-service
                                                        serviceid = VALUE #( lt_supply_scenario_all[ 1 ]-iserv[ service = ls_service-service ]-serviceid OPTIONAL ) ) ).

            DELETE lt_serviceid WHERE service = lc_service_drsp.
          ENDIF.
          "Create services with start date as one day less than move-in date
          rv_success = go_cust_swt_misc_dao->modify_services( iv_int_pod         = iv_int_pod
                                                              iv_supply_scenario = lt_supply_scenario_all[ 1 ]-scenario
                                                              iv_from_date       = CONV sydatum( iv_move_in_date - 1 )
*                                                             iv_to_date         =
                                                              it_serviceid       =  lt_serviceid ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
