*&---------------------------------------------------------------------*
*& Report  ZICA_VARIANT_REGENERATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT yica_variant_regenerate NO STANDARD PAGE HEADING.

INCLUDE YICA_VARIANT_REGENERATE_TOP.
*INCLUDE zica_variant_regenerate_top.
INCLUDE YICA_VARIANT_REGENERATE_SEL.
*INCLUDE zica_variant_regenerate_sel.


START-OF-SELECTION.

  PERFORM check_authorization.
  PERFORM start_process.

END-OF-SELECTION.

  PERFORM display_report.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report .

  DATA : lo_salv_table TYPE REF TO cl_salv_table,
         lr_functions  TYPE  REF TO cl_salv_functions.


  IF go_var_regen IS BOUND AND
    go_var_regen->gt_output[] IS NOT INITIAL.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = go_var_regen->gt_output.
      CATCH cx_salv_msg .
        RETURN.
    ENDTRY.
    lr_functions = lo_salv_table->get_functions( ).
    lr_functions->set_all( 'X' ).

    lo_salv_table->display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DESKTOP_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM get_desktop_path  CHANGING p_path.


  DATA:
    lt_file_table TYPE filetable,
    ls_file_table TYPE file_table,
    lv_rc         TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF lv_rc GT 0.
      READ TABLE lt_file_table INTO ls_file_table INDEX 1.
      IF sy-subrc = 0.
        p_path = ls_file_table-filename.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN.
    IF screen-group1 EQ 'M1' AND p_batch EQ abap_true.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  START_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_process .

  go_var_regen = NEW zcl_ica_variant_regeneration( ).

  IF p_batch EQ abap_true.
    go_var_regen->regenerate_variant( ).
  ELSE.
    CALL METHOD go_var_regen->regenerate_variant_adhoc
      EXPORTING
        iv_path     = p_path
        iv_idkey    = p_idkey
      EXCEPTIONS
        input_error = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE text-003 TYPE gc_e.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXPORT_TO_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM export_to_index .

  DATA : lt_input_data TYPE zicadt_obj_vari.

  CONCATENATE sy-repid(8) sy-datum sy-uzeit INTO p_idkey.

  IF p_path IS NOT INITIAL.

    CALL METHOD zcl_pep_common_util=>xlsx_upload
      EXPORTING
        iv_file_name  = p_path
      IMPORTING
        et_data       = lt_input_data
      EXCEPTIONS
        error_occured = 1
        OTHERS        = 2.

* Export the internal table into INDX table
    EXPORT datatab = lt_input_data
    TO DATABASE indx(zx) ID p_idkey.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FUNCTION_KEYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_function_keys .

  gt_functxt-icon_id = icon_import.
  gt_functxt-quickinfo = 'Download'.
  gt_functxt-icon_text = 'Download Sample File'.
  sscrfields-functxt_01 = gt_functxt.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_user_req .

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM download_sample_file.
    WHEN 'SJOB'.
      PERFORM export_to_index.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SAMPLE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_sample_file .

  DATA: lt_sample_file TYPE zicadt_obj_vari.
  DATA:
*     Internal Tables
    lt_field_catalog TYPE lvc_t_fcat,
    lv_file          TYPE string,
    lv_dir           TYPE string,
    lv_def_filename  TYPE string,
    lv_filename      TYPE string,
    lv_path          TYPE string,
    lv_fullpath      TYPE string,
    lv_user_action   TYPE i,

*     Object References
    lo_salv_table    TYPE REF TO cl_salv_table,
    lo_columns       TYPE REF TO cl_salv_columns_table,
    lo_aggreg        TYPE REF TO cl_salv_aggregations.

  FIELD-SYMBOLS:
  <ls_field_catalog> TYPE lvc_s_fcat.

*   Get the column header using ALV functionality
  TRY.
      cl_salv_table=>factory(
      EXPORTING
        list_display = abap_false
      IMPORTING
        r_salv_table = lo_salv_table
      CHANGING
        t_table      = lt_sample_file ).

    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  "get colums & aggregation info to create fieldcat
  lo_columns = lo_salv_table->get_columns( ).
  lo_aggreg = lo_salv_table->get_aggregations( ).
  lt_field_catalog = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                           r_columns      = lo_columns
                                           r_aggregations = lo_aggreg ).

  lv_def_filename = |sampleFile|.
  cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension = 'xlsx'
        default_file_name = lv_def_filename
        file_filter       = cl_gui_frontend_services=>filetype_all
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_user_action
      EXCEPTIONS
        OTHERS            = 1
    ).

  CALL METHOD zcl_pep_common_util=>xlsx_download
    EXPORTING
      iv_file_name     = lv_fullpath
      it_data          = lt_sample_file
      it_field_catalog = lt_field_catalog
    EXCEPTIONS
      error_occured    = 1
      OTHERS           = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_authorization .

  CALL FUNCTION 'AUTHORITY_CHECK'
    EXPORTING
      user                = sy-uname
      object              = gc_auth_object
      field1              = gc_actvt
      value1              = gc_16
      field2              = gc_ztranscode
      value2              = gc_tcode
      field3              = gc_zreport
      value3              = gc_report
    EXCEPTIONS
      user_dont_exist     = 1
      user_is_authorized  = 2
      user_not_authorized = 3
      user_is_locked      = 4
      OTHERS              = 5.
  IF sy-subrc <> 2.
    MESSAGE text-004 TYPE gc_e.
  ENDIF.

ENDFORM.
