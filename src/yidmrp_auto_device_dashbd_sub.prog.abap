*&---------------------------------------------------------------------*
*&  Include           ZIDMRP_AUTO_DEVICE_DASHBD_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA c_cursor TYPE cursor.

  DATA: ls_idoc_cal          TYPE ty_idoc_cal,
        lt_idoc_rec_basic    TYPE TABLE OF ty_idoc_rec,
        lt_idoc_rec_gas      TYPE TABLE OF ty_idoc_rec,
        lt_idoc_rec_interval TYPE TABLE OF ty_idoc_rec,
        lv_count_total       TYPE i,
        lv_count_success     TYPE i,
        lv_zero1(1)          TYPE c,
        lv_zero2(1)          TYPE c,
        lv_index(20)          TYPE c.

  CONSTANTS: lc_val_61  TYPE p VALUE '0.61',
             lc_val_68  TYPE p VALUE '0.68',
             lc_val_114 TYPE p VALUE '1.14',
             lc_val_143 TYPE p VALUE '1.43',
             lc_val_7   TYPE p VALUE '7',
             lc_val_8   TYPE p VALUE '8'.

  OPEN CURSOR c_cursor FOR

  SELECT idoctp status credat FROM edidc
                                WHERE status IN ('53','56')
                                AND credat IN s_creat
                                AND idoctp IN (tc_constants-gas, tc_constants-basic,tc_constants-interval).

  "Move the data from the Cursor into the target area.
  FETCH NEXT CURSOR c_cursor INTO
  TABLE gt_idoc_rec.

  "Close the Cursor to complete the operation.
  CLOSE CURSOR c_cursor.

  SORT gt_idoc_rec BY basic_type.

  lt_idoc_rec_basic = lt_idoc_rec_gas = lt_idoc_rec_interval = gt_idoc_rec.

* For basic type: BASIC
  DELETE lt_idoc_rec_basic WHERE basic_type <> tc_constants-basic.
  IF lt_idoc_rec_basic IS NOT INITIAL.
    ls_idoc_cal-meter_type = text-003.
*  Get total count of 'BASIC'
    DESCRIBE TABLE lt_idoc_rec_basic LINES lv_count_total.

* Get success & fail Count of 'BASIC'
    DELETE lt_idoc_rec_basic WHERE status <> tc_constants-success.
    DESCRIBE TABLE lt_idoc_rec_basic LINES lv_count_success.
    ls_idoc_cal-success_idoc = lv_count_success.
    ls_idoc_cal-failure_idoc = lv_count_total - lv_count_success.
    IF ls_idoc_cal-success_idoc IS INITIAL.
      lv_zero1 = '0'.
    ENDIF.
    IF ls_idoc_cal-failure_idoc IS INITIAL.
      lv_zero2 = '0'.
    ENDIF.
    SHIFT ls_idoc_cal-success_idoc LEFT DELETING LEADING '0'.
    SHIFT ls_idoc_cal-failure_idoc LEFT DELETING LEADING '0'.
* calculate index for BASIC
    IF lv_zero1 <> '0'.
      ls_idoc_cal-index = ( ( ls_idoc_cal-failure_idoc ) * 100 ) / ( 252 * ( ls_idoc_cal-success_idoc ) ).
      WRITE ls_idoc_cal-index TO lv_index.
    ENDIF.
    CONCATENATE '(' lv_zero2 ls_idoc_cal-failure_idoc '*' '100' ')' '/' '(' '252' '*' lv_zero1 ls_idoc_cal-success_idoc ')' '=' lv_index
    INTO ls_idoc_cal-index_all SEPARATED BY space.
* Determine Status as per index value
    IF ls_idoc_cal-index < lc_val_61.
      ls_idoc_cal-status = 'G'.
    ELSEIF ls_idoc_cal-index >= lc_val_61 AND ls_idoc_cal-index <= lc_val_68.
      ls_idoc_cal-status = 'O'.
    ELSEIF ls_idoc_cal-index > lc_val_68.
      ls_idoc_cal-status = 'R'.
    ENDIF.
    APPEND ls_idoc_cal TO gt_idoc_cal.
  ENDIF.

  CLEAR: lv_count_success,lv_count_total,ls_idoc_cal,lv_zero1,lv_zero2,lv_index.

* For basic type: GAS
  DELETE lt_idoc_rec_gas WHERE basic_type <> tc_constants-gas.
  IF lt_idoc_rec_gas IS NOT INITIAL.
    ls_idoc_cal-meter_type = text-004.
*  Get total count of 'GAS'
    DESCRIBE TABLE lt_idoc_rec_gas LINES lv_count_total.

* Get success & fail Count of 'GAS'
    DELETE lt_idoc_rec_gas WHERE status <> tc_constants-success.
    DESCRIBE TABLE lt_idoc_rec_gas LINES lv_count_success.
    ls_idoc_cal-success_idoc = lv_count_success.
    ls_idoc_cal-failure_idoc = lv_count_total - lv_count_success.
    IF ls_idoc_cal-success_idoc IS INITIAL.
      lv_zero1 = '0'.
    ENDIF.
    IF ls_idoc_cal-failure_idoc IS INITIAL.
      lv_zero2 = '0'.
    ENDIF.
    SHIFT ls_idoc_cal-success_idoc LEFT DELETING LEADING '0'.
    SHIFT ls_idoc_cal-failure_idoc LEFT DELETING LEADING '0'.
* calculate index for GAS
    IF lv_zero1 <> '0'.
      ls_idoc_cal-index = ( ( ls_idoc_cal-failure_idoc ) * 100 ) / ( 252 * ( ls_idoc_cal-success_idoc ) ).
      WRITE ls_idoc_cal-index TO lv_index.
    ENDIF.
    WRITE ls_idoc_cal-index TO lv_index.
    CONCATENATE '(' lv_zero2 ls_idoc_cal-failure_idoc '*' '100' ')' '/' '(' '252' '*' lv_zero1 ls_idoc_cal-success_idoc ')' '=' lv_index
    INTO ls_idoc_cal-index_all SEPARATED BY space.
* Determine Status as per index value
    IF ls_idoc_cal-index < lc_val_114.
      ls_idoc_cal-status = 'G'.
    ELSEIF ls_idoc_cal-index >= lc_val_114 AND ls_idoc_cal-index <= lc_val_143.
      ls_idoc_cal-status = 'O'.
    ELSEIF ls_idoc_cal-index > lc_val_143.
      ls_idoc_cal-status = 'R'.
    ENDIF.
    APPEND ls_idoc_cal TO gt_idoc_cal.
  ENDIF.

  CLEAR: lv_count_success,lv_count_total,ls_idoc_cal,lv_zero1,lv_zero2, lv_index.

* For basic type: Interval
  DELETE lt_idoc_rec_interval WHERE basic_type <> tc_constants-interval.
  IF lt_idoc_rec_interval IS NOT INITIAL.
    ls_idoc_cal-meter_type = text-002.
*  Get total count of 'Interval'
    DESCRIBE TABLE lt_idoc_rec_interval LINES lv_count_total.

* Get success & fail Count of 'Interval'
    DELETE lt_idoc_rec_interval WHERE status <> tc_constants-success.
    DESCRIBE TABLE lt_idoc_rec_interval LINES lv_count_success.
    ls_idoc_cal-success_idoc = lv_count_success.
    ls_idoc_cal-failure_idoc = lv_count_total - lv_count_success.
    IF ls_idoc_cal-success_idoc IS INITIAL.
      lv_zero1 = '0'.
    ENDIF.
    IF ls_idoc_cal-failure_idoc IS INITIAL.
      lv_zero2 = '0'.
    ENDIF.
    SHIFT ls_idoc_cal-success_idoc LEFT DELETING LEADING '0'.
    SHIFT ls_idoc_cal-failure_idoc LEFT DELETING LEADING '0'.
* calculate index for Interval
    IF lv_zero1 <> '0'.
      ls_idoc_cal-index = ( ( ls_idoc_cal-failure_idoc ) * 100 ) / ( ls_idoc_cal-success_idoc ).
      WRITE ls_idoc_cal-index TO lv_index.
    ENDIF.
    CONCATENATE '(' lv_zero2 ls_idoc_cal-failure_idoc '*' '100' ')' '/' '(' lv_zero1 ls_idoc_cal-success_idoc ')' '=' lv_index INTO
    ls_idoc_cal-index_all SEPARATED BY space.
* Determine Status as per index value
    IF ls_idoc_cal-index < lc_val_7.
      ls_idoc_cal-status = 'G'.
    ELSEIF ls_idoc_cal-index >= lc_val_7 AND ls_idoc_cal-index <= lc_val_8.
      ls_idoc_cal-status = 'O'.
    ELSEIF ls_idoc_cal-index > lc_val_8.
      ls_idoc_cal-status = 'R'.
    ENDIF.
    APPEND ls_idoc_cal TO gt_idoc_cal.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_RECIPIENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_recipients .

* Get Email Recipients from BRF+
  CALL FUNCTION 'Z_IDM_GET_EMAIL_RECIPIENT'
    IMPORTING
      et_email_id = gt_recipients.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.


  DATA:

    lv_sent_to_all   TYPE os_boolean,
    lv_email         TYPE ad_smtpadr,
    lt_body          TYPE soli_tab,
    lr_send_request  TYPE REF TO cl_bcs,
    lr_bcs_exception TYPE REF TO cx_bcs,
    lr_static_check  TYPE REF TO cx_static_check,
    lr_recipient     TYPE REF TO if_recipient_bcs,
    lr_sender        TYPE REF TO cl_sapuser_bcs,
    lr_document      TYPE REF TO cl_document_bcs,
    lv_date          TYPE so_obj_des,
    lv_subject       TYPE so_obj_des,
    lo_mime_helper   TYPE REF TO cl_gbt_multirelated_service.

  IF gt_idoc_cal IS NOT INITIAL.
    CREATE OBJECT lo_mime_helper.
    PERFORM get_image_from_mime CHANGING lo_mime_helper .

* Fill the email body
    PERFORM fill_email_body CHANGING lt_body.

*Create HTML form with HTML content.
    CALL METHOD lo_mime_helper->set_main_html
      EXPORTING
        content = lt_body.


    TRY.
        "Create send request
        lr_send_request = cl_bcs=>create_persistent( ).

        "Email FROM...
        lr_sender = cl_sapuser_bcs=>create( sy-uname ).

        "Add sender to send request
        CALL METHOD lr_send_request->set_sender
          EXPORTING
            i_sender = lr_sender.

        LOOP AT gt_recipients ASSIGNING FIELD-SYMBOL(<fs_recipients>).
          "Email TO...
          lv_email = <fs_recipients>-email_id.
          lr_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

          "Add recipient to send request
          CALL METHOD lr_send_request->add_recipient
            EXPORTING
              i_recipient = lr_recipient
              i_express   = abap_true.
        ENDLOOP.

* Fill the Subject Line
        WRITE s_creat-low TO lv_date MM/DD/YYYY.
        CONCATENATE 'Meter Read Dash Board for'(006) ' ' lv_date INTO lv_subject.


*Create HTML form with HTML content
        lr_document =  cl_document_bcs=>create_from_multirelated(
            i_subject          = lv_subject
            i_multirel_service = lo_mime_helper ).

        "Add document to send request
        CALL METHOD lr_send_request->set_document( lr_document ).

        "Trigger E-Mail immediately
*      lr_send_request->set_send_immediately( abap_true ).

        "Send email
        CALL METHOD lr_send_request->send(
          EXPORTING
            i_with_error_screen = abap_true
          RECEIVING
            result              = lv_sent_to_all ).
        IF lv_sent_to_all = abap_true.
          COMMIT WORK.
        ENDIF.
        "Exception handling
      CATCH cx_bcs INTO lr_bcs_exception.
        IF lr_bcs_exception IS NOT INITIAL.
          WRITE: 'Error in Sending Mail'(036).
        ENDIF.
      CATCH cx_static_check INTO lr_static_check.
        IF lr_static_check IS NOT INITIAL.
          WRITE: 'Error in Sending Mail'(036).
        ENDIF.

    ENDTRY.
  ELSE.
    WRITE: 'No Entries Found'(037).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_EMAIL_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_BODY  text
*----------------------------------------------------------------------*
FORM fill_email_body  CHANGING p_lt_body TYPE soli_tab.

  CONSTANTS: lc_zero(1) TYPE c VALUE '0'.

  DATA: lv_date           TYPE so_obj_des,
        lv_time           TYPE so_obj_des,
        lv_head           TYPE so_text255,
        lv_colour         TYPE so_text255,
        lv_col1           TYPE so_text255,
        lv_col2           TYPE so_text255,
        lv_col3           TYPE so_text255,
        lv_col4           TYPE so_text255,
        lv_col5           TYPE so_text255,
        lv_col            TYPE so_text255,
*        lv_index(6) TYPE c,
        lv_index_all(120) TYPE c.

  "Fill body header
  WRITE sy-datum TO lv_date MM/DD/YYYY.
  WRITE sy-uzeit TO lv_time USING EDIT MASK '__:__:__'.
  CONCATENATE '<h2>'  'Time of Generation'(007) lv_date  lv_time '</h2>' INTO lv_head SEPARATED BY space.

  APPEND '<html>'(008) TO p_lt_body.
  APPEND '<head>'(009) TO p_lt_body.
  APPEND '<style>'(010) TO p_lt_body.
  APPEND 'table, th, td { border: 1px solid black; border-collapse: collapse;}'(011) TO p_lt_body.
  APPEND '</style>'(012) TO p_lt_body.
  APPEND '</head>'(013) TO p_lt_body.
  APPEND '<body>'(014) TO p_lt_body.
  APPEND  lv_head TO p_lt_body.

  APPEND '<table style="width:100%"><tr style= "color:white; background-color:rgb(69,115,200);">'(015) TO p_lt_body.
  APPEND '<th>Type of Meter Data</th><th>Success IDOC</th><th>Error IDOC</th>'(016) TO p_lt_body.
  APPEND '<th>Measured Index</th><th>Operational Status</th></tr>'(017) TO p_lt_body.

  LOOP AT gt_idoc_cal ASSIGNING FIELD-SYMBOL(<fs_idoc_cal>).
    IF sy-tabix MOD 3 = 1.
      lv_colour = '<tr style= "color:black; background-color:rgb(207,213,235);">'(018).
    ELSEIF sy-tabix MOD 3 = 2.
      lv_colour = '<tr style= "color:black; background-color:rgb(233,236,247);">'(019).
    ELSEIF sy-tabix MOD 3 = 0.
      lv_colour = '<tr style= "color:black; background-color:rgb(207,213,235);">'(020).
    ENDIF.

    APPEND lv_colour TO p_lt_body.

    CONCATENATE '<td>' <fs_idoc_cal>-meter_type '</td>' INTO lv_col1.

    IF <fs_idoc_cal>-success_idoc <> ''.
      CONCATENATE '<td>' <fs_idoc_cal>-success_idoc '</td>' INTO lv_col2.
    ELSE.
      CONCATENATE '<td>' lc_zero '</td>' INTO lv_col2.
    ENDIF.
    IF <fs_idoc_cal>-failure_idoc <> ''.
      CONCATENATE '<td>' <fs_idoc_cal>-failure_idoc '</td>' INTO lv_col3.
    ELSE.
      CONCATENATE '<td>' lc_zero '</td>' INTO lv_col3.
    ENDIF.

*    WRITE <fs_idoc_cal>-index TO lv_index.
    WRITE <fs_idoc_cal>-index_all TO lv_index_all.
    CONCATENATE '<td>' lv_index_all '</td>' INTO lv_col4.

    IF <fs_idoc_cal>-success_idoc = ''.
      lv_col5 =  '<td><div style="text-align:center"><img alt="[image]" src="cid:black.png" height="50" width="110" /></div></td>'(040).
    ELSE.
      IF <fs_idoc_cal>-status = 'R'.
        lv_col5 =  '<td><div style="text-align:center"><img alt="[image]" src="cid:red.png" height="50" width="110" /></div></td>'(021).
      ELSEIF <fs_idoc_cal>-status = 'O'.
        lv_col5 =  '<td><div style="text-align:center"><img alt="[image]" src="cid:orange.png" height="50" width="110" /></div></td>'(022).
      ELSEIF <fs_idoc_cal>-status = 'G'.
        lv_col5 =  '<td><div style="text-align:center"><img alt="[image]" src="cid:green.png" height="50" width="110" /></div></td>'(023).
      ENDIF.
    ENDIF.

    APPEND lv_col1 TO p_lt_body.
    APPEND lv_col2 TO p_lt_body.
    APPEND lv_col3 TO p_lt_body.
    APPEND lv_col4 TO p_lt_body.
    APPEND lv_col5 TO p_lt_body.
    APPEND '</tr>'(024) TO p_lt_body.
  ENDLOOP.

  APPEND '</table>'(025) TO p_lt_body.
  APPEND '</body>'(026) TO p_lt_body.
  APPEND '</html>'(027) TO p_lt_body.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_IMAGE_FROM_MIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LO_MIME_HELPER  text
*----------------------------------------------------------------------*
FORM get_image_from_mime  CHANGING p_lo_mime_helper TYPE REF TO cl_gbt_multirelated_service.

  TYPES: BEGIN OF ty_image,
           url(50)       TYPE c,
           content       TYPE xstring,
           filename      TYPE mime_text,
           contend_id    TYPE mime_cntid,
           object_length TYPE so_obj_len,
         END OF ty_image.

  DATA(lv_mr_api) = cl_mime_repository_api=>if_mr_api~get_api( ).


  DATA:
    lv_content TYPE xstring,
    lv_obj_len TYPE so_obj_len,
    lt_solix   TYPE solix_tab,
    ls_image   TYPE ty_image,
    lt_image   TYPE TABLE OF ty_image.


* Red Image
  ls_image-url = '/SAP/PUBLIC/traffic_light_RED.png'(028).
  ls_image-contend_id = 'red.png'(031).
  ls_image-filename = 'red.png'(031).
  APPEND ls_image TO lt_image.


*Orange Image
  ls_image-url = '/SAP/PUBLIC/traffic_light_ORANGE.png'(029).
  ls_image-contend_id = 'orange.png'(032).
  ls_image-filename = 'orange.png'(032).
  APPEND ls_image TO lt_image.

*Green Image
  ls_image-url = '/SAP/PUBLIC/traffic_light_GREEN.png'(030).
  ls_image-contend_id = 'green.png'(033).
  ls_image-filename = 'green.png'(033).
  APPEND ls_image TO lt_image.

*Black Image
  ls_image-url = '/SAP/PUBLIC/traffic_light_BLACK.png'(038).
  ls_image-contend_id = 'black.png'(039).
  ls_image-filename = 'black.png'(039).
  APPEND ls_image TO lt_image.

  LOOP AT lt_image ASSIGNING FIELD-SYMBOL(<fs_image>).


*Get image from mime repository in form of xstring
    CALL METHOD lv_mr_api->get
      EXPORTING
        i_url              = <fs_image>-url
      IMPORTING
        e_content          = lv_content
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        not_found          = 3
        permission_failure = 4
        OTHERS             = 5.
    IF sy-subrc = 0.
      <fs_image>-content = lv_content.


      <fs_image>-object_length = xstrlen( lv_content ).
      lv_obj_len = xstrlen( lv_content ).

*Convert the image from xstring to table form
      CALL METHOD cl_bcs_convert=>xstring_to_solix
        EXPORTING
          iv_xstring = lv_content
        RECEIVING
          et_solix   = lt_solix.



      CALL METHOD p_lo_mime_helper->add_binary_part
        EXPORTING
          content      = lt_solix
          filename     = <fs_image>-filename
          extension    = 'PNG'
          content_type = 'image/png'
          length       = lv_obj_len
          content_id   = <fs_image>-contend_id.

    ENDIF.
    CLEAR: lt_solix, lv_content, lv_obj_len.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_field .

  IF s_creat IS INITIAL.
    MESSAGE 'No Date Entered.'(035) TYPE 'E'.
  ENDIF.

ENDFORM.
