*&---------------------------------------------------------------------*
*& Report  Y_ONLINE_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT y_online_test.

********************************************************************
*1.
*DATA: BEGIN OF line1 OCCURS 0,
*        name TYPE char10,
*        id   TYPE i,
*      END OF line1.
*
*DATA: table1 LIKE TABLE OF line1.
*line1-name = 'sourav'.
*line1-id = 1.
*ADD line1 TO table1. --->"TABLE1" may not be converted into a number. number.
*append line1 to table1. ---> Correct way to add entry
*
*LOOP AT table1 INTO line1.
*  WRITE: line1-name, line1-id.
*ENDLOOP.

*result: syntax error.
********************************************************************

********************************************************************
*2
*CLASS c1 DEFINITION .
*
*  PUBLIC SECTION.
*    DATA: lv_pub TYPE char10 VALUE 'pub'.
*  PROTECTED SECTION.
*    DATA:lv_pro TYPE char10 VALUE 'pro'.
*  PRIVATE SECTION.
*    DATA: lv_pri TYPE char10 VALUE 'pri'.
*
*ENDCLASS.
*
*CLASS c1 IMPLEMENTATION.
*ENDCLASS.
*
*START-OF-SELECTION.
*  DATA: obj TYPE REF TO c1.
*
*  CREATE OBJECT: obj.
*  WRITE: /5 obj->lv_pub.
********************************************************************

********************************************************************
**3
**CLASS c2 DEFINITION DEFERRED.  "----This fixes all issues
*CLASS c1 DEFINITION.
*
*  PUBLIC SECTION.
*    DATA: ob2 TYPE REF TO c2.
*ENDCLASS.
*
*CLASS c2 DEFINITION .
*
*  PUBLIC SECTION.
*    DATA: num TYPE i VALUE 5.
*ENDCLASS.
*
*
*START-OF-SELECTION.
*
*  DATA: obj TYPE REF TO c1.
*
*  CREATE OBJECT: obj.
*
*  IF obj->ob2 IS NOT BOUND.
*    "allowed since the component ob2 hasn't been accessed during initialization of obj.
*  ENDIF.
*
*  CREATE OBJECT obj->ob2. "-->not an object reference
*  WRITE: /5 obj->ob2->num. " no
*
**result: syntax error
********************************************************************

********************************************************************
*4.

********************************************************************

********************************************************************

********************************************************************


********************************************************************

********************************************************************
CLASS cl_events DEFINITION DEFERRED.
CLASS cl_main DEFINITION DEFERRED.

DATA lo_event TYPE REF TO cl_events. "declare class
DATA lo_main TYPE REF TO cl_main. "declare class

DATA : wa_mara TYPE mara. "declare work area
PARAMETERS p_matnr TYPE mara-matnr. "Material no input


CLASS cl_main DEFINITION. "class definition

  PUBLIC SECTION.
    EVENTS : no_material. "event
    METHODS : get_material_details
      IMPORTING im_matnr TYPE mara-matnr
      EXPORTING ex_mara  TYPE mara.

ENDCLASS.
CLASS cl_events DEFINITION.
  PUBLIC SECTION.
    METHODS : event_handler FOR EVENT no_material OF cl_main. "event handler method
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT lo_event. "create object
  CREATE OBJECT lo_main. "create object
  SET HANDLER lo_event->event_handler FOR lo_main. "register event handler method for the object
  CALL METHOD lo_main->get_material_details "call method to get material details
    EXPORTING
      im_matnr = p_matnr
    IMPORTING
      ex_mara  = wa_mara.
  WRITE :/ wa_mara-matnr, wa_mara-mtart, wa_mara-meins, wa_mara-matkl.

CLASS cl_main IMPLEMENTATION. "class implementation
  METHOD get_material_details.
    SELECT SINGLE * FROM mara
      INTO ex_mara
    WHERE matnr = im_matnr.
    IF sy-subrc NE 0.
      RAISE EVENT no_material. "trigger the event
    ENDIF.
  ENDMETHOD.
ENDCLASS.
CLASS cl_events IMPLEMENTATION.

  METHOD event_handler.
    WRITE :/ 'No material found'. "event handler method implementation
  ENDMETHOD.
ENDCLASS.
"WRITE: 'end'.
