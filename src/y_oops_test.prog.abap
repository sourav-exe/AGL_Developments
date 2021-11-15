*&---------------------------------------------------------------------*
*& Report  Y_OOPS_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT y_oops_test.

CLASS cls1 DEFINITION.
  PUBLIC SECTION.
    DATA: lv_parent TYPE i VALUE 1.
    METHODS: constructor,
      display.
ENDCLASS.

CLASS cls1 IMPLEMENTATION.
  METHOD constructor.
    WRITE: / 'parent constructor'.
  ENDMETHOD.
  METHOD display.
    WRITE: / 'parent',
              lv_parent.
  ENDMETHOD.
ENDCLASS.

CLASS cls2 DEFINITION INHERITING FROM cls1.
  PUBLIC SECTION.
    DATA: lv_child TYPE i VALUE 2.
    METHODS constructor.
    METHODS: display REDEFINITION.

ENDCLASS.

CLASS cls2 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    WRITE: / 'Child constructor'.
  ENDMETHOD.

  METHOD display.
    super->display( ).
    WRITE:/ 'child',
           lv_child.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_test) = NEW cls2( ).
  lo_test->display( ).
