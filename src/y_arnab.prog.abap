*&---------------------------------------------------------------------*
*& Report  Y_ARNAB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT y_arnab.

*Define structure
TYPES:
  BEGIN OF ty_struct1,
    field1 TYPE i,
    field2 TYPE string,
  END OF ty_struct1,
  BEGIN OF ty_struct2,
    field1 TYPE i,
    field2 TYPE string,
    field3 TYPE i,
  END OF ty_struct2.

*Define table types
TYPES: gtt_struct1 TYPE STANDARD TABLE OF ty_struct1 WITH DEFAULT KEY,
       gtt_struct2 TYPE STANDARD TABLE OF ty_struct2 WITH DEFAULT KEY.

* Initialize source table with some random values
DATA(lt_source) = VALUE gtt_struct1(
    ( field1 = 1 field2 = 'A' )
    ( field1 = 2 field2 = 'B' ) ).

*Use like simple MOVE CORRESPONDING
DATA(lt_target1) = VALUE gtt_struct2( FOR lwa_source IN lt_source ( CORRESPONDING #( lwa_source ) ) ).
cl_demo_output=>display( lt_target1 ).

DATA(lt_target1_new) = CORRESPONDING gtt_struct2( lt_source ).
cl_demo_output=>display( lt_target1_new ).

*Populate sy-tabix in the additional fields within the for loop
DATA(lt_target2) = VALUE gtt_struct2( FOR lwa_source IN lt_source
                            INDEX INTO index
                            LET base = VALUE ty_struct2( field3 = index )
                            IN ( CORRESPONDING #( BASE ( base ) lwa_source ) ) ).

cl_demo_output=>display( lt_target2 ).

*Populate sy-tabix  in the additional field(field3) and constant in field1 within the for loop
DATA(lt_target2_new) = VALUE gtt_struct2( FOR lwa_source IN lt_source
                            INDEX INTO index
                            LET base = VALUE ty_struct2( field1 = 9 field3 = index  )
                            IN ( CORRESPONDING #( BASE ( base ) lwa_source EXCEPT field1 ) ) ).

cl_demo_output=>display( lt_target2_new ).

*Populate any value or call custom method  in the additional fields within the for loop
DATA(lt_target3) = VALUE gtt_struct2( FOR lwa_source IN lt_source
             LET base = VALUE ty_struct2( field3 = 10 ) "<<< Custom method/any value
             IN ( CORRESPONDING #( BASE ( base ) lwa_source ) ) ).
cl_demo_output=>display( lt_target3 ).
