*&---------------------------------------------------------------------*
*& Report  zcds_report
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zcds_report.


**********************CDS Join************************************************
SELECT * FROM zcds_join INTO TABLE @DATA(lt_join) WHERE business_partner = '0100000123'.
IF sy-subrc = 0.
  cl_demo_output=>display( lt_join ).
ENDIF.
**********************CDS Join************************************************

**********************CDS Association************************************************
SELECT
business_partner
FROM zcds_association
INTO TABLE @DATA(lt_association).
IF sy-subrc = 0.
  cl_demo_output=>display( lt_association ).
ENDIF.
**********************CDS Association************************************************

**********************CDS Parameters************************************************
select * from zcds_parameter(  p_partner1 = '0100000142', p_partner2 = '0100000123' )
into table @data(lt_parameters).
IF sy-subrc = 0.
  cl_demo_output=>display( lt_parameters ).
ENDIF.
**********************CDS Parameters*************************************************
