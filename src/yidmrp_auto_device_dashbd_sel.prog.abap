*&---------------------------------------------------------------------*
*&  Include           ZIDMRP_AUTO_DEVICE_DASHBD_SEL
*&---------------------------------------------------------------------*

TABLES : EDIDC.
SELECTION-SCREEN BEGIN OF BLOCK Details WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  s_creat FOR EDIDC-CREDAT.
SELECTION-SCREEN END OF BLOCK Details.
