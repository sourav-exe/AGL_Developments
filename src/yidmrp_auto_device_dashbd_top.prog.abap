*&---------------------------------------------------------------------*
*&  Include           ZIDMRP_AUTO_DEVICE_DASHBD_TOP
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF tc_constants,
             success  TYPE edi_status VALUE '53',
             fail     TYPE edi_status VALUE '56',
             gas      TYPE edi_idoctp VALUE 'Z_ISU_IDE_MDN_GAS',
             basic    TYPE edi_idoctp VALUE 'Z_ISU_IDE_MDN_BASIC',
             interval TYPE edi_idoctp VALUE 'Z_ISU_IDE_MDN_INTERVAL',
           END OF tc_constants.

TYPES: BEGIN OF ty_idoc_rec,
         basic_type TYPE  edi_idoctp,
         status     TYPE edi_status,
         created_on TYPE edi_ccrdat,
       END OF ty_idoc_rec,

       BEGIN OF ty_idoc_cal,
         meter_type(100) TYPE c,
         success_idoc    TYPE num12,
         failure_idoc    TYPE num12,
         index           TYPE p DECIMALS 2,
         index_all(120)  type c,
         status(1)       TYPE c,
       END OF ty_idoc_cal.




DATA: gt_idoc_rec TYPE TABLE OF ty_idoc_rec,
      gt_idoc_cal TYPE TABLE OF ty_idoc_cal,
      gt_recipients TYPE ZIDMDT_EMAIL_RECIPIENT.
