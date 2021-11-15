REPORT YIDMRP_AUTO_DEVICE_DASHBD.

*======================================================================*
*OVERVIEW

*Automation of Device Dashboard


*======================================================================*
*
* CHANGE HISTORY
* Date            Name                  Reference             Description
* 17/07/2018      Sourav Roy            WO: WO0000000630017   Automation of Device Dashboard
*======================================================================*


*&---------------------------------------------------------------------*
*&                Include for data Declaration
*&---------------------------------------------------------------------*
INCLUDE YIDMRP_AUTO_DEVICE_DASHBD_TOP.
*INCLUDE ZIDMRP_AUTO_DEVICE_DASHBD_top.


*&---------------------------------------------------------------------*
*&                Include for Selection Screen
*&---------------------------------------------------------------------*
INCLUDE YIDMRP_AUTO_DEVICE_DASHBD_SEL.
*INCLUDE ZIDMRP_AUTO_DEVICE_DASHBD_sel.


*&---------------------------------------------------------------------*
*&                Include for Subroutines
*&---------------------------------------------------------------------*
INCLUDE YIDMRP_AUTO_DEVICE_DASHBD_SUB.
*INCLUDE ZIDMRP_AUTO_DEVICE_DASHBD_sub.



*&---------------------------------------------------------------------*
*&             INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
SY-TITLE = 'Automation of Device Dashboard'(034).

*&---------------------------------------------------------------------*
*&             INITIALIZATION
*&---------------------------------------------------------------------*
At SELECTION-SCREEN.
* validate date field
perform validate_field.


*&---------------------------------------------------------------------*
*&                START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

* get the success/fail Idocs with basic types:  Z_ISU_IDE_MDN_GAS, Z_ISU_IDE_MDN_BASIC, Z_ISU_IDE_MDN_INTERVAL
perform get_data.

* get Email Recipients
perform get_recipients.

* Send Email
perform send_email.
