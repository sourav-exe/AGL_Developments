*&---------------------------------------------------------------------*
*&  Include           ZICA_VARIANT_REGENERATE_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS : p_adhoc RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND u1,
             p_batch RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
PARAMETERS : p_path  TYPE string MODIF ID m1,
             p_idkey LIKE indx-srtfd NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK a2.

SELECTION-SCREEN: FUNCTION KEY 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM get_desktop_path CHANGING p_path.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.
  PERFORM modify_function_keys.

AT SELECTION-SCREEN.
  PERFORM handle_user_req.
