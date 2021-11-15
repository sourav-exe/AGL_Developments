*&---------------------------------------------------------------------*
*&  Include           ZICA_VARIANT_REGENERATE_TOP
*&---------------------------------------------------------------------*
TABLES: sscrfields.
DATA: go_var_regen TYPE REF TO zcl_ica_variant_regeneration,
      gt_functxt   TYPE smp_dyntxt.
CONSTANTS : gc_e           TYPE char1 VALUE 'E',
            gc_auth_object TYPE xuobject VALUE 'ZBASIS',
            gc_actvt       TYPE xufield VALUE 'ACTVT',
            gc_16          TYPE xuval VALUE '16',
            gc_ztranscode  TYPE xufield VALUE 'ZTRANSCODE',
            gc_tcode       TYPE xuval VALUE 'ZICA_REGEN_VARIANT',
            gc_zreport     TYPE xufield VALUE 'ZREPORT',
            gc_report      TYPE xuval VALUE 'ZICA_VARIANT_REGENERATE'.
