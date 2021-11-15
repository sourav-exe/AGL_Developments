*&---------------------------------------------------------------------*
*& Report zprogram_amdp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprogram_amdp.

TABLES: ekko.

PARAMETERS: p_doc1 TYPE ebeln.
*            p_doc2 TYPE ebeln.

SELECT-OPTIONS: s_po_doc FOR ekko-ebeln,
                s_code FOR ekko-bukrs.

START-OF-SELECTION.

**********************************************************************
  IF NOT cl_abap_dbfeatures=>use_features( requested_features = VALUE #( ( cl_abap_dbfeatures=>amdp_table_function ) ) ).
    RETURN.
  ENDIF.

**********************************************************************
  DATA(lo_amdp) =  NEW zcl_amdp(  ).
**********************************************************************
  IF p_doc1 IS NOT INITIAL.

    lo_amdp->get_po_header_details(
      EXPORTING
        it_po_document       = VALUE #( ( purchasing_document = p_doc1 ) )
*                                      ( purchasing_document = p_doc2 ) )
      IMPORTING
        et_po_header_details = DATA(lt_po_details) ).

    cl_demo_output=>display( data = lt_po_details
                             name = |Purchase Order Header Details| ).
  ENDIF.
**********************************************************************
  IF p_doc1 IS NOT INITIAL.
    lo_amdp->get_po_header_item_details(
      EXPORTING
        it_po_document            = VALUE #( ( purchasing_document = p_doc1 ) )
*                                           ( purchasing_document = p_doc2 ) )
      IMPORTING
        et_po_header_item_details = DATA(lt_po_header_item_details) ).

    cl_demo_output=>display( data = lt_po_header_item_details
                      name = |Purchase Order Header And Item Details| ).
  ENDIF.
**********************************************************************
  IF s_code IS NOT INITIAL OR
     s_po_doc IS NOT INITIAL.

    DATA(lv_where_clause) = cl_shdb_seltab=>combine_seltabs( it_named_seltabs  = VALUE #( ( name = 'EBELN' dref = REF #( s_po_doc[] ) )
                                                                                          ( name = 'BUKRS' dref = REF #( s_code[] ) ) )
*  iv_client_field   = 'MANDT'
    ).

    lo_amdp->get_po_head_select_opt(
      EXPORTING
        iv_filter          = lv_where_clause
      IMPORTING
        et_po_head_details = DATA(lt_po_head_select_options) ).

    cl_demo_output=>display( data = lt_po_head_select_options
                      name = |Purchase Order Header Details Using AMDP Select Options| ).
  ENDIF.
**********************************************************************
  IF s_code IS NOT INITIAL OR
     s_po_doc IS NOT INITIAL.

    DATA(lv_where_clause2) = cl_shdb_seltab=>combine_seltabs( it_named_seltabs  = VALUE #( ( name = 'EBELN' dref = REF #( s_po_doc[] ) )
                                                                                          ( name = 'BUKRS' dref = REF #( s_code[] ) ) )
*  iv_client_field   = 'MANDT'
    ).

    SELECT * FROM zcds_table_functon( p_select_options = @lv_where_clause2 ) USING CLIENT '200' INTO TABLE @DATA(lt_po_head_select_options_tf).

    cl_demo_output=>display( data = lt_po_head_select_options_tf
                      name = |Purchase Order Header Details Using CDS Table Function Select Options| ).
  ENDIF.



**********************************************************************
