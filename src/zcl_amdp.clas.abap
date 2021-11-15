CLASS zcl_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.

    TYPES: BEGIN OF ty_po_doc,
             purchasing_document TYPE ebeln,
           END OF ty_po_doc,

           tty_po_doc TYPE TABLE OF ty_po_doc,

           BEGIN OF ty_po_header,
             purchasing_document TYPE ebeln,
             company_code        TYPE bukrs,
             document_category   TYPE ebstyp,
             document_type       TYPE esart,
             status              TYPE estak,
             creation_date       TYPE erdat,
             creator             TYPE ernam,
           END OF ty_po_header,

           tty_po_header TYPE TABLE OF ty_po_header,

           BEGIN OF ty_po_header_item,
             purchasing_document TYPE ebeln,
             document_category   TYPE ebstyp,
             document_type       TYPE esart,
             status              TYPE estak,
             creation_date       TYPE erdat,
             creator             TYPE ernam,
             item_no             TYPE ebelp,
             text                TYPE txz01,
             material_no         TYPE matnr,
             mat_no              TYPE ematnr,
           END OF ty_po_header_item,

           tty_po_header_item TYPE TABLE OF ty_po_header_item,

           BEGIN OF ty_po_header_select_opt,
             purchasing_document         TYPE ebeln,
             company_code                TYPE bukrs,
             document_category           TYPE ebstyp,
             document_type               TYPE esart,
             status                      TYPE estak,
             creation_date               TYPE erdat,
             creation_date_formatted(10) TYPE c,
             creator                     TYPE ernam,
             vendor_name                 TYPE bu_nameor1,
           END OF ty_po_header_select_opt,

           tty_po_header_select_opt TYPE TABLE OF ty_po_header_select_opt.


    METHODS: get_po_header_details
      IMPORTING VALUE(it_po_document)       TYPE tty_po_doc
      EXPORTING VALUE(et_po_header_details) TYPE tty_po_header.


    METHODS: get_po_header_item_details
      IMPORTING VALUE(it_po_document)            TYPE tty_po_doc
      EXPORTING VALUE(et_po_header_item_details) TYPE tty_po_header_item.

    METHODS: get_po_head_select_opt
      IMPORTING VALUE(iv_filter)          TYPE string
      EXPORTING VALUE(et_po_head_details) TYPE tty_po_header_select_opt.

    CLASS-METHODS: get_po_head_table_funct FOR TABLE FUNCTION zcds_table_functon.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_amdp IMPLEMENTATION.

  METHOD get_po_header_details BY DATABASE PROCEDURE
                               FOR HDB LANGUAGE SQLSCRIPT
                               OPTIONS READ-ONLY USING ekko.

    et_po_header_details = SELECT ebeln as purchasing_document,
                                  bukrs as company_code ,
                                  bstyp as document_category ,
                                  bsart as document_type,
                                  statu as status,
                                  aedat as creation_date,
                                  ernam as creator
                                  from :it_po_document
                                  LEFT outer join ekko
                                  on ebeln = purchasing_document
                                  where mandt = 200;
  ENDMETHOD.

  METHOD get_po_header_item_details BY DATABASE PROCEDURE
                                    FOR HDB LANGUAGE SQLSCRIPT
                                    OPTIONS READ-ONLY USING zcl_amdp=>get_po_header_details ekpo.

    call "ZCL_AMDP=>GET_PO_HEADER_DETAILS" ( it_po_document => :it_po_document,
                                             et_po_header_details => :lt_po_header_details );

et_po_header_item_details = SELECT a.purchasing_document,
                                   a.document_category ,
                                   a.document_type,
                                   a.status,
                                   a.creation_date,
                                   a.creator,
                                   b.ebelp as item_no,
                                   b.txz01 as text,
                                   b.matnr as material_no,
                                   b.ematn as mat_no
                                   from :lt_po_header_details as a
                                   left outer join ekpo as b
                                   on a.purchasing_document = b.ebeln
                                   where b.mandt = 200;

  ENDMETHOD.

  METHOD get_po_head_select_opt BY DATABASE PROCEDURE
                                FOR HDB LANGUAGE SQLSCRIPT
                                OPTIONS READ-ONLY USING ekko but000.

    lt_ekko = APPLY_FILTER(ekko, :iv_filter );

    et_po_head_details = SELECT a.ebeln as purchasing_document,
                                a.bukrs as company_code ,
                                a.bstyp as document_category ,
                                a.bsart as document_type,
                                a.statu as status,
                                a.aedat as creation_date,
                                to_varchar( a.aedat,'DD.MM.YYYY' ) as creation_date_formatted,
                                a.ernam as creator,
                                b.name_org1 as vendor_name
                                from :lt_ekko as a
                                LEFT outer join but000 as b
                                on a.lifnr = b.partner
                                where mandt = 200;
  endmethod.

  method get_po_head_table_funct BY DATABASE FUNCTION
                                 FOR HDB LANGUAGE SQLSCRIPT
                                 OPTIONS READ-ONLY
                                 USING ekko but000.

    lt_ekko = APPLY_FILTER(ekko, :p_select_options );

    RETURN
    SELECT
           a.mandt as client,
           a.ebeln as purchasing_document,
           a.bukrs as company_code ,
           a.bstyp as document_category ,
           a.bsart as document_type,
           a.statu as status,
           a.aedat as creation_date,
           to_varchar( a.aedat,'DD.MM.YYYY' ) as creation_date_formatted,
           a.ernam as creator,
           b.name_org1 as vendor_name
           from :lt_ekko as a
           LEFT outer join but000 as b
           on a.lifnr = b.partner
           where a.mandt = 200;

  ENDMETHOD.

ENDCLASS.






