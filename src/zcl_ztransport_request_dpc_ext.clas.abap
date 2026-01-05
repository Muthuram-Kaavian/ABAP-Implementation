class ZCL_ZTRANSPORT_REQUEST_DPC_EXT definition
  public
  inheriting from ZCL_ZTRANSPORT_REQUEST_DPC
  create public .

public section.
protected section.

  methods TRANSPORTREQUEST_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZTRANSPORT_REQUEST_DPC_EXT IMPLEMENTATION.


METHOD transportrequest_get_entityset.

  DATA: lt_filters          TYPE /iwbep/t_mgw_select_option,
        lv_check            TYPE string,
        lv_trkorr_pattern   TYPE string.

  " Internal table to hold results from E070
  DATA: lt_e070 TYPE TABLE OF e070.

  " *** Properly typed variables for domain values ***
  DATA: lv_trstatus   TYPE trstatus,
        lv_trfunction TYPE trfunction.

  " Base filter for status
  lv_check = `trstatus IN ('D', 'L')`.

  " Get filters from OData request
  lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

  " Process TRKORR filter
  LOOP AT lt_filters INTO DATA(ls_filter).
    CASE ls_filter-property.
      WHEN 'TRKORR'.
        LOOP AT ls_filter-select_options INTO DATA(ls_so).
          IF ls_so-option = 'CP' OR ls_so-low CS '*'.
            lv_trkorr_pattern = ls_so-low.
            REPLACE ALL OCCURRENCES OF '*' IN lv_trkorr_pattern WITH '%'.
            lv_check = |{ lv_check } AND trkorr LIKE '{ lv_trkorr_pattern }'|.
          ELSE.
            lv_check = |{ lv_check } AND trkorr = '{ ls_so-low }'|.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDLOOP.

  " 1. --- SELECT FROM E070 ---
  SELECT trkorr, trfunction, trstatus, tarsystem
    FROM e070
    INTO TABLE @lt_e070
   WHERE (lv_check).

  " 2. --- POPULATE THE FINAL ENTITY SET ---
  LOOP AT lt_e070 INTO DATA(ls_e070).
    APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).

    <fs_entity>-trkorr     = ls_e070-trkorr.
    <fs_entity>-tarsystem  = ls_e070-tarsystem.

    " *** Assign to properly typed variables ***
    lv_trstatus   = ls_e070-trstatus.
    lv_trfunction = ls_e070-trfunction.

    " --- Map Status Description ---
    CASE lv_trstatus.
      WHEN 'N'.  <fs_entity>-trstatus = 'Released (with import protection for repaired objects)'.
      WHEN 'P'.  <fs_entity>-trstatus = 'Release Preparation'.
      WHEN 'D'.  <fs_entity>-trstatus = 'Modifiable'.
      WHEN 'L'.  <fs_entity>-trstatus = 'Modifiable, Protected'.
      WHEN 'O'.  <fs_entity>-trstatus = 'Release Started'.
      WHEN 'R'.  <fs_entity>-trstatus = 'Released'.
      WHEN OTHERS.
        <fs_entity>-trstatus = ls_e070-trstatus.
    ENDCASE.

    " --- Map Function Description ---
    CASE lv_trfunction.
      WHEN 'K'.  <fs_entity>-trfunction = 'Workbench Request'.
      WHEN 'W'.  <fs_entity>-trfunction = 'Customizing Request'.
      WHEN 'C'.  <fs_entity>-trfunction = 'Relocation of Objects Without Package Change'.
      WHEN 'O'.  <fs_entity>-trfunction = 'Relocation of Objects with Package Change'.
      WHEN 'E'.  <fs_entity>-trfunction = 'Relocation of complete package'.
      WHEN 'T'.  <fs_entity>-trfunction = 'Transport of Copies'.
      WHEN 'S'.  <fs_entity>-trfunction = 'Development/Correction'.
      WHEN 'R'.  <fs_entity>-trfunction = 'Repair'.
      WHEN 'X'.  <fs_entity>-trfunction = 'Unclassified Task'.
      WHEN 'Q'.  <fs_entity>-trfunction = 'Customizing Task'.
      WHEN 'G'.  <fs_entity>-trfunction = 'Piece List for CTS Project'.
      WHEN 'M'.  <fs_entity>-trfunction = 'Client Transport Request'.
      WHEN 'P'.  <fs_entity>-trfunction = 'Piece List for Upgrade'.
      WHEN 'D'.  <fs_entity>-trfunction = 'Piece List for Support Package'.
      WHEN 'F'.  <fs_entity>-trfunction = 'Piece List'.
      WHEN 'L'.  <fs_entity>-trfunction = 'Deletion transport'.
      WHEN 'Y'.  <fs_entity>-trfunction = 'Piece list for commit'.
      WHEN OTHERS.
        <fs_entity>-trfunction = ls_e070-trfunction.
    ENDCASE.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
