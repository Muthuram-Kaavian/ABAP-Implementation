class ZCL_ZTABLE_SEARCH_D_01_DPC_EXT definition
  public
  inheriting from ZCL_ZTABLE_SEARCH_D_01_DPC
  create public .

public section.
protected section.

  methods TABLESET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZTABLE_SEARCH_D_01_DPC_EXT IMPLEMENTATION.


METHOD tableset_get_entityset.

  DATA lt_filter_select_options TYPE /iwbep/t_mgw_select_option.
   DATA ls_filter_so LIKE LINE OF lt_filter_select_options.
   DATA lr_tabname TYPE RANGE OF tabname.
   DATA ls_tabname_range LIKE LINE OF lr_tabname.

  " Get filter from OData request
   lt_filter_select_options = io_tech_request_context->get_filter( )->get_filter_select_options( ).
   READ TABLE lt_filter_select_options INTO ls_filter_so WITH KEY property = 'TABNAME'.

  " Flag to track valid custom tables - REMOVE THE VALIDATION LOGIC
   DATA lv_is_valid_filter TYPE abap_bool VALUE abap_true.

  " Process filter if found
   IF sy-subrc = 0.
     LOOP AT ls_filter_so-select_options INTO DATA(ls_select_option).
       " Convert to uppercase for consistent processing
       DATA(lv_low_upper) = to_upper( ls_select_option-low ).

      " Build range table for database query - MODIFIED FOR CONTAINS SEARCH
       APPEND VALUE #( sign = 'I' option = 'CP' low = '*' && lv_low_upper && '*' ) TO lr_tabname.
     ENDLOOP.
   ENDIF.

  " Execute database query - ADD Z/Y FILTER IN DATABASE QUERY
   SELECT tabname
     FROM dd02l
     INTO TABLE @et_entityset
     WHERE tabname IN @lr_tabname
       AND ( tabname LIKE 'Z%' OR tabname LIKE 'Y%' )  " ONLY tables starting with Z or Y
       AND as4local  = 'A'           " Only active objects
       AND as4vers   = '0000'        " Only current version
       AND tabclass  = 'TRANSP'.     " ONLY TRANSPARENT TABLES (no structures, no CDS)

  " If no results found and filter was applied, show error message
   IF lines( et_entityset ) = 0 AND lr_tabname IS NOT INITIAL.
     DATA(lo_message_container) = me->mo_context->get_message_container( ).
     lo_message_container->add_message_text_only(
       EXPORTING
         iv_msg_text = 'No custom tables found with the specified filter. Only tables starting with Z or Y are allowed.'
         iv_msg_type = 'E'
     ).
     RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
       EXPORTING
         message_container = lo_message_container.
   ENDIF.

ENDMETHOD.
ENDCLASS.
