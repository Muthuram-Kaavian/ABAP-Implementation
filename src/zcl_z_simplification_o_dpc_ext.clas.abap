class ZCL_Z_SIMPLIFICATION_O_DPC_EXT definition
  public
  inheriting from ZCL_Z_SIMPLIFICATION_O_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.

  methods SIMPLIFICATIONCH_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_SIMPLIFICATION_O_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

  DATA: ls_input_data TYPE zcl_z_simplification_o_mpc=>ts_simplificationcheck,
        ls_output_data TYPE zcl_z_simplification_o_mpc=>ts_simplificationcheck,
        lv_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr,
        lt_check_result TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
        ls_check_result TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str,
        lt_excel TYPE TABLE OF string,
        lv_row TYPE string.

  " Check if this is for our entity set
  IF iv_entity_set_name <> 'SimplificationChecks'.
    RETURN.
  ENDIF.

  " Get input data from request
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).

  " Get target stack from request
  lv_stack = ls_input_data-targetstack.

  IF lv_stack IS INITIAL.
    " Return error if no stack provided
    mo_context->get_message_container( )->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Target stack is required' ).
    RETURN.
  ENDIF.

  " Call the BAPI to get simplification items
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
    EXPORTING
      iv_target_stack   = lv_stack
    IMPORTING
      et_rel_chk_result = lt_check_result.

  IF lt_check_result IS INITIAL.
    mo_context->get_message_container( )->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'No results found for the selected stack' ).
    RETURN.
  ENDIF.

  " 1. Create Excel Header Row
  lv_row = |RELEVANCE_STATUS\tID\tTITLE\tLOB\tBUSINESS_AREA\tNOTE\tCATEGORY\tCOMPONENT\tSTATUS_TEXT\tAPPLICATION_AREA\tSUMMARY\tCONSISTENCY_STATUS|.
  APPEND lv_row TO lt_excel.

  " 2. Create Data Rows
  LOOP AT lt_check_result INTO ls_check_result.
    CLEAR lv_row.

    " Prepare relevance icon
    DATA(lv_relevance) = ls_check_result-relevant_stat.
    IF lv_relevance IS INITIAL.
      lv_relevance = icon_light_out.
    ENDIF.

    " Prepare consistency icon
    DATA(lv_consistency) = ls_check_result-consistency_stat_disp(4).

    " Prepare LoB field
    DATA(lv_lob) = ls_check_result-lob_technology_des.
    IF lv_lob IS INITIAL.
      lv_lob = ls_check_result-app_area.
    ENDIF.

    " Build Excel row with all fields using tab separator
    lv_row = |{ lv_relevance }\t{ ls_check_result-sitem_id }\t{ ls_check_result-title_en }\t{ lv_lob }\t{ ls_check_result-business_area_des }\t{ ls_check_result-buz_imp_note }\t{ ls_check_result-category_text }\t{ ls_check_result-app_components }\t{
ls_check_result-proc_status }\t{ ls_check_result-app_area }\t{ ls_check_result-summary }\t{ lv_consistency }|.

    APPEND lv_row TO lt_excel.
  ENDLOOP.

  " 3. Convert table to XSTRING directly (more efficient)
  DATA: lt_solisti1 TYPE TABLE OF solisti1,
        ls_solisti1 TYPE solisti1,
        lv_xstring TYPE xstring.

  " Convert string table to SOLISTI1 format
  LOOP AT lt_excel INTO lv_row.
    ls_solisti1-line = lv_row.
    APPEND ls_solisti1 TO lt_solisti1.
  ENDLOOP.

  " Convert to XSTRING using standard function
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lt_solisti1
    IMPORTING
      buffer = lv_xstring
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  IF sy-subrc <> 0.
    " Fallback: simple conversion
    lv_xstring = cl_abap_codepage=>convert_to(
      source = concat_lines_of( table = lt_excel sep = cl_abap_char_utilities=>cr_lf ) ).
  ENDIF.

  " 4. Convert to Base64
  DATA(lv_base64) = cl_http_utility=>encode_x_base64( lv_xstring ).

  " 5. Prepare response data
  ls_output_data-targetstack = lv_stack.
  ls_output_data-filecontent = lv_base64.  " Base64 encoded string
  ls_output_data-filename = |S4_Simplification_Results_{ lv_stack }_{ sy-datum }_{ sy-uzeit }.xls|.
  ls_output_data-filetype = 'XLS'.
  ls_output_data-message = |File generated successfully with { lines( lt_check_result ) } items|.

  " 6. Return the deep entity response
  copy_data_to_ref(
    EXPORTING
      is_data = ls_output_data
    CHANGING
      cr_data = er_deep_entity
  ).

ENDMETHOD.


METHOD simplificationch_create_entity.

  DATA: ls_input_data   TYPE zcl_z_simplification_o_mpc=>ts_simplificationcheck,
        lv_stack_string TYPE string,
        lv_stack        TYPE /sdf/cl_rc_chk_utility=>ty_bormnr,
        lt_check_result TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab.

  " --- DEFINE CONSTANTS ---
  DATA(lv_crlf) = cl_abap_char_utilities=>cr_lf.
  DATA(lv_tab) = cl_abap_char_utilities=>horizontal_tab.

  " 1. GET INPUT DATA
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).
  lv_stack_string = ls_input_data-targetstack.

  IF lv_stack_string IS INITIAL.
    mo_context->get_message_container( )->add_message_text_only(
      iv_msg_type = 'E' iv_msg_text = 'Target stack is required' ).
    RETURN.
  ENDIF.

  lv_stack = lv_stack_string.

  " 2. GET READINESS CHECKS
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
    EXPORTING
      iv_target_stack   = lv_stack
    IMPORTING
      et_rel_chk_result = lt_check_result.

  " Fallback Logic
  IF lt_check_result IS INITIAL.
    DATA(lv_short_stack) = 'SAPS/4HANA2023[03(02'.
    CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
      EXPORTING
        iv_target_stack   = lv_short_stack
      IMPORTING
        et_rel_chk_result = lt_check_result.
  ENDIF.

  IF lt_check_result IS INITIAL.
    er_entity = VALUE #(
      targetstack = lv_stack_string
      filecontent = 'No data'
      filename    = |error_{ sy-datum }_{ sy-uzeit }.xls|
      filetype    = 'XLS'
      message     = |No data found.|
    ).
    RETURN.
  ENDIF.

  " 3. BUILD HTML TABLE (XLS format - Excel recognizes HTML tables as XLS)
  DATA: lv_html_content TYPE string.

  lv_html_content = |<html><head><meta charset="UTF-8"></head><body>| && lv_crlf &&
    |<table border="1" cellspacing="0" cellpadding="5" width="100%">| && lv_crlf &&
    |<tr>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>RELEVANCE_STATUS</b></font></th>| && lv_crlf &&  " Inline styling
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>ID</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>TITLE</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>LOB</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>BUSINESS_AREA</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>NOTE</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>CATEGORY</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>COMPONENT</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>STATUS_TEXT</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>APPLICATION_AREA</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>SUMMARY</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>CONSISTENCY_STATUS</b></font></th>| && lv_crlf &&
    |</tr>| && lv_crlf.

  LOOP AT lt_check_result INTO DATA(ls_check).

    " --- SYMBOL MAPPING ---
    DATA: lv_rel_symbol  TYPE string,
          lv_cons_symbol TYPE string.

    " Relevance Status
    IF ls_check-relevant_stat CS '@5D@'.
      lv_rel_symbol = '⚠️'.
    ELSEIF ls_check-relevant_stat CS '@01@'.
      lv_rel_symbol = '✅'.
    ELSEIF ls_check-relevant_stat CS '@08@'.
      lv_rel_symbol = '🟢'.
    ELSEIF ls_check-relevant_stat CS '@0A@'.
      lv_rel_symbol = '🔴'.
    ELSE.
      lv_rel_symbol = ls_check-relevant_stat.
    ENDIF.

    " Consistency Status
    IF ls_check-consistency_stat_disp CS '@3U'.
       lv_cons_symbol = '🛑'.
    ELSEIF ls_check-consistency_stat_disp CS '@00'.
       lv_cons_symbol = '❌'.
    ELSEIF ls_check-consistency_stat_disp CS '@5D'.
       lv_cons_symbol = '⚠️'.
    ELSEIF ls_check-consistency_stat_disp CS '@5B'.
       lv_cons_symbol = '❌'.
    ELSEIF ls_check-consistency_stat_disp CS '@5C'.
       lv_cons_symbol = '✅'.
    ELSE.
       lv_cons_symbol = replace( val = ls_check-consistency_stat_disp sub = '@' with = '' occ = 0 ).
    ENDIF.

    " LOB Logic
    DATA(lv_lob) = COND #( WHEN ls_check-lob_technology_des IS NOT INITIAL
                           THEN ls_check-lob_technology_des ELSE ls_check-app_area ).

    " --- DATA SANITIZATION ---
    " Replace tabs and newlines with spaces - FIXED SYNTAX
    DATA(lv_title) = ls_check-title_en.
    REPLACE ALL OCCURRENCES OF lv_crlf IN lv_title WITH ' '.  " Fixed syntax
    REPLACE ALL OCCURRENCES OF lv_tab IN lv_title WITH ' '.   " Fixed syntax

    DATA(lv_summary) = ls_check-summary.
    REPLACE ALL OCCURRENCES OF lv_crlf IN lv_summary WITH ' '.  " Fixed syntax
    REPLACE ALL OCCURRENCES OF lv_tab IN lv_summary WITH ' '.   " Fixed syntax

    DATA(lv_note) = ls_check-buz_imp_note.
    REPLACE ALL OCCURRENCES OF lv_crlf IN lv_note WITH ' '.     " Fixed syntax
    REPLACE ALL OCCURRENCES OF lv_tab IN lv_note WITH ' '.      " Fixed syntax

    " Escape HTML special characters
    REPLACE ALL OCCURRENCES OF '&' IN lv_title WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN lv_title WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN lv_title WITH '&gt;'.

    REPLACE ALL OCCURRENCES OF '&' IN lv_summary WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN lv_summary WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN lv_summary WITH '&gt;'.

    REPLACE ALL OCCURRENCES OF '&' IN lv_note WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN lv_note WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN lv_note WITH '&gt;'.

    " Build table row - use align="left" for consistency
    lv_html_content = lv_html_content && |<tr>| && lv_crlf &&
      |<td align="left">{ lv_rel_symbol }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-sitem_id }</td>| && lv_crlf &&
      |<td align="left">{ lv_title }</td>| && lv_crlf &&
      |<td align="left">{ lv_lob }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-business_area_des }</td>| && lv_crlf &&
      |<td align="left">{ lv_note }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-category_text }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-app_components }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-proc_status }</td>| && lv_crlf &&
      |<td align="left">{ ls_check-app_area }</td>| && lv_crlf &&
      |<td align="left">{ lv_summary }</td>| && lv_crlf &&
      |<td align="left">{ lv_cons_symbol }</td>| && lv_crlf &&
      |</tr>| && lv_crlf.

  ENDLOOP.

  lv_html_content = lv_html_content && |</table>| && lv_crlf &&
    |</body></html>|.

  " 4. CONVERT TO BINARY (UTF-8)
  DATA(lv_xstring) = cl_abap_codepage=>convert_to( source = lv_html_content ).

  " 5. Encode to Base64
  DATA(lv_base64) = cl_http_utility=>encode_x_base64( lv_xstring ).

  " Return
  er_entity = VALUE #(
    targetstack = lv_stack_string
    filecontent = lv_base64
    filename    = |Simplification_Results_{ sy-datum }_{ sy-uzeit }|
    filetype    = 'XLS'
    message     = |Success: { lines( lt_check_result ) } items found.|
  ).

ENDMETHOD.
ENDCLASS.
