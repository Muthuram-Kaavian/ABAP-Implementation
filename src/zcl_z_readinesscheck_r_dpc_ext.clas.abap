class ZCL_Z_READINESSCHECK_R_DPC_EXT definition
  public
  inheriting from ZCL_Z_READINESSCHECK_R_DPC
  create public .

public section.
protected section.

  methods CONSISTENCYCHECK_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_READINESSCHECK_R_DPC_EXT IMPLEMENTATION.


METHOD consistencycheck_create_entity.
  DATA: ls_input_data   TYPE ZCL_Z_READINESSCHECK_R_MPC=>ts_consistencycheck,
        lv_stack_string TYPE string,
        lv_stack        TYPE /sdf/cl_rc_chk_utility=>ty_bormnr,
        gt_cons_result  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_tab,
        gt_header_lines TYPE salv_wd_t_string,
        gs_header_info  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_header_str.

  " Sub-types for XML parsing
  TYPES: ty_descr_line TYPE string.
  TYPES: ty_descr_tab  TYPE STANDARD TABLE OF ty_descr_line WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_clas_chk_result_str,
           return_code  TYPE string,
           descriptions TYPE ty_descr_tab,
           check_sub_id TYPE string,
         END OF ty_clas_chk_result_str.

  TYPES: ty_clas_chk_result_tab TYPE STANDARD TABLE OF ty_clas_chk_result_str
                                  WITH DEFAULT KEY.

  DATA: ls_res          TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_str,
        lt_clas_result  TYPE ty_clas_chk_result_tab,
        ls_clas_result  TYPE ty_clas_chk_result_str,
        lv_rc_int       TYPE i,
        lv_msg          TYPE string,
        lx_root         TYPE REF TO cx_root,
        gv_line         TYPE string.

  " --- DEFINE CONSTANTS ---
  DATA(lv_crlf) = cl_abap_char_utilities=>cr_lf.

  " 1. GET INPUT DATA
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).
  lv_stack_string = ls_input_data-targetstack.

  IF lv_stack_string IS INITIAL.
    mo_context->get_message_container( )->add_message_text_only(
      iv_msg_type = 'E' iv_msg_text = 'Target stack is required' ).
    RETURN.
  ENDIF.

  lv_stack = lv_stack_string.

  " 2. GET CONSISTENCY CHECK RESULTS
  CLEAR: gt_cons_result, gt_header_lines, gs_header_info.

  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_consistency_result_get
    EXPORTING
      iv_target_stack      = lv_stack
    IMPORTING
      et_cons_chk_result   = gt_cons_result
      et_cons_header_info  = gt_header_lines
      es_header_info       = gs_header_info.

  IF gt_cons_result IS INITIAL AND gt_header_lines IS INITIAL.
    er_entity = VALUE #(
      targetstack = lv_stack_string
      filecontent = 'No data'
      filename    = |consistency_error_{ sy-datum }_{ sy-uzeit }.xls|
      filetype    = 'XLS'
      message     = |No consistency check result found for this target stack.|
    ).
    RETURN.
  ENDIF.

  " 3. BUILD HTML TABLE (XLS format)
  DATA: lv_html_content TYPE string.

  lv_html_content = |<html><head><meta charset="UTF-8"></head><body>| && lv_crlf &&
    |<table border="1" cellspacing="0" cellpadding="5" width="100%">| && lv_crlf &&
    |<tr>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>S.NO</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>STATUS</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>SOURCE</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>SITEM_ID</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>CHECK_CLASS</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>SAP_NOTE</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>RETURN_CODE</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>CHECK_SUB_ID</b></font></th>| && lv_crlf &&
    |<th bgcolor="#4472C4" align="left"><font color="white"><b>MESSAGE_TEXT</b></font></th>| && lv_crlf &&
    |</tr>| && lv_crlf.

  DATA: lv_sr_no       TYPE i VALUE 0,
        lv_status      TYPE string,
        lv_status_icon TYPE string.

  " 3A. Overall header lines (top block)
  LOOP AT gt_header_lines INTO gv_line.
    CONDENSE gv_line.
    IF gv_line IS INITIAL.
      CONTINUE.
    ENDIF.

    lv_sr_no = lv_sr_no + 1.

    IF gv_line CS 'not implemented'
       OR gv_line CS 'error'
       OR gv_line CS 'failed'.
      lv_status = 'Error'.
      lv_status_icon = '❌'.
    ELSEIF gv_line CS 'skipped'
           OR gv_line CS 'no consistency check defined'.
      lv_status = 'Warning'.
      lv_status_icon = '⚠️'.
    ELSE.
      lv_status = 'Info'.
      lv_status_icon = '🔵'.
    ENDIF.

    " Escape HTML
    DATA(lv_msg_escaped) = gv_line.
    REPLACE ALL OCCURRENCES OF '&' IN lv_msg_escaped WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN lv_msg_escaped WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN lv_msg_escaped WITH '&gt;'.

    lv_html_content = lv_html_content && |<tr>| && lv_crlf &&
      |<td align="left">{ lv_sr_no }</td>| && lv_crlf &&
      |<td align="left">{ lv_status_icon } { lv_status }</td>| && lv_crlf &&
      |<td align="left">Overall</td>| && lv_crlf &&
      |<td align="left"></td>| && lv_crlf &&
      |<td align="left"></td>| && lv_crlf &&
      |<td align="left"></td>| && lv_crlf &&
      |<td align="left"></td>| && lv_crlf &&
      |<td align="left"></td>| && lv_crlf &&
      |<td align="left">{ lv_msg_escaped }</td>| && lv_crlf &&
      |</tr>| && lv_crlf.
  ENDLOOP.

  " 3B. Per-item details (HEADER_INFO_TABLE + XML)
  LOOP AT gt_cons_result INTO ls_res.

    " **ADD: Highest consistency check return code for each item**
    IF ls_res-return_code IS NOT INITIAL.
      lv_sr_no = lv_sr_no + 1.

      " Get status from RC
      IF ls_res-return_code IS INITIAL.
        lv_status = 'Success'.
        lv_status_icon = '✅'.
      ELSEIF ls_res-return_code <= 4.
        lv_status = 'Warning'.
        lv_status_icon = '⚠️'.
      ELSE.
        lv_status = 'Error'.
        lv_status_icon = '❌'.
      ENDIF.

      DATA(lv_highest_rc_msg) = |Highest consistency check return code: { ls_res-return_code }|.

      lv_html_content = lv_html_content && |<tr>| && lv_crlf &&
        |<td align="left">{ lv_sr_no }</td>| && lv_crlf &&
        |<td align="left">{ lv_status_icon } { lv_status }</td>| && lv_crlf &&
        |<td align="left">Header</td>| && lv_crlf &&
        |<td align="left">{ ls_res-sitem_id }</td>| && lv_crlf &&
        |<td align="left">{ ls_res-check_class }</td>| && lv_crlf &&
        |<td align="left">{ ls_res-sap_note }</td>| && lv_crlf &&
        |<td align="left">{ ls_res-return_code }</td>| && lv_crlf &&
        |<td align="left"></td>| && lv_crlf &&
        |<td align="left">{ lv_highest_rc_msg }</td>| && lv_crlf &&
        |</tr>| && lv_crlf.
    ENDIF.

    " Per-item HEADER_INFO_TABLE lines
    IF ls_res-header_info_table IS NOT INITIAL.
      LOOP AT ls_res-header_info_table ASSIGNING FIELD-SYMBOL(<ls_nv>).
        lv_sr_no = lv_sr_no + 1.

        " Get status from RC
        IF ls_res-return_code IS INITIAL.
          lv_status = 'Success'.
          lv_status_icon = '✅'.
        ELSEIF ls_res-return_code <= 4.
          lv_status = 'Warning'.
          lv_status_icon = '⚠️'.
        ELSE.
          lv_status = 'Error'.
          lv_status_icon = '❌'.
        ENDIF.

        " Escape HTML
        DATA(lv_nv_value) = <ls_nv>-value.
        REPLACE ALL OCCURRENCES OF '&' IN lv_nv_value WITH '&amp;'.
        REPLACE ALL OCCURRENCES OF '<' IN lv_nv_value WITH '&lt;'.
        REPLACE ALL OCCURRENCES OF '>' IN lv_nv_value WITH '&gt;'.

        lv_html_content = lv_html_content && |<tr>| && lv_crlf &&
          |<td align="left">{ lv_sr_no }</td>| && lv_crlf &&
          |<td align="left">{ lv_status_icon } { lv_status }</td>| && lv_crlf &&
          |<td align="left">Header</td>| && lv_crlf &&
          |<td align="left">{ ls_res-sitem_id }</td>| && lv_crlf &&
          |<td align="left">{ ls_res-check_class }</td>| && lv_crlf &&
          |<td align="left">{ ls_res-sap_note }</td>| && lv_crlf &&
          |<td align="left">{ ls_res-return_code }</td>| && lv_crlf &&
          |<td align="left">{ <ls_nv>-name }</td>| && lv_crlf &&
          |<td align="left">{ lv_nv_value }</td>| && lv_crlf &&
          |</tr>| && lv_crlf.
      ENDLOOP.
    ENDIF.

    " XML sub-check details
    IF ls_res-chk_clas_result_xstr IS NOT INITIAL.
      CLEAR lt_clas_result.

      TRY.
          CALL TRANSFORMATION id
            SOURCE XML ls_res-chk_clas_result_xstr
            RESULT clas_chk_result = lt_clas_result.

          LOOP AT lt_clas_result INTO ls_clas_result.
            lv_rc_int = ls_clas_result-return_code.

            " Get status from RC
            IF lv_rc_int IS INITIAL.
              lv_status = 'Success'.
              lv_status_icon = '✅'.
            ELSEIF lv_rc_int <= 4.
              lv_status = 'Warning'.
              lv_status_icon = '⚠️'.
            ELSE.
              lv_status = 'Error'.
              lv_status_icon = '❌'.
            ENDIF.

            LOOP AT ls_clas_result-descriptions INTO lv_msg.
              CONDENSE lv_msg.
              IF lv_msg IS INITIAL.
                CONTINUE.
              ENDIF.

              lv_sr_no = lv_sr_no + 1.

              " Escape HTML
              DATA(lv_xml_msg) = lv_msg.
              REPLACE ALL OCCURRENCES OF '&' IN lv_xml_msg WITH '&amp;'.
              REPLACE ALL OCCURRENCES OF '<' IN lv_xml_msg WITH '&lt;'.
              REPLACE ALL OCCURRENCES OF '>' IN lv_xml_msg WITH '&gt;'.

              lv_html_content = lv_html_content && |<tr>| && lv_crlf &&
                |<td align="left">{ lv_sr_no }</td>| && lv_crlf &&
                |<td align="left">{ lv_status_icon } { lv_status }</td>| && lv_crlf &&
                |<td align="left">XML</td>| && lv_crlf &&
                |<td align="left">{ ls_res-sitem_id }</td>| && lv_crlf &&
                |<td align="left">{ ls_res-check_class }</td>| && lv_crlf &&
                |<td align="left">{ ls_res-sap_note }</td>| && lv_crlf &&
                |<td align="left">{ lv_rc_int }</td>| && lv_crlf &&
                |<td align="left">{ ls_clas_result-check_sub_id }</td>| && lv_crlf &&
                |<td align="left">{ lv_xml_msg }</td>| && lv_crlf &&
                |</tr>| && lv_crlf.
            ENDLOOP.
          ENDLOOP.

        CATCH cx_root INTO lx_root.
          " Skip XML parsing errors
          CONTINUE.
      ENDTRY.
    ENDIF.

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
    filename    = |consistency_check_Report_{ sy-datum }_{ sy-uzeit }|
    filetype    = 'XLS'
    message     = |Success: { lv_sr_no } rows found.|
  ).

ENDMETHOD.
ENDCLASS.
