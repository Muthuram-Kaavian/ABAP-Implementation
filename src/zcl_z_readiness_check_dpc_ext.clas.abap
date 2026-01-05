class ZCL_Z_READINESS_CHECK_DPC_EXT definition
  public
  inheriting from ZCL_Z_READINESS_CHECK_DPC
  create public .

public section.
protected section.

  methods ZRC_RESULTSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_READINESS_CHECK_DPC_EXT IMPLEMENTATION.


METHOD zrc_resultset_create_entity.
*--------------------------------------------------------------------*
*  Input  : ZRC_RESULT (only field STACK is relevant)
*  Output : same structure with
*           - FILECONTENT (Base64 Excel)
*           - FILENAME
*           - FILETYPE
*           - MESSAGE
*--------------------------------------------------------------------*

*--- Entity structure (DDIC ZRC_RESULT) ------------------------------*
  DATA: ls_input  TYPE zrc_result,
        ls_output TYPE zrc_result.

*--- Read input payload from OData request --------------------------*
  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_input ).

  DATA(lv_stack_string) = ls_input-stack.
  DATA lv_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

  IF lv_stack_string IS INITIAL.
    ls_output-message     = 'STACK is required'.
    ls_output-filecontent = ''.
    ls_output-filename    = |consistency_error_{ sy-datum }_{ sy-uzeit }.xlsx|.
    ls_output-filetype    = 'XLSX'.
    er_entity             = ls_output.
    RETURN.
  ENDIF.

  lv_stack = lv_stack_string.

*=====================================================================*
* 1. Call consistency check (same source as your report)
*=====================================================================*
  TYPES: ty_descr_line TYPE string.
  TYPES: ty_descr_tab  TYPE STANDARD TABLE OF ty_descr_line WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_clas_chk_result_str,
           return_code  TYPE string,
           descriptions TYPE ty_descr_tab,
           check_sub_id TYPE string,
         END OF ty_clas_chk_result_str.

  TYPES: ty_clas_chk_result_tab TYPE STANDARD TABLE OF ty_clas_chk_result_str
                                  WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_out,
           sr_no        TYPE i,
           status       TYPE c LENGTH 1,
           status_txt   TYPE string,
           source       TYPE c LENGTH 1,
           sitem_guid   TYPE guid_32,
           sitem_id     TYPE /sdf/cl_rc_chk_utility=>ty_smdb_title,
           check_class  TYPE /sdf/cl_rc_chk_utility=>ty_check_id,
           sap_note     TYPE string,
           return_code  TYPE /sdf/cl_rc_chk_utility=>ty_return_code,
           check_sub_id TYPE string,
           msg_text     TYPE string,
         END OF ty_out.

  TYPES: ty_out_tab TYPE STANDARD TABLE OF ty_out WITH DEFAULT KEY.

  DATA: lt_cons_result  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_tab,
        lt_header_lines TYPE salv_wd_t_string,
        ls_header_info  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_header_str,

        ls_res          TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_str,
        lt_clas_result  TYPE ty_clas_chk_result_tab,
        ls_clas_result  TYPE ty_clas_chk_result_str,

        lt_out          TYPE ty_out_tab,
        ls_out          TYPE ty_out.

  DATA: lv_line   TYPE string,
        lv_msg    TYPE string,
        lv_rc_int TYPE i,
        lv_idx    TYPE i VALUE 0.

  DATA lx_root TYPE REF TO cx_root.

  /sdf/cl_rc_chk_utility=>sitem_consistency_result_get(
    EXPORTING
      iv_target_stack      = lv_stack
    IMPORTING
      et_cons_chk_result   = lt_cons_result
      et_cons_header_info  = lt_header_lines
      es_header_info       = ls_header_info ).

  IF lt_cons_result IS INITIAL AND lt_header_lines IS INITIAL.
    ls_output-stack       = lv_stack_string.
    ls_output-filecontent = ''.
    ls_output-filename    = |consistency_error_{ sy-datum }_{ sy-uzeit }.xlsx|.
    ls_output-filetype    = 'XLSX'.
    ls_output-message     = |No consistency check result found for stack { lv_stack_string }.|.
    er_entity             = ls_output.
    RETURN.
  ENDIF.

*=====================================================================*
* 2. Flatten data like in the report
*=====================================================================*

*-- 2A. Overall header lines (source = 'O')
  LOOP AT lt_header_lines INTO lv_line.
    CONDENSE lv_line.
    IF lv_line IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR ls_out.
    ls_out-source     = 'O'.
    ls_out-msg_text   = lv_line.
    ls_out-status     = 'G'.
    ls_out-status_txt = 'Info'.

    IF lv_line CS 'not implemented'
    OR lv_line CS 'error'
    OR lv_line CS 'failed'.
      ls_out-status     = 'R'.
      ls_out-status_txt = 'Error'.
    ELSEIF lv_line CS 'skipped'
       OR lv_line CS 'no consistency check defined'.
      ls_out-status     = 'Y'.
      ls_out-status_txt = 'Warning'.
    ENDIF.

    APPEND ls_out TO lt_out.
  ENDLOOP.

*-- 2B. Per-item details (HEADER_INFO_TABLE + XML)
  LOOP AT lt_cons_result INTO ls_res.

*   Header info table (source = 'H')
    IF ls_res-header_info_table IS NOT INITIAL.
      LOOP AT ls_res-header_info_table ASSIGNING FIELD-SYMBOL(<ls_nv>).

        CLEAR ls_out.
        ls_out-source       = 'H'.
        ls_out-sitem_guid   = ls_res-sitem_guid.
        ls_out-sitem_id     = ls_res-sitem_id.
        ls_out-check_class  = ls_res-check_class.
        ls_out-sap_note     = ls_res-sap_note.
        ls_out-return_code  = ls_res-return_code.
        ls_out-check_sub_id = <ls_nv>-name.
        ls_out-msg_text     = <ls_nv>-value.

        lv_rc_int = ls_res-return_code.
        IF lv_rc_int IS INITIAL.
          ls_out-status     = 'G'.
          ls_out-status_txt = 'Success'.
        ELSEIF lv_rc_int <= 4.
          ls_out-status     = 'Y'.
          ls_out-status_txt = 'Warning'.
        ELSE.
          ls_out-status     = 'R'.
          ls_out-status_txt = 'Error'.
        ENDIF.

        APPEND ls_out TO lt_out.
      ENDLOOP.
    ENDIF.

*   XML sub-checks (source = 'X')
    IF ls_res-chk_clas_result_xstr IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR lt_clas_result.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML ls_res-chk_clas_result_xstr
          RESULT clas_chk_result = lt_clas_result.
      CATCH cx_root INTO lx_root.
        CONTINUE.
    ENDTRY.

    LOOP AT lt_clas_result INTO ls_clas_result.

      lv_rc_int = ls_clas_result-return_code.

      LOOP AT ls_clas_result-descriptions INTO lv_msg.
        CONDENSE lv_msg.
        IF lv_msg IS INITIAL.
          CONTINUE.
        ENDIF.

        CLEAR ls_out.
        ls_out-source       = 'X'.
        ls_out-sitem_guid   = ls_res-sitem_guid.
        ls_out-sitem_id     = ls_res-sitem_id.
        ls_out-check_class  = ls_res-check_class.
        ls_out-sap_note     = ls_res-sap_note.
        ls_out-return_code  = lv_rc_int.
        ls_out-check_sub_id = ls_clas_result-check_sub_id.
        ls_out-msg_text     = lv_msg.

        IF lv_rc_int IS INITIAL.
          ls_out-status     = 'G'.
          ls_out-status_txt = 'Success'.
        ELSEIF lv_rc_int <= 4.
          ls_out-status     = 'Y'.
          ls_out-status_txt = 'Warning'.
        ELSE.
          ls_out-status     = 'R'.
          ls_out-status_txt = 'Error'.
        ENDIF.

        APPEND ls_out TO lt_out.
      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

*-- 2C. Assign SR_NO
  lv_idx = 0.
  LOOP AT lt_out INTO ls_out.
    lv_idx       = lv_idx + 1.
    ls_out-sr_no = lv_idx.
    MODIFY lt_out FROM ls_out.
  ENDLOOP.

*=====================================================================*
* 3. Create XLSX from LT_OUT (no GUI, only xstring)
*=====================================================================*
  DATA: lo_alv  TYPE REF TO cl_salv_table,
        lv_xlsx TYPE xstring.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_out ).

      lv_xlsx = lo_alv->to_xml( if_salv_bs_xml=>c_type_xlsx ).

    CATCH cx_salv_msg INTO lx_root.
      ls_output-stack       = lv_stack_string.
      ls_output-filecontent = ''.
      ls_output-filename    = |consistency_error_{ sy-datum }_{ sy-uzeit }.xlsx|.
      ls_output-filetype    = 'XLSX'.
      ls_output-message     = lx_root->get_text( ).
      er_entity             = ls_output.
      RETURN.
  ENDTRY.

*=====================================================================*
* 4. Encode to Base64 and return in the same entity
*=====================================================================*
  DATA(lv_base64) = cl_http_utility=>encode_x_base64( lv_xlsx ).

  ls_output-stack       = lv_stack_string.
  ls_output-filecontent = lv_base64.
  ls_output-filename    = |Consistency_Check_{ sy-datum }_{ sy-uzeit }.xlsx|.
  ls_output-filetype    = 'XLSX'.
  ls_output-message     = |Success: { lines( lt_out ) } rows written.|.

  er_entity = ls_output.

ENDMETHOD.
ENDCLASS.
