REPORT z_sic_applog_export.

*---------------------------------------------------------------------*
* 1. Types & data declarations
*---------------------------------------------------------------------*

" One output line per detailed message
TYPES: BEGIN OF ty_out,
         sitem_guid  TYPE guid_32,
         sitem_id    TYPE /sdf/cl_rc_chk_utility=>ty_smdb_title,
         check_class TYPE /sdf/cl_rc_chk_utility=>ty_check_id,
         sap_note    TYPE string,
         return_code TYPE /sdf/cl_rc_chk_utility=>ty_return_code,
         line_no     TYPE i,
         line_name   TYPE string,
         line_value  TYPE string,   "this is the actual text you want
       END OF ty_out.

DATA: gv_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

DATA: gt_cons_result  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_tab,
      gt_header_lines TYPE salv_wd_t_string,
      gs_header_info  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_header_str.

DATA: gt_out TYPE STANDARD TABLE OF ty_out,
      gs_out TYPE ty_out.

DATA: gv_line TYPE string.

*---------------------------------------------------------------------*
* 2. Selection screen
*---------------------------------------------------------------------*
PARAMETERS p_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr OBLIGATORY.

*---------------------------------------------------------------------*
* 3. Start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.

  gv_stack = p_stack.

  CLEAR: gt_cons_result, gt_header_lines, gs_header_info.

  "--- Get stored consistency-check result ----------------------------
  TRY.
      CALL METHOD /sdf/cl_rc_chk_utility=>sitem_consistency_result_get
        EXPORTING
          iv_target_stack      = gv_stack
        IMPORTING
          et_cons_chk_result   = gt_cons_result
          et_cons_header_info  = gt_header_lines
          es_header_info       = gs_header_info.
    CATCH cx_root INTO DATA(lx_any).
      MESSAGE lx_any->get_text( ) TYPE 'E'.
  ENDTRY.

  IF gt_cons_result IS INITIAL AND gt_header_lines IS INITIAL.
    MESSAGE 'No consistency check result found for selected target stack'
      TYPE 'I'.
    EXIT.
  ENDIF.

*---------------------------------------------------------------------*
* 4. Overview text (same as consistency result header in GUI)
*---------------------------------------------------------------------*
  WRITE: / 'Consistency check result for target stack:', gv_stack.
  ULINE.

  LOOP AT gt_header_lines INTO gv_line.
    WRITE: / gv_line.
  ENDLOOP.

  SKIP 2.

*---------------------------------------------------------------------*
* 5. Flatten HEADER_INFO_TABLE for each check into GT_OUT
*---------------------------------------------------------------------*
  CLEAR gt_out.

  LOOP AT gt_cons_result ASSIGNING FIELD-SYMBOL(<ls_res>).

    DATA(lv_idx) = 0.

    "If there are no details, still put one summary line
    IF <ls_res>-header_info_table IS INITIAL.

      CLEAR gs_out.
      gs_out-sitem_guid  = <ls_res>-sitem_guid.
      gs_out-sitem_id    = <ls_res>-sitem_id.
      gs_out-check_class = <ls_res>-check_class.
      gs_out-sap_note    = <ls_res>-sap_note.
      gs_out-return_code = <ls_res>-return_code.
      gs_out-line_no     = 0.
      gs_out-line_name   = ''.
      gs_out-line_value  = 'No detailed messages available.'.
      APPEND gs_out TO gt_out.

    ELSE.

      "TIHTTPNVP has fields NAME and VALUE
      LOOP AT <ls_res>-header_info_table ASSIGNING FIELD-SYMBOL(<ls_nv>).

        lv_idx = lv_idx + 1.
        CLEAR gs_out.

        gs_out-sitem_guid  = <ls_res>-sitem_guid.
        gs_out-sitem_id    = <ls_res>-sitem_id.
        gs_out-check_class = <ls_res>-check_class.
        gs_out-sap_note    = <ls_res>-sap_note.
        gs_out-return_code = <ls_res>-return_code.

        gs_out-line_no     = lv_idx.
        gs_out-line_name   = <ls_nv>-name.
        gs_out-line_value  = <ls_nv>-value.

        APPEND gs_out TO gt_out.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

*---------------------------------------------------------------------*
* 6. Display detailed messages in ALV
*---------------------------------------------------------------------*
  DATA lo_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_out ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).

      " You probably care most about LINE_VALUE – you can drag it wider in ALV
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
  ENDTRY.
