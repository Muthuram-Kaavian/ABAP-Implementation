REPORT z_odata_deimplement_check1.
*---------------------------------------------------------------------*
* 1. Types
*---------------------------------------------------------------------*
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
*---------------------------------------------------------------------*
* 2. Data declarations
*---------------------------------------------------------------------*
PARAMETERS p_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr OBLIGATORY.
DATA: gt_cons_result  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_tab,
      gt_header_lines TYPE salv_wd_t_string,
      gs_header_info  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_header_str.
DATA: ls_res          TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_str.
DATA: lt_clas_result  TYPE ty_clas_chk_result_tab,
      ls_clas_result  TYPE ty_clas_chk_result_str.
DATA: gt_out          TYPE ty_out_tab,
      gs_out          TYPE ty_out.
DATA: lv_rc_int       TYPE i.
DATA: lv_msg          TYPE string.
DATA: lx_root         TYPE REF TO cx_root.
DATA: lo_alv          TYPE REF TO cl_salv_table.
DATA: gv_line         TYPE string.
*---------------------------------------------------------------------*
* 3. Status helper
*---------------------------------------------------------------------*
FORM get_status_from_rc USING    iv_rc         TYPE i
                        CHANGING ev_status     TYPE c
                                 ev_status_txt TYPE string.
  IF iv_rc IS INITIAL.
    ev_status     = 'G'.
    ev_status_txt = 'Success'.
  ELSEIF iv_rc <= 4.
    ev_status     = 'Y'.
    ev_status_txt = 'Warning'.
  ELSE.
    ev_status     = 'R'.
    ev_status_txt = 'Error'.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
* 4. Start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: gt_cons_result, gt_header_lines, gs_header_info.
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_consistency_result_get
    EXPORTING
      iv_target_stack      = p_stack
    IMPORTING
      et_cons_chk_result   = gt_cons_result
      et_cons_header_info  = gt_header_lines
      es_header_info       = gs_header_info.
  IF gt_cons_result IS INITIAL AND gt_header_lines IS INITIAL.
    WRITE: / 'No consistency check result found for this target stack.'.
    RETURN.
  ENDIF.
  CLEAR gt_out.
*---------------------------------------------------------------------*
* 4A. Overall header lines
*---------------------------------------------------------------------*
  LOOP AT gt_header_lines INTO gv_line.
    CONDENSE gv_line.
    IF gv_line IS INITIAL.
      CONTINUE.
    ENDIF.
    CLEAR gs_out.
    gs_out-source       = 'O'.
    gs_out-msg_text     = gv_line.
    gs_out-status       = 'G'.
    gs_out-status_txt   = 'Info'.
    IF gv_line CS 'not implemented'
    OR gv_line CS 'error'
    OR gv_line CS 'failed'.
      gs_out-status     = 'R'.
      gs_out-status_txt = 'Error'.
    ELSEIF gv_line CS 'skipped'
    OR gv_line CS 'no consistency check defined'.
      gs_out-status     = 'Y'.
      gs_out-status_txt = 'Warning'.
    ENDIF.
    APPEND gs_out TO gt_out.
  ENDLOOP.
*---------------------------------------------------------------------*
* 4B. Per-item details (header + XML)
*---------------------------------------------------------------------*
  LOOP AT gt_cons_result INTO ls_res.
*-- HEADER INFO
    IF ls_res-header_info_table IS NOT INITIAL.
      LOOP AT ls_res-header_info_table ASSIGNING FIELD-SYMBOL(<ls_nv>).
        CLEAR gs_out.
        gs_out-source       = 'H'.
        gs_out-sitem_guid   = ls_res-sitem_guid.
        gs_out-sitem_id     = ls_res-sitem_id.
        gs_out-check_class  = ls_res-check_class.
        gs_out-sap_note     = ls_res-sap_note.
        gs_out-return_code  = ls_res-return_code.
        gs_out-check_sub_id = <ls_nv>-name.
        gs_out-msg_text     = <ls_nv>-value.
        PERFORM get_status_from_rc
          USING    ls_res-return_code
          CHANGING gs_out-status
                   gs_out-status_txt.
        APPEND gs_out TO gt_out.
      ENDLOOP.
    ENDIF.
*-- XML DETAILS
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
        CLEAR gs_out.
        gs_out-source       = 'X'.
        gs_out-sitem_guid   = ls_res-sitem_guid.
        gs_out-sitem_id     = ls_res-sitem_id.
        gs_out-check_class  = ls_res-check_class.
        gs_out-sap_note     = ls_res-sap_note.
        gs_out-return_code  = lv_rc_int.
        gs_out-check_sub_id = ls_clas_result-check_sub_id.
        gs_out-msg_text     = lv_msg.
        PERFORM get_status_from_rc
          USING    lv_rc_int
          CHANGING gs_out-status
                   gs_out-status_txt.
        APPEND gs_out TO gt_out.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
*---------------------------------------------------------------------*
* 4C. Assign S.No
*---------------------------------------------------------------------*
  DATA lv_idx TYPE i VALUE 0.
  LOOP AT gt_out ASSIGNING FIELD-SYMBOL(<ls_out>).
    lv_idx = lv_idx + 1.
<ls_out>-sr_no = lv_idx.
  ENDLOOP.
*---------------------------------------------------------------------*
* 5. EXPORT TO EXCEL (XLSX) — WITH SAVE-AS POPUP
*---------------------------------------------------------------------*
DATA: lv_xml      TYPE xstring,
      lt_bin      TYPE solix_tab,
      lv_fullpath TYPE string,
      lv_path     TYPE string,
      lv_fname    TYPE string VALUE 'Odata_Deimplementation_Report.xlsx'.
" Show Save-As popup
cl_gui_frontend_services=>file_save_dialog(
  EXPORTING
    default_extension = 'xlsx'
    default_file_name = lv_fname
  CHANGING
    filename          = lv_fname
    fullpath          = lv_fullpath
    path              = lv_path
  EXCEPTIONS
    others = 1 ).
IF lv_fullpath IS NOT INITIAL.
  " Prepare XML for XLSX
  TRY.
      " Create ALV just to extract XLSX
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_out ).
      lv_xml = lo_alv->to_xml( if_salv_bs_xml=>c_type_xlsx ).
      lt_bin = cl_bcs_convert=>xstring_to_solix( lv_xml ).
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename     = lv_fullpath
          filetype     = 'BIN'
          bin_filesize = xstrlen( lv_xml )
        TABLES
          data_tab     = lt_bin.
    CATCH cx_salv_msg INTO lx_root.
      MESSAGE lx_root->get_text( ) TYPE 'I'.
  ENDTRY.
ELSE.
  MESSAGE 'Download cancelled' TYPE 'I'.
ENDIF.
*---------------------------------------------------------------------*
* 6. Display ALV normally
*---------------------------------------------------------------------*
TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_out ).
    lo_alv->get_functions( )->set_all( abap_true ).
    lo_alv->get_columns( )->set_optimize( abap_true ).
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    lo_alv->display( ).
  CATCH cx_salv_msg INTO lx_root.
    WRITE: / lx_root->get_text( ).
ENDTRY.
