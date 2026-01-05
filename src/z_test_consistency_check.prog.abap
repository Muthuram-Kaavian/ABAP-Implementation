*&---------------------------------------------------------------------*
*& Report Z_TEST_CONSISTENCY_CHECK
*&---------------------------------------------------------------------*
REPORT z_test_consistency_check.

TYPE-POOLS: vrm.

* Data declarations
DATA: lt_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
      ls_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str,
      lv_selected_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

* Simplification Check Results
DATA: lt_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
      ls_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str.

* Output Structure for Consistency Overview
TYPES: BEGIN OF ty_overview_output,
         field_name  TYPE string,
         field_value TYPE string,
       END OF ty_overview_output.

* Output Structure for Consistency Details
TYPES: BEGIN OF ty_detail_output,
         title          TYPE string,
         lob_technology TYPE string,
         business_area  TYPE string,
         category       TYPE string,
         sitem_id       TYPE string,
         status         TYPE string,
         return_code    TYPE string,
         note           TYPE string,
         component      TYPE string,
       END OF ty_detail_output.

DATA: lt_overview TYPE TABLE OF ty_overview_output,
      ls_overview TYPE ty_overview_output,
      lt_details  TYPE TABLE OF ty_detail_output,
      ls_detail   TYPE ty_detail_output.

*--------------------------------------------------------------------*
* SELECTION SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS: p_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr AS LISTBOX VISIBLE LENGTH 80 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  PARAMETERS: p_overv TYPE c AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_detls TYPE c AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_onlyc TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

*--------------------------------------------------------------------*
* INITIALIZATION
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM populate_dropdown.

*--------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.

  lv_selected_stack = p_stack.

  IF lv_selected_stack IS INITIAL.
    MESSAGE 'Error: No Stack Selected.' TYPE 'E'.
    RETURN.
  ENDIF.

  " Get Simplification Results
  PERFORM get_simplification_results.

  " Extract Overview Information
  IF p_overv = 'X'.
    PERFORM extract_overview_info.
  ENDIF.

  " Extract Detailed Results
  IF p_detls = 'X'.
    PERFORM extract_detailed_results.
  ENDIF.

  " Download to Excel
  PERFORM download_to_excel.

*&---------------------------------------------------------------------*
*&      Form  POPULATE_DROPDOWN
*&---------------------------------------------------------------------*
FORM populate_dropdown.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  CALL METHOD /sdf/cl_rc_chk_utility=>get_target_s4_version
    IMPORTING
      et_version = lt_target_stack
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_target_stack BY stack_number DESCENDING.

  LOOP AT lt_target_stack INTO ls_target_stack.
    CLEAR ls_value.
    ls_value-key = ls_target_stack-stack_number.
    CONCATENATE ls_target_stack-prod_ver_name
                ' [' ls_target_stack-stack_name ']'
                INTO ls_value-text.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STACK'
      values = lt_values.

  READ TABLE lt_target_stack INTO ls_target_stack INDEX 1.
  IF sy-subrc = 0.
    p_stack = ls_target_stack-stack_number.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_SIMPLIFICATION_RESULTS
*&---------------------------------------------------------------------*
FORM get_simplification_results.
  " Get simplification results
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
    EXPORTING
      iv_target_stack   = lv_selected_stack
    IMPORTING
      et_rel_chk_result = lt_check_result.

  IF lt_check_result IS INITIAL.
    WRITE: / 'No results found. Please run /SDF/RC_START_CHECK first.'.
    RETURN.
  ELSE.
    WRITE: / 'Total simplification items found:', lines( lt_check_result ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_OVERVIEW_INFO
*&---------------------------------------------------------------------*
FORM extract_overview_info.
  " Extract overview information from check results

  " System Information
  ls_overview-field_name = 'System'.
  ls_overview-field_value = sy-sysid.
  APPEND ls_overview TO lt_overview.

  " Target Version
  READ TABLE lt_target_stack INTO ls_target_stack
    WITH KEY stack_number = lv_selected_stack.
  IF sy-subrc = 0.
    ls_overview-field_name = 'Target Version'.
    ls_overview-field_value = ls_target_stack-prod_ver_name.
    APPEND ls_overview TO lt_overview.
  ENDIF.

  " Check Statistics
  DATA: lv_total TYPE i,
        lv_skipped TYPE i,
        lv_irrelevant TYPE i,
        lv_no_check TYPE i,
        lv_checked TYPE i.

  lv_total = lines( lt_check_result ).

  LOOP AT lt_check_result INTO ls_check_result.
    " Count by relevance and consistency status
    IF ls_check_result-consistency_stat_disp IS NOT INITIAL.
      lv_checked = lv_checked + 1.
    ELSE.
      CASE ls_check_result-relevant_stat.
        WHEN '@5D@'. " Relevant - green icon
          lv_no_check = lv_no_check + 1.
        WHEN '@5B@'. " Not relevant - red icon
          lv_irrelevant = lv_irrelevant + 1.
        WHEN OTHERS.
          lv_skipped = lv_skipped + 1.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  " Add statistics to overview
  ls_overview-field_name = 'Total Items'.
  ls_overview-field_value = lv_total.
  APPEND ls_overview TO lt_overview.

  ls_overview-field_name = 'Items Skipped'.
  ls_overview-field_value = lv_skipped.
  APPEND ls_overview TO lt_overview.

  ls_overview-field_name = 'Items Irrelevant'.
  ls_overview-field_value = lv_irrelevant.
  APPEND ls_overview TO lt_overview.

  ls_overview-field_name = 'Items Relevant (No Check)'.
  ls_overview-field_value = lv_no_check.
  APPEND ls_overview TO lt_overview.

  ls_overview-field_name = 'Items Checked for Consistency'.
  ls_overview-field_value = lv_checked.
  APPEND ls_overview TO lt_overview.

  " Get highest return code
  DATA: lv_highest_rc TYPE string.
  LOOP AT lt_check_result INTO ls_check_result
    WHERE consistency_stat_disp IS NOT INITIAL.
    IF ls_check_result-consistency_stat_disp > lv_highest_rc.
      lv_highest_rc = ls_check_result-consistency_stat_disp.
    ENDIF.
  ENDLOOP.

  ls_overview-field_name = 'Highest Return Code'.
  ls_overview-field_value = lv_highest_rc.
  APPEND ls_overview TO lt_overview.

  WRITE: / 'Overview information extracted.'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_DETAILED_RESULTS
*&---------------------------------------------------------------------*
FORM extract_detailed_results.
  DATA: lv_consistency_count TYPE i.

  " Extract detailed consistency check results
  LOOP AT lt_check_result INTO ls_check_result.
    CLEAR ls_detail.

    " Filter logic based on checkbox
    IF p_onlyc = 'X'. " Only items with consistency checks
      IF ls_check_result-consistency_stat_disp IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Map data to detail structure
    ls_detail-title          = ls_check_result-title_en.
    ls_detail-lob_technology = ls_check_result-lob_technology_des.
    ls_detail-business_area  = ls_check_result-business_area_des.
    ls_detail-category       = ls_check_result-category_text.
    ls_detail-sitem_id       = ls_check_result-sitem_id.
    ls_detail-status         = ls_check_result-proc_status.
    ls_detail-return_code    = ls_check_result-consistency_stat_disp.
    ls_detail-note           = ls_check_result-buz_imp_note.
    ls_detail-component      = ls_check_result-app_components.

    APPEND ls_detail TO lt_details.

    IF ls_check_result-consistency_stat_disp IS NOT INITIAL.
      lv_consistency_count = lv_consistency_count + 1.
    ENDIF.
  ENDLOOP.

  " Display summary
  WRITE: / 'Detailed results extracted:'.
  WRITE: / 'Total items:', lines( lt_details ).
  WRITE: / 'Items with consistency checks:', lv_consistency_count.
  WRITE: /.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_EXCEL
*&---------------------------------------------------------------------*
FORM download_to_excel.
  DATA: lv_fullpath TYPE string,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lt_excel    TYPE TABLE OF string,
        lv_row      TYPE string,
        lv_tab      TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

  IF lt_overview IS INITIAL AND lt_details IS INITIAL.
    MESSAGE 'No data to download' TYPE 'S'.
    RETURN.
  ENDIF.

  " Create Excel file with both overview and details

  " 1. OVERVIEW SHEET
  IF p_overv = 'X' AND lt_overview IS NOT INITIAL.
    APPEND 'CONSISTENCY CHECK OVERVIEW' TO lt_excel.
    APPEND '' TO lt_excel.

    " Overview Header
    CONCATENATE 'FIELD_NAME' 'FIELD_VALUE' INTO lv_row SEPARATED BY lv_tab.
    APPEND lv_row TO lt_excel.

    " Overview Data
    LOOP AT lt_overview INTO ls_overview.
      CONCATENATE ls_overview-field_name
                  ls_overview-field_value
        INTO lv_row SEPARATED BY lv_tab.
      APPEND lv_row TO lt_excel.
    ENDLOOP.

    APPEND '' TO lt_excel.
    APPEND '' TO lt_excel.
  ENDIF.

  " 2. DETAILED RESULTS SHEET
  IF p_detls = 'X' AND lt_details IS NOT INITIAL.
    APPEND 'CONSISTENCY CHECK DETAILS' TO lt_excel.
    APPEND '' TO lt_excel.

    " Details Header
    CONCATENATE 'TITLE' 'LOB_TECHNOLOGY' 'BUSINESS_AREA' 'CATEGORY'
                'ITEM_ID' 'STATUS' 'RETURN_CODE' 'NOTE' 'COMPONENT'
      INTO lv_row SEPARATED BY lv_tab.
    APPEND lv_row TO lt_excel.

    " Details Data
    LOOP AT lt_details INTO ls_detail.
      CONCATENATE ls_detail-title
                  ls_detail-lob_technology
                  ls_detail-business_area
                  ls_detail-category
                  ls_detail-sitem_id
                  ls_detail-status
                  ls_detail-return_code
                  ls_detail-note
                  ls_detail-component
        INTO lv_row SEPARATED BY lv_tab.
      APPEND lv_row TO lt_excel.
    ENDLOOP.
  ENDIF.

  " Save Dialog
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      window_title      = 'Save Consistency Check Results'
      default_extension = 'xls'
      default_file_name = 'S4_Consistency_Full_Results.xls'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath ).

  IF lv_fullpath IS INITIAL.
    MESSAGE 'Download cancelled by user' TYPE 'S'.
    RETURN.
  ENDIF.

  " Trigger Download
  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                = lv_fullpath
      filetype                = 'ASC'
      codepage                = '4110'
    CHANGING
      data_tab                = lt_excel
    EXCEPTIONS
      OTHERS                  = 1 ).

  IF sy-subrc = 0.
    MESSAGE 'Consistency check results downloaded successfully' TYPE 'S'.
    WRITE: / 'File downloaded to:', lv_fullpath.

    " Display what was downloaded
    IF p_overv = 'X'.
      WRITE: / 'Overview records:', lines( lt_overview ).
    ENDIF.
    IF p_detls = 'X'.
      WRITE: / 'Detail records:', lines( lt_details ).
    ENDIF.
  ELSE.
    MESSAGE 'Error downloading file' TYPE 'E'.
  ENDIF.
ENDFORM.
