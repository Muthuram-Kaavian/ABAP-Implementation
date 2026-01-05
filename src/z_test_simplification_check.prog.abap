*&---------------------------------------------------------------------*
*& Report Z_TEST_SIMPLIFICATION_CHECK
*&---------------------------------------------------------------------*
REPORT z_test_simplification_check.

TYPE-POOLS: vrm. " Required for Dropdown List

* 1. Define Data based on your Class Definition
DATA: lt_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
      ls_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str,
      lv_selected_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

* Result Data
DATA: lt_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
      ls_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str.

* Output Structure
TYPES: BEGIN OF ty_output,
         relevance        TYPE char4,   " Icon
         id               TYPE string,
         title            TYPE string,
         lob              TYPE string,
         business_area    TYPE string,
         note             TYPE string,
         category         TYPE string,
         component        TYPE string,
         status           TYPE string,  " Text Status
         application_area TYPE string,
         summary          TYPE string,
         consistency      TYPE char4,   " Icon
       END OF ty_output.

DATA: lt_output TYPE TABLE OF ty_output,
      ls_output TYPE ty_output.

*--------------------------------------------------------------------*
* SELECTION SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  " Defined as Listbox for Dropdown
  PARAMETERS: p_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr AS LISTBOX VISIBLE LENGTH 80 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
* INITIALIZATION - POPULATE DROPDOWN
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM populate_dropdown.

*--------------------------------------------------------------------*
* MAIN LOGIC
*--------------------------------------------------------------------*
START-OF-SELECTION.

  " 1. Use the selected value from Dropdown
  lv_selected_stack = p_stack.

  IF lv_selected_stack IS INITIAL.
    WRITE: / 'Error: No Stack Selected.'.
    RETURN.
  ENDIF.

  " 2. FETCH THE RESULTS
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
    EXPORTING
      iv_target_stack   = lv_selected_stack
    IMPORTING
      et_rel_chk_result = lt_check_result.

  IF lt_check_result IS INITIAL.
    WRITE: / 'No results found. Please ensure the standard report /SDF/RC_START_CHECK has been run manually for this stack at least once.'.
    RETURN.
  ENDIF.

  " 3. PREPARE OUTPUT
  LOOP AT lt_check_result INTO ls_check_result.
    CLEAR ls_output.

    " --- Basic Info ---
    ls_output-id            = ls_check_result-sitem_id.
    ls_output-title         = ls_check_result-title_en.
    ls_output-business_area = ls_check_result-business_area_des.

    " --- LoB Logic ---
    IF ls_check_result-lob_technology_des IS NOT INITIAL.
      ls_output-lob = ls_check_result-lob_technology_des.
    ELSE.
      ls_output-lob = ls_check_result-app_area.
    ENDIF.

    " --- New Fields Mapping ---
    ls_output-note             = ls_check_result-buz_imp_note.
    ls_output-category         = ls_check_result-category_text.
    ls_output-component        = ls_check_result-app_components.
    ls_output-status           = ls_check_result-proc_status.
    ls_output-application_area = ls_check_result-app_area.
    ls_output-summary          = ls_check_result-summary.

    " --- Icons ---
    IF ls_check_result-relevant_stat IS NOT INITIAL.
       ls_output-relevance = ls_check_result-relevant_stat.
    ELSE.
       ls_output-relevance = icon_light_out.
    ENDIF.

    ls_output-consistency = ls_check_result-consistency_stat_disp(4).

    APPEND ls_output TO lt_output.
  ENDLOOP.

  " 4. DOWNLOAD TO EXCEL (LOCAL FILE)
  PERFORM download_to_excel.

*&---------------------------------------------------------------------*
*&      Form  POPULATE_DROPDOWN
*&---------------------------------------------------------------------*
FORM populate_dropdown.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  " Get Available Stacks
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
*&      Form  DOWNLOAD_TO_EXCEL
*&---------------------------------------------------------------------*
FORM download_to_excel.
  DATA: lv_fullpath TYPE string,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lt_excel    TYPE TABLE OF string,
        lv_row      TYPE string,
        lv_tab      TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

  " 1. Create Header Row
  CONCATENATE 'RELEVANCE_STATUS' 'ID' 'TITLE' 'LOB' 'BUSINESS_AREA' 'NOTE' 'CATEGORY'
              'COMPONENT' 'STATUS_TEXT' 'APPLICATION_AREA' 'SUMMARY' 'CONSISTENCY_STATUS'
    INTO lv_row SEPARATED BY lv_tab.
  APPEND lv_row TO lt_excel.

  " 2. Create Data Rows
  LOOP AT lt_output INTO ls_output.

    " REQUEST: Keep the code itself (e.g. @5D@), remove text translation.
    " We directly concatenate the fields holding the icon codes.

    CONCATENATE ls_output-relevance    " Icon Code (e.g. @5D@)
                ls_output-id
                ls_output-title
                ls_output-lob
                ls_output-business_area
                ls_output-note
                ls_output-category
                ls_output-component
                ls_output-status
                ls_output-application_area
                ls_output-summary
                ls_output-consistency  " Icon Code (e.g. @01@)
      INTO lv_row SEPARATED BY lv_tab.
    APPEND lv_row TO lt_excel.
  ENDLOOP.

  " 3. Save Dialog
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      window_title      = 'Save Check Results'
      default_extension = 'xls'
      default_file_name = 'S4_Simplification_Results.xls'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath ).

  IF lv_fullpath IS INITIAL.
    RETURN. " User cancelled
  ENDIF.

  " 4. Trigger Download
  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                = lv_fullpath
      filetype                = 'ASC'
    CHANGING
      data_tab                = lt_excel
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      others                  = 24 ).

  CASE sy-subrc.
    WHEN 0.
      WRITE: / 'File downloaded successfully to:', lv_fullpath.
    WHEN 15.
      WRITE: / 'Error: Access Denied (RC=15). File might be open or folder restricted.'.
    WHEN OTHERS.
      WRITE: / 'Error downloading file. Return code:', sy-subrc.
  ENDCASE.

ENDFORM.
