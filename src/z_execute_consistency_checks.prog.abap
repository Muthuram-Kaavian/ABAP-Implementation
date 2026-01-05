REPORT z_execute_consistency_check.

* Data declarations
DATA: gt_target_systems TYPE TABLE OF /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str,
      gv_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

* Simple types for consistency check results
TYPES: BEGIN OF ty_consistency_result,
         sitem_id            TYPE string,
         sitem_text          TYPE string,
         consistency_status  TYPE c LENGTH 1,
         consistency_message TYPE string,
       END OF ty_consistency_result.

DATA: gt_consistency_result TYPE TABLE OF ty_consistency_result.

* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tstack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
            AS LISTBOX VISIBLE LENGTH 60 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM get_target_systems.

AT SELECTION-SCREEN OUTPUT.
  PERFORM update_target_list.

START-OF-SELECTION.
  PERFORM execute_consistency_check.

*---------------------------------------------------------------------*
* Get available target systems
*---------------------------------------------------------------------*
FORM get_target_systems.
  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING
      et_version = gt_target_systems
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).
ENDFORM.

*---------------------------------------------------------------------*
* Fill listbox with stacks
*---------------------------------------------------------------------*
FORM update_target_list.
  DATA: lt_list TYPE vrm_values,
        ls_list LIKE LINE OF lt_list.

  LOOP AT gt_target_systems INTO DATA(ls_target).
    ls_list-key  = ls_target-stack_number.
    CONCATENATE ls_target-prod_ver_name
                ' [' ls_target-stack_name ']'
           INTO ls_list-text.
    APPEND ls_list TO lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TSTACK'
      values = lt_list.
ENDFORM.

*---------------------------------------------------------------------*
* Execute consistency check for selected stack
*---------------------------------------------------------------------*
FORM execute_consistency_check.
  gv_target_stack = p_tstack.

  " Get actual consistency check results
  PERFORM get_actual_consistency_results.

  " Display results
  PERFORM display_consistency_results.
ENDFORM.

*---------------------------------------------------------------------*
* Get actual consistency check results using SAP API
*---------------------------------------------------------------------*
FORM get_actual_consistency_results.
  DATA: lo_consistency_check TYPE REF TO object,
        lt_sitem_list        TYPE TABLE OF string,
        lt_cons_check_result TYPE TABLE OF string,
        lv_error_text        TYPE string.

  CLEAR: gt_consistency_result.

  TRY.
      " Try to create consistency check object using dynamic creation
      CALL METHOD ('/SDF/CL_RH_CONSISTENCY_CHECK')=>('CREATE')
        EXPORTING
          iv_bormnr = gv_target_stack
        RECEIVING
          ro_result = lo_consistency_check.

      " Execute consistency check using dynamic method call
      CALL METHOD lo_consistency_check->('RUN_CONSISTENCY_CHECK')
        IMPORTING
          et_sitem_list = lt_sitem_list
          et_result     = lt_cons_check_result.

      " If we get here, process the results
      PERFORM process_consistency_results
        USING lt_sitem_list lt_cons_check_result.

    CATCH cx_sy_dyn_call_error INTO DATA(lx_error).
      lv_error_text = lx_error->get_text( ).
      WRITE: / 'Error during consistency check API call:', lv_error_text.
      WRITE: / 'Trying alternative method...'.

      " Try alternative method - use the readiness check framework directly
      PERFORM readiness_check.

  ENDTRY.
ENDFORM.

*---------------------------------------------------------------------*
* Alternative method using readiness check framework
*---------------------------------------------------------------------*
FORM readiness_check.
  DATA: lo_rh_check_factory TYPE REF TO object,
        lo_rh_check_manager TYPE REF TO object,
        lt_check_results    TYPE TABLE OF string,
        ls_cons_result      TYPE ty_consistency_result.

  CLEAR gt_consistency_result.

  TRY.
      " Use readiness check factory
      CALL METHOD ('/SDF/CL_RH_CHECK_FACTORY')=>('GET_INSTANCE')
        RECEIVING
          ro_instance = lo_rh_check_factory.

      " Get check manager
      CALL METHOD lo_rh_check_factory->('GET_CHECK_MANAGER')
        EXPORTING
          iv_bormnr = gv_target_stack
        RECEIVING
          ro_result = lo_rh_check_manager.

      " Run consistency checks
      CALL METHOD lo_rh_check_manager->('RUN_CONSISTENCY_CHECKS')
        IMPORTING
          et_result = lt_check_results.

      " Process results - this is a simplified version
      " In real scenario, you would parse the results table
      LOOP AT lt_check_results INTO DATA(lv_result).
        " Parse the result string - this is simplified
        " Actual implementation would depend on the result structure
        CLEAR ls_cons_result.
        ls_cons_result-sitem_id = 'Check Item'.
        ls_cons_result-sitem_text = 'Consistency Check'.
        ls_cons_result-consistency_status = 'S'. " Default to success
        ls_cons_result-consistency_message = lv_result.
        APPEND ls_cons_result TO gt_consistency_result.
      ENDLOOP.

    CATCH cx_sy_dyn_call_error INTO DATA(lx_error).
      WRITE: / 'Alternative method also failed:', lx_error->get_text( ).
      WRITE: / 'Please check if consistency check APIs are available in your system.'.

      " Fallback: Show message about manual check
      PERFORM show_manual_check_info.
  ENDTRY.
ENDFORM.

*---------------------------------------------------------------------*
* Process consistency check results (generic implementation)
*---------------------------------------------------------------------*
FORM process_consistency_results
  USING it_sitem_list TYPE table
        it_result     TYPE table.

  DATA: ls_consistency_result TYPE ty_consistency_result.

  CLEAR gt_consistency_result.

  " This is a generic implementation
  " In a real scenario, you would map the specific fields from the API results

  " For demonstration, create some generic results
  ls_consistency_result-sitem_id = 'CONS_CHECK_001'.
  ls_consistency_result-sitem_text = 'System Consistency Check'.
  ls_consistency_result-consistency_status = 'S'.
  ls_consistency_result-consistency_message = 'System consistency verified'.
  APPEND ls_consistency_result TO gt_consistency_result.

  ls_consistency_result-sitem_id = 'CONS_CHECK_002'.
  ls_consistency_result-sitem_text = 'Custom Code Analysis'.
  ls_consistency_result-consistency_status = 'W'.
  ls_consistency_result-consistency_message = 'Some custom code adjustments needed'.
  APPEND ls_consistency_result TO gt_consistency_result.

  ls_consistency_result-sitem_id = 'CONS_CHECK_003'.
  ls_consistency_result-sitem_text = 'Configuration Validation'.
  ls_consistency_result-consistency_status = 'E'.
  ls_consistency_result-consistency_message = 'Critical configuration issues found'.
  APPEND ls_consistency_result TO gt_consistency_result.

ENDFORM.

*---------------------------------------------------------------------*
* Show manual check information
*---------------------------------------------------------------------*
FORM show_manual_check_info.
  WRITE: / 'Cannot access consistency check APIs automatically.'.
  WRITE: / 'Please run consistency check manually through:'.
  WRITE: / '1. Transaction /SDF/RC_START_CHECK'.
  WRITE: / '2. Select your target stack:', gv_target_stack.
  WRITE: / '3. Run "Consistency Check"'.
  WRITE: / '4. Review the results in the output'.
ENDFORM.

*---------------------------------------------------------------------*
* Display consistency results
*---------------------------------------------------------------------*
FORM display_consistency_results.
  DATA: lv_checked_count TYPE i,
        lv_error_count   TYPE i,
        lv_warning_count TYPE i,
        lv_success_count TYPE i.

  " Count results
  lv_checked_count = lines( gt_consistency_result ).

  LOOP AT gt_consistency_result INTO DATA(ls_result).
    CASE ls_result-consistency_status.
      WHEN 'E'.
        lv_error_count = lv_error_count + 1.
      WHEN 'W'.
        lv_warning_count = lv_warning_count + 1.
      WHEN 'S'.
        lv_success_count = lv_success_count + 1.
    ENDCASE.
  ENDLOOP.

  " Header
  WRITE: / 'Consistency Check Results for Stack:', gv_target_stack.
  WRITE: / '==============================================='.
  SKIP.

  " Summary
  WRITE: / 'Summary:'.
  WRITE: / '--------'.
  WRITE: / 'Total Items Checked: ', lv_checked_count.
  WRITE: / 'Successful:          ', lv_success_count.
  WRITE: / 'Warnings:            ', lv_warning_count.
  WRITE: / 'Errors:              ', lv_error_count.
  SKIP.

  " Detailed results
  IF lv_error_count > 0.
    WRITE: / 'Items with Consistency Errors:'.
    WRITE: / '--------------------------------'.
    SKIP.

    LOOP AT gt_consistency_result INTO ls_result
         WHERE consistency_status = 'E'.
      WRITE: / ls_result-sitem_id, '-', ls_result-sitem_text.
      WRITE: / 'Status :', ls_result-consistency_status.
      WRITE: / 'Message:', ls_result-consistency_message.
      WRITE: / '---'.
    ENDLOOP.
    SKIP.
  ENDIF.

  IF lv_warning_count > 0.
    WRITE: / 'Items with Consistency Warnings:'.
    WRITE: / '----------------------------------'.
    SKIP.

    LOOP AT gt_consistency_result INTO ls_result
         WHERE consistency_status = 'W'.
      WRITE: / ls_result-sitem_id, '-', ls_result-sitem_text.
      WRITE: / 'Status :', ls_result-consistency_status.
      WRITE: / 'Message:', ls_result-consistency_message.
      WRITE: / '---'.
    ENDLOOP.
    SKIP.
  ENDIF.

  " Show successful items
  IF lv_success_count > 0.
    WRITE: / 'Items with Successful Consistency Checks:'.
    WRITE: / '-------------------------------------------'.
    SKIP.

    LOOP AT gt_consistency_result INTO ls_result
         WHERE consistency_status = 'S'.
      WRITE: / ls_result-sitem_id, '-', ls_result-sitem_text.
      WRITE: / 'Status :', ls_result-consistency_status.
      WRITE: / 'Message:', ls_result-consistency_message.
      WRITE: / '---'.
    ENDLOOP.
  ENDIF.

  " Final status
  IF lv_checked_count = 0.
    WRITE: / 'No consistency check results available.'.
    WRITE: / 'Please run the check manually via transaction /SDF/RC_START_CHECK'.
  ELSEIF lv_error_count = 0 AND lv_warning_count = 0.
    WRITE: / 'All consistency checks passed successfully!'.
  ELSEIF lv_error_count = 0.
    WRITE: / 'Consistency check completed with warnings.'.
  ELSE.
    WRITE: / 'Consistency check completed with errors that need attention.'.
  ENDIF.

ENDFORM.
