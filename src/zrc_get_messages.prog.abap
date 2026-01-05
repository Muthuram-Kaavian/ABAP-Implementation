*&---------------------------------------------------------------------*
*& Report ZRC_GET_MESSAGES
*&---------------------------------------------------------------------*
REPORT zrc_get_messages.

* Use the types from the standard include
INCLUDE /sdf/rc_start_check_variable.
DATA: gt_target_systems TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_all  TYPE abap_bool RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_spec TYPE abap_bool RADIOBUTTON GROUP gr1. " Changed from p_specific
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_tstack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
            AS LISTBOX VISIBLE LENGTH 60.
SELECTION-SCREEN END OF BLOCK b2.
INITIALIZATION.
  PERFORM initialize_program.
AT SELECTION-SCREEN OUTPUT.
  PERFORM update_target_system_list.
START-OF-SELECTION.
  PERFORM execute_consistency_checks.
FORM initialize_program.
  " Get available target systems
  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING
      et_version = gt_target_systems
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).
ENDFORM.
FORM update_target_system_list.
  DATA: lt_list TYPE vrm_values,
        ls_list LIKE LINE OF lt_list.
  " Prepare dropdown list
  LOOP AT gt_target_systems INTO DATA(ls_target).
    ls_list-key  = ls_target-stack_number.
    CONCATENATE ls_target-prod_ver_name
                ' [' ls_target-stack_name ']'
                INTO ls_list-text.
    APPEND ls_list TO lt_list.
  ENDLOOP.
  " Set dropdown values
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TSTACK'
      values = lt_list.
ENDFORM.
FORM execute_consistency_checks.
  IF p_all = abap_true.
    " Execute for all target systems
    PERFORM execute_for_all_systems.
  ELSE.
    " Execute for specific target system
    PERFORM execute_for_specific_system.
  ENDIF.
ENDFORM.
FORM execute_for_all_systems.
  WRITE: / 'Executing consistency checks for all target systems...'.
  SKIP.
  LOOP AT gt_target_systems INTO DATA(ls_target_system).
    gv_target_stack = ls_target_system-stack_number.
    WRITE: / 'Target System:', ls_target_system-stack_number,
           ls_target_system-prod_ver_name.
    PERFORM perform_relevance_check.
    PERFORM perform_consistency_check.
    SKIP.
  ENDLOOP.
ENDFORM.
FORM execute_for_specific_system.
  IF p_tstack IS INITIAL.
    MESSAGE 'Please select a target system' TYPE 'E'.
    RETURN.
  ENDIF.
  READ TABLE gt_target_systems INTO DATA(ls_selected_target)
    WITH KEY stack_number = p_tstack.
  IF sy-subrc <> 0.
    MESSAGE 'Invalid target system selected' TYPE 'E'.
    RETURN.
  ENDIF.
  gv_target_stack = p_tstack.
  WRITE: / 'Executing consistency checks for:',
           ls_selected_target-prod_ver_name.
  SKIP.
  PERFORM perform_relevance_check.
  PERFORM perform_consistency_check.
ENDFORM.
FORM perform_relevance_check.
  " Perform relevance check using standard function
  CALL FUNCTION '/SDF/GEN_FUNCS_S4_REL_CHK_JOB'
    EXPORTING
      iv_target_stack = gv_target_stack.
  WRITE: / 'Relevance check completed for target system'.
ENDFORM.
FORM perform_consistency_check.
  " Perform consistency check using the standard form
  PERFORM chk_consistency_4_all_items IN PROGRAM /sdf/rc_start_check
    IF FOUND.
  WRITE: / 'Consistency check completed for target system'.
ENDFORM.              " SET_COLUMN_HEADERS_SIMPLE
