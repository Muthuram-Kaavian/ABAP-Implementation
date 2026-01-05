REPORT z_display_consistency_check_all.

* Include standard variables
INCLUDE /sdf/rc_start_check_variable.

DATA: gt_target_systems TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tstack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
            AS LISTBOX VISIBLE LENGTH 60 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM get_target_systems.

AT SELECTION-SCREEN OUTPUT.
  PERFORM update_target_list.

START-OF-SELECTION.
  PERFORM display_consistency_check_all.

FORM get_target_systems.
  " Get available target systems with error handling
  TRY.
      /sdf/cl_rc_chk_utility=>get_target_s4_version(
        IMPORTING
          et_version = gt_target_systems
        EXCEPTIONS
          error      = 1
          OTHERS     = 2 ).

      IF sy-subrc <> 0 OR lines( gt_target_systems ) = 0.
        MESSAGE 'No target systems found. Please run standard transaction /SDF/RC_START_CHECK' TYPE 'E'.
      ENDIF.
    CATCH cx_root.
      MESSAGE 'Error retrieving target systems' TYPE 'E'.
  ENDTRY.
ENDFORM.

FORM update_target_list.
  DATA: lt_list TYPE vrm_values,
        ls_list LIKE LINE OF lt_list.

  IF lines( gt_target_systems ) = 0.
    MESSAGE 'No target systems available' TYPE 'I'.
    RETURN.
  ENDIF.

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

FORM display_consistency_check_all.
  " Check if target system is selected
  IF p_tstack IS INITIAL.
    MESSAGE 'Please select a target system' TYPE 'E'.
    RETURN.
  ENDIF.

  " Set target stack and run standard check
  gv_target_stack = p_tstack.
  gv_mode = 'D'.  " Display mode

  " Submit the standard consistency check
  SUBMIT /sdf/rc_start_check
    WITH so_target = p_tstack
    WITH p_mode = 'D'
    AND RETURN.
ENDFORM.
