REPORT z_get_s4_target_systems.

TYPE-POOLS: vrm.

DATA: gt_conv_targ_stack TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
      gt_list            TYPE vrm_values,
      gs_list            LIKE LINE OF gt_list.

START-OF-SELECTION.
  PERFORM get_target_systems.

FORM get_target_systems.
  " Get S/4HANA conversion target releases
  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING
      et_version = gt_conv_targ_stack
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).

  IF sy-subrc <> 0.
    WRITE: / 'Error getting target systems'.
    RETURN.
  ENDIF.

  " Prepare dropdown list values
  LOOP AT gt_conv_targ_stack INTO DATA(ls_target_stack).
    gs_list-key  = ls_target_stack-stack_number.
    CONCATENATE ls_target_stack-prod_ver_name
                ' [' ls_target_stack-stack_name ']'
                INTO gs_list-text.
    APPEND gs_list TO gt_list.
  ENDLOOP.

  " Display available target systems
  WRITE: / 'Available SAP S/4HANA Target Systems:'.
  SKIP.

  LOOP AT gt_list INTO gs_list.
    WRITE: / gs_list-key, gs_list-text.
  ENDLOOP.

ENDFORM.
