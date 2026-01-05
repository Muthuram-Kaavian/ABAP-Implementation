class ZCL_ZTARGET_SYSTEM_DPC_EXT definition
  public
  inheriting from ZCL_ZTARGET_SYSTEM_DPC
  create public .

public section.
protected section.

  methods TARGETSYSTEMSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZTARGET_SYSTEM_DPC_EXT IMPLEMENTATION.


METHOD targetsystemset_get_entityset.

  DATA: lt_conv_targ_stack TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
        ls_conv_targ_stack LIKE LINE OF lt_conv_targ_stack,
        ls_entity          TYPE zcl_ztarget_system_mpc=>ts_targetsystem.

  " Get S/4HANA target versions
  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING
      et_version = lt_conv_targ_stack
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception(
      textid  = /iwbep/cx_mgw_busi_exception=>business_error
      message = |Error getting S/4HANA target systems (SY-SUBRC = { sy-subrc })| ).
  ENDIF.

  LOOP AT lt_conv_targ_stack INTO ls_conv_targ_stack.
    CLEAR ls_entity.

    " Raw fields 1:1
    ls_entity-stacknumber = ls_conv_targ_stack-stack_number.
    ls_entity-prodvername = ls_conv_targ_stack-prod_ver_name.
    ls_entity-proddesc    = ls_conv_targ_stack-prod_desc.
    ls_entity-stackname   = ls_conv_targ_stack-stack_name.

    " New: combined display value like the report:
    " SAP S/4HANA 2025 [01 (02/2026) FPS]
    IF ls_conv_targ_stack-stack_name IS INITIAL.
      ls_entity-displayname = ls_conv_targ_stack-prod_ver_name.
    ELSE.
      ls_entity-displayname = |{ ls_conv_targ_stack-prod_ver_name } [{ ls_conv_targ_stack-stack_name }]|.
    ENDIF.

    APPEND ls_entity TO et_entityset.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
