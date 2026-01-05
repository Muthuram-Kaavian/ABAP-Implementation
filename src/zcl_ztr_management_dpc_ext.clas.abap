class ZCL_ZTR_MANAGEMENT_DPC_EXT definition
  public
  inheriting from ZCL_ZTR_MANAGEMENT_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZTR_MANAGEMENT_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
  DATA: lv_action TYPE string.
  lv_action = iv_action_name.

  CASE lv_action.
    WHEN 'ManageTR'.

      "=== TEMPORARY DEBUG CODE - REMOVE LATER ===
      DATA: lt_params TYPE /iwbep/t_mgw_name_value_pair.
      lt_params = io_tech_request_context->get_parameters( ).

      "Create debug response to see parameter names
      TYPES: BEGIN OF ty_param_debug,
               param_name TYPE string,
               param_value TYPE string,
             END OF ty_param_debug.
      DATA: lt_debug TYPE TABLE OF ty_param_debug.

      LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<ls_param>).
        APPEND VALUE ty_param_debug(
          param_name = <ls_param>-name
          param_value = <ls_param>-value
        ) TO lt_debug.
      ENDLOOP.

      "If no parameters found, show message
      IF lines( lt_debug ) = 0.
        APPEND VALUE ty_param_debug(
          param_name = 'INFO'
          param_value = 'No parameters received'
        ) TO lt_debug.
      ENDIF.

      copy_data_to_ref( EXPORTING is_data = lt_debug CHANGING cr_data = er_data ).
      RETURN.
      "=== END DEBUG CODE ===

    WHEN OTHERS.
      "Keep existing code
  ENDCASE.
ENDMETHOD.
ENDCLASS.
