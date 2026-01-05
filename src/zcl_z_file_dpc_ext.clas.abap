class ZCL_Z_FILE_DPC_EXT definition
  public
  inheriting from ZCL_Z_FILE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_FILE_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_function.
  DATA lv_func TYPE string.
  " Which function import was called?
  lv_func = io_tech_request_context->get_function_import_name( ).

  CASE lv_func.

    WHEN 'RunExport'.  " <- your Function Import name in SEGW

      " 1) Read POST body parameter: { "TableList": "EKKO" }
      DATA(lv_tablelist) = VALUE string( ).
      READ TABLE it_parameter WITH KEY name = 'TableList' INTO DATA(ls_param).
      IF sy-subrc = 0.
        lv_tablelist = ls_param-value.
      ENDIF.
      IF lv_tablelist IS INITIAL.
        lv_tablelist = '*'. " optional default
      ENDIF.

      " 2) Call your logic
      DATA(lo_exporter) = NEW zcl_file_export( ).
      DATA(lt_results_abap) = lo_exporter->run( iv_table_list = lv_tablelist ).

      " 3) Map to OData entity type table (generated in MPC)
      "    Replace zcl_z_file_srv_mpc with YOUR project's MPC class name
      DATA lt_result_es TYPE zcl_z_file_srv_mpc=>tt_result.
      LOOP AT lt_results_abap INTO DATA(ls_res).
        APPEND VALUE zcl_z_file_srv_mpc=>ts_result(
          tablename = ls_res-table_name
          status    = ls_res-status
          message   = ls_res-message
          filepath  = ls_res-file_path ) TO lt_result_es.
      ENDLOOP.

      " 4) Return a *collection* -> goes out as {"d":{"results":[...]}}
      copy_data_to_ref(
        EXPORTING is_data = lt_result_es
        CHANGING  cr_data = er_entityset ).

    WHEN OTHERS.
      " Let super handle anything else (if present)
      super->/iwbep/if_mgw_appl_srv_runtime~execute_function(
        EXPORTING iv_function_import_name = iv_function_import_name
                  it_parameter            = it_parameter
        IMPORTING er_entity               = er_entity
                  er_entityset            = er_entityset
                  et_return               = et_return ).
  ENDCASE.
ENDMETHOD.
ENDCLASS.
