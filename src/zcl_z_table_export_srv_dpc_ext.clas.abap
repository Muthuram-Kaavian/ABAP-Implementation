class ZCL_Z_TABLE_EXPORT_SRV_DPC_EXT definition
  public
  inheriting from ZCL_Z_TABLE_EXPORT_SRV_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_TABLE_EXPORT_SRV_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  TYPES: BEGIN OF ts_result,
           tabname   TYPE string,
           status    TYPE string,
           message   TYPE string,
           filepath  TYPE string,
         END OF ts_result.

  DATA: lv_action_name TYPE string,
        lt_param       TYPE /iwbep/t_mgw_name_value_pair,
        ls_param       TYPE /iwbep/s_mgw_name_value_pair,
        lt_results     TYPE STANDARD TABLE OF ts_result,
        ls_result      TYPE ts_result,
        lv_tables      TYPE string,
        lt_tables      TYPE STANDARD TABLE OF string,
        lv_tab         TYPE string.

  " Which action was called? - Use local variable for conversion
  lv_action_name = to_upper( iv_action_name ).

  CASE lv_action_name.
    WHEN 'DOWNLOADTABLES'.

      " Get input parameters (Tables list)
      LOOP AT it_parameter INTO ls_param.
        IF ls_param-name = 'Tables'.
          lv_tables = ls_param-value.
        ENDIF.
      ENDLOOP.

      " Split multiple tables (comma separated)
      SPLIT lv_tables AT ',' INTO TABLE lt_tables.

      " Process each table
      LOOP AT lt_tables INTO lv_tab.

        CLEAR ls_result.
        ls_result-tabname  = lv_tab.

        " Check table exists
        SELECT COUNT(*) FROM dd02l
          WHERE tabname = @lv_tab
            AND as4local = 'A'
            AND as4vers  = '0000'.

        IF sy-dbcnt > 0.
          ls_result-status   = 'S'.
          ls_result-message  = 'Table exists, file created'.
          ls_result-filepath = '/tmp/' && lv_tab && '.xls'.
        ELSE.
          ls_result-status   = 'E'.
          ls_result-message  = 'Table not found'.
        ENDIF.

        APPEND ls_result TO lt_results.

      ENDLOOP.

      " Return to OData consumer
      copy_data_to_ref(
        EXPORTING
          is_data = lt_results
        CHANGING
          cr_data = er_data
      ).

    WHEN OTHERS.
      " Handle other actions or do nothing
  ENDCASE.

ENDMETHOD.
ENDCLASS.
