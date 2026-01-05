class ZCL_Z_EXPORT_TABLE__01_DPC_EXT definition
  public
  inheriting from ZCL_Z_EXPORT_TABLE__01_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.

  methods PROCESS_TABLES
    importing
      !IT_INPUT_TABLES type ZCL_Z_EXPORT_TABLE__01_MPC=>TT_INPUTTYPE
    exporting
      !ET_RESULTS type ZCL_Z_EXPORT_TABLE__01_MPC=>TT_RESULTTYPE .
  methods DOWNLOAD_TABLE_TO_SERVER
    importing
      !IV_TABNAME type TABNAME
    changing
      !CS_RESULT type ZCL_Z_EXPORT_TABLE__01_MPC=>TS_RESULTTYPE .
  methods PARSE_JSON_INPUT
    importing
      !IV_JSON type STRING
    exporting
      !ET_INPUT_TABLES type ZCL_Z_EXPORT_TABLE__01_MPC=>TT_INPUTTYPE .
ENDCLASS.



CLASS ZCL_Z_EXPORT_TABLE__01_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
  DATA: lt_input_tables TYPE zcl_z_export_table__01_mpc=>tt_inputtype,
        lt_results TYPE zcl_z_export_table__01_mpc=>tt_resulttype,
        ls_input TYPE zcl_z_export_table__01_mpc=>ts_inputtype.

  CASE iv_action_name.
    WHEN 'ProcessTables'.
      " Hardcode test values
      ls_input-tablename = 'Z01_GROCER'.
      APPEND ls_input TO lt_input_tables.

      ls_input-tablename = 'NONEXISTENT_TABLE'.
      APPEND ls_input TO lt_input_tables.

      " Process tables
      process_tables(
        EXPORTING
          it_input_tables = lt_input_tables
        IMPORTING
          et_results = lt_results ).

      " Return response
      copy_data_to_ref(
        EXPORTING
          is_data = lt_results
        CHANGING
          cr_data = er_data ).
  ENDCASE.
ENDMETHOD.


METHOD download_table_to_server.
  " Temporary implementation
  cs_result-status = 'S'.
  cs_result-message = 'File downloaded successfully'.
  cs_result-filepath = |/tmp/{ iv_tabname }_{ sy-datum }_{ sy-uzeit }.xls|.
ENDMETHOD.


METHOD parse_json_input.
  DATA: lv_temp      TYPE string,
        lt_lines     TYPE TABLE OF string,
        ls_input     TYPE zcl_z_export_table__01_mpc=>ts_inputtype,
        lv_clean     TYPE string.

  " Simple JSON parsing for the specific structure
  lv_temp = iv_json.

  " Remove whitespace and brackets
  REPLACE ALL OCCURRENCES OF REGEX '\s' IN lv_temp WITH ''.
  REPLACE ALL OCCURRENCES OF '[' IN lv_temp WITH ''.
  REPLACE ALL OCCURRENCES OF ']' IN lv_temp WITH ''.

  " Split by table objects
  SPLIT lv_temp AT '},' INTO TABLE lt_lines.

  LOOP AT lt_lines INTO lv_temp.
    " Find TableName field
    IF lv_temp CS '"TableName":'.
      " Extract the table name value
      SPLIT lv_temp AT '"TableName":"' INTO lv_temp lv_clean.
      SPLIT lv_clean AT '"' INTO ls_input-tablename lv_clean.

      " Add to result table
      IF ls_input-tablename IS NOT INITIAL.
        APPEND ls_input TO et_input_tables.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD process_tables.
  DATA: ls_result TYPE zcl_z_export_table__01_mpc=>ts_resulttype.

  LOOP AT it_input_tables INTO DATA(ls_input).
    CLEAR ls_result.
    ls_result-tablename = ls_input-tablename.

    DATA(lv_tabname) = CONV tabname( ls_input-tablename ).

    SELECT COUNT(*) FROM dd02l
      WHERE tabname = @lv_tabname
        AND as4local = 'A'
        AND as4vers = '0000'.

    IF sy-subrc = 0 AND sy-dbcnt > 0.
      download_table_to_server(
        EXPORTING
          iv_tabname = lv_tabname
        CHANGING
          cs_result  = ls_result ).
    ELSE.
      ls_result-status = 'E'.
      ls_result-message = 'Table does not exist'.
      ls_result-filepath = ''.
    ENDIF.

    APPEND ls_result TO et_results.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
