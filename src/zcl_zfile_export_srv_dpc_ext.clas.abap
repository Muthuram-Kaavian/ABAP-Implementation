class ZCL_ZFILE_EXPORT_SRV_DPC_EXT definition
  public
  inheriting from ZCL_ZFILE_EXPORT_SRV_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFILE_EXPORT_SRV_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  DATA: lt_parameters TYPE /iwbep/t_mgw_name_value_pair,
        ls_parameter  TYPE /iwbep/s_mgw_name_value_pair,
        lv_tablist    TYPE string,
        lt_tables     TYPE STANDARD TABLE OF tabname,
        lv_all_tables TYPE abap_bool,
        lt_results    TYPE TABLE OF zcl_zfile_export_srv_mpc=>ts_result,
        ls_result     TYPE zcl_zfile_export_srv_mpc=>ts_result,
        lv_table      TYPE tabname,
        lt_fields     TYPE TABLE OF dfies,
        ls_field      TYPE dfies,
        lr_table      TYPE REF TO data,
        lt_excel_data TYPE TABLE OF string,
        lv_line       TYPE string,
        lv_value      TYPE string,
        lv_server_path TYPE string,
        lv_filename    TYPE string.

  FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                 <fs_line>  TYPE ANY,
                 <fs_field> TYPE ANY.

  CASE iv_action_name.
    WHEN 'DownloadTables'.

      " --- 1. Retrieve parameter TableList ---
      lt_parameters = io_tech_request_context->get_parameters( ).
      READ TABLE lt_parameters INTO ls_parameter WITH KEY name = 'TableList'.
      IF sy-subrc = 0.
        lv_tablist = ls_parameter-value.
      ELSE.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = 'Parameter TableList is required'.
      ENDIF.

      " --- 2. Parse input ---
      TRANSLATE lv_tablist TO UPPER CASE.
      CONDENSE lv_tablist.

      IF lv_tablist = '*'.
        lv_all_tables = abap_true.
      ELSE.
        REPLACE ALL OCCURRENCES OF '[' IN lv_tablist WITH ''.
        REPLACE ALL OCCURRENCES OF ']' IN lv_tablist WITH ''.
        REPLACE ALL OCCURRENCES OF '''' IN lv_tablist WITH ''.
        SPLIT lv_tablist AT ',' INTO TABLE lt_tables.
        LOOP AT lt_tables INTO lv_table.
          CONDENSE lv_table.
          MODIFY lt_tables FROM lv_table.
        ENDLOOP.
        DELETE lt_tables WHERE table_line IS INITIAL.
      ENDIF.

      IF lt_tables IS INITIAL AND lv_all_tables = abap_false.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = 'No valid tables specified in TableList parameter'.
      ENDIF.

      " --- 3. Handle wildcard '*' ---
      IF lv_all_tables = abap_true.
        SELECT tabname FROM dd02l
          INTO TABLE @lt_tables
          WHERE as4local = 'A'
            AND as4vers  = '0000'
            AND tabclass = 'TRANSP'.
      ENDIF.

      " --- 4. Process each table ---
      LOOP AT lt_tables INTO lv_table.
        CLEAR: ls_result, lt_fields, lt_excel_data.
        ls_result-TableName = lv_table.

        " Check if table exists
        SELECT COUNT(*) FROM dd02l
          WHERE tabname = @lv_table
            AND as4local = 'A'
            AND as4vers  = '0000'
          INTO @DATA(lv_count).
        IF lv_count = 0.
          ls_result-Status  = 'E'.
          ls_result-Message = 'Table does not exist'.
          APPEND ls_result TO lt_results.
          CONTINUE.
        ENDIF.

        " Get table structure
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname   = lv_table
            all_types = 'X'
          TABLES
            dfies_tab = lt_fields
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          ls_result-Status  = 'E'.
          ls_result-Message = 'Error retrieving table structure'.
          APPEND ls_result TO lt_results.
          CONTINUE.
        ENDIF.

        " Create dynamic internal table
        CREATE DATA lr_table TYPE TABLE OF (lv_table).
        ASSIGN lr_table->* TO <fs_table>.

        " Select table data
        SELECT * FROM (lv_table) INTO TABLE @<fs_table>.
        IF sy-subrc <> 0 OR lines( <fs_table> ) = 0.
          ls_result-Status  = 'E'.
          ls_result-Message = 'No data found in table'.
          APPEND ls_result TO lt_results.
          CONTINUE.
        ENDIF.

        " Prepare Excel header
        CLEAR lv_line.
        LOOP AT lt_fields INTO ls_field.
          CONCATENATE lv_line ls_field-fieldname
                      cl_abap_char_utilities=>horizontal_tab
                 INTO lv_line.
        ENDLOOP.
        APPEND lv_line TO lt_excel_data.

        " Prepare Excel rows
        LOOP AT <fs_table> ASSIGNING <fs_line>.
          CLEAR lv_line.
          LOOP AT lt_fields INTO ls_field.
            ASSIGN COMPONENT ls_field-fieldname
                   OF STRUCTURE <fs_line> TO <fs_field>.
            IF sy-subrc = 0.
              lv_value = <fs_field>.
              REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab
                  IN lv_value WITH ' '.
            ELSE.
              lv_value = 'N/A'.
            ENDIF.
            CONCATENATE lv_line lv_value
                        cl_abap_char_utilities=>horizontal_tab
                   INTO lv_line.
          ENDLOOP.
          APPEND lv_line TO lt_excel_data.
        ENDLOOP.

        " Create filename and server path
        CONCATENATE lv_table '_' sy-datum '_' sy-uzeit '.xls'
                    INTO lv_filename.
        lv_server_path = '/usr/sap/S4H/D00/work/Z_UPGRADE_BACKUP/' && lv_filename.

        " Save Excel to server
        OPEN DATASET lv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc = 0.
          LOOP AT lt_excel_data INTO lv_line.
            TRANSFER lv_line TO lv_server_path.
          ENDLOOP.
          CLOSE DATASET lv_server_path.

          ls_result-Status   = 'S'.
          ls_result-Message  = 'File downloaded successfully'.
          ls_result-FilePath = lv_server_path.
        ELSE.
          ls_result-Status   = 'E'.
          ls_result-Message  = 'Could not save file to server'.
        ENDIF.

        APPEND ls_result TO lt_results.
      ENDLOOP.

      " --- 5. Map results to OData response ---
      copy_data_to_ref(
        EXPORTING is_data = lt_results
        CHANGING  cr_data = er_data
      ).

  ENDCASE.

ENDMETHOD.
ENDCLASS.
