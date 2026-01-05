REPORT z_file_srv.

TYPES: BEGIN OF ty_result,
         table_name TYPE tabname,
         status     TYPE c LENGTH 1,  " S=Success, E=Error
         message    TYPE string,
         file_path  TYPE string,
       END OF ty_result.

TYPES: tt_results TYPE TABLE OF ty_result.

DATA: lt_tables TYPE TABLE OF tabname,
      lv_all_tables TYPE abap_bool.

" Hardcoded table names - remove selection screen and use these instead
CONSTANTS: gc_tables TYPE string VALUE `['Z01_GROCERY]`.

START-OF-SELECTION.
  " Parse input tables from hardcoded constant
  PERFORM parse_input_tables.

  " Process tables
  PERFORM process_tables.

*&---------------------------------------------------------------------*
*&      Form  PARSE_INPUT_TABLES
*&---------------------------------------------------------------------*
FORM parse_input_tables.
  DATA: lv_input TYPE string,
        lv_table TYPE tabname.

  lv_input = gc_tables.
  TRANSLATE lv_input TO UPPER CASE.
  CONDENSE lv_input.

  " Check for wildcard
  IF lv_input = '*'.
    lv_all_tables = abap_true.
    RETURN.
  ENDIF.

  " Remove brackets and quotes
  REPLACE ALL OCCURRENCES OF '[' IN lv_input WITH ''.
  REPLACE ALL OCCURRENCES OF ']' IN lv_input WITH ''.
  REPLACE ALL OCCURRENCES OF '''' IN lv_input WITH ''.

  " Split by comma
  SPLIT lv_input AT ',' INTO TABLE lt_tables.

  " Remove leading/trailing spaces
  LOOP AT lt_tables INTO lv_table.
    CONDENSE lv_table.
    MODIFY lt_tables FROM lv_table.
  ENDLOOP.

  " Remove empty entries
  DELETE lt_tables WHERE table_line IS INITIAL.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_TABLES
*&---------------------------------------------------------------------*
FORM process_tables.
  DATA: lt_results TYPE tt_results,
        ls_result  TYPE ty_result.

  FIELD-SYMBOLS: <fs_table> TYPE tabname.

  " Get all tables if wildcard is specified
  IF lv_all_tables = abap_true.
    PERFORM get_all_tables.
  ENDIF.

  " Process each table
  LOOP AT lt_tables ASSIGNING <fs_table>.
    CLEAR ls_result.
    ls_result-table_name = <fs_table>.

    " Check if table exists
    PERFORM check_table_exists USING <fs_table> CHANGING ls_result.

    IF ls_result-status = 'S'.
      " Table exists, download it
      PERFORM download_table_to_server USING <fs_table> CHANGING ls_result.
    ELSE.
      " Table doesn't exist
      ls_result-status = 'E'.
      ls_result-message = 'Table does not exist'.
    ENDIF.

    APPEND ls_result TO lt_results.
  ENDLOOP.

  " Display results
  PERFORM display_results USING lt_results.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_TABLE_EXISTS
*&---------------------------------------------------------------------*
FORM check_table_exists USING iv_tabname TYPE tabname
                     CHANGING cs_result TYPE ty_result.
  SELECT COUNT(*) FROM dd02l
    WHERE tabname = @iv_tabname
      AND as4local = 'A'
      AND as4vers = '0000'.
  IF sy-subrc = 0 AND sy-dbcnt > 0.
    cs_result-status = 'S'.
    cs_result-message = 'Table exists'.
  ELSE.
    cs_result-status = 'E'.
    cs_result-message = 'Table does not exist'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TABLE_TO_SERVER
*&---------------------------------------------------------------------*
FORM download_table_to_server USING iv_tabname TYPE tabname
                           CHANGING cs_result TYPE ty_result.
  DATA: lt_fields TYPE TABLE OF dfies,
        ls_field  TYPE dfies,
        lr_table  TYPE REF TO data,
        lt_excel_data TYPE TABLE OF string,
        lv_line   TYPE string,
        lv_value  TYPE string,
        lv_server_path TYPE string,
        lv_filename TYPE string.

  FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                 <fs_line>  TYPE ANY,
                 <fs_field> TYPE ANY.

  " Get table structure
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = iv_tabname
      all_types      = 'X'
    TABLES
      dfies_tab      = lt_fields
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    cs_result-status = 'E'.
    cs_result-message = 'Error retrieving table structure'.
    RETURN.
  ENDIF.

  " Create dynamic internal table
  CREATE DATA lr_table TYPE TABLE OF (iv_tabname).
  ASSIGN lr_table->* TO <fs_table>.

  " Select data
  SELECT * FROM (iv_tabname) INTO TABLE <fs_table>.
  IF sy-subrc <> 0 OR <fs_table> IS INITIAL.
    cs_result-status = 'E'.
    cs_result-message = 'No data found in table'.
    RETURN.
  ENDIF.

  " Create Excel data
  CLEAR lv_line.
  LOOP AT lt_fields INTO ls_field.
    CONCATENATE lv_line ls_field-fieldname cl_abap_char_utilities=>horizontal_tab INTO lv_line.
  ENDLOOP.
  APPEND lv_line TO lt_excel_data.

  LOOP AT <fs_table> ASSIGNING <fs_line>.
    CLEAR lv_line.
    LOOP AT lt_fields INTO ls_field.
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <fs_line> TO <fs_field>.
      IF sy-subrc = 0.
        lv_value = <fs_field>.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_value WITH ' '.
      ELSE.
        lv_value = 'N/A'.
      ENDIF.
      CONCATENATE lv_line lv_value cl_abap_char_utilities=>horizontal_tab INTO lv_line.
    ENDLOOP.
    APPEND lv_line TO lt_excel_data.
  ENDLOOP.

  " Create filename with timestamp
  CONCATENATE iv_tabname '_' sy-datum '_' sy-uzeit '.xls' INTO lv_filename.

  " Create full server path in /tmp/ directory
  lv_server_path = '/usr/sap/S4H/D00/work/Z_UPGRADE_BACKUP/'&& lv_filename.

  " Try to save to server
  OPEN DATASET lv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    " Save data to server file
    LOOP AT lt_excel_data INTO lv_line.
      TRANSFER lv_line TO lv_server_path.
    ENDLOOP.
    CLOSE DATASET lv_server_path.

    cs_result-status = 'S'.
    cs_result-message = 'File downloaded successfully'.
    cs_result-file_path = lv_server_path.
  ELSE.
    cs_result-status = 'E'.
    cs_result-message = 'Could not save file to server'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_ALL_TABLES
*&---------------------------------------------------------------------*
FORM get_all_tables.
  " Get all tables from current client
  SELECT tabname FROM dd02l
    INTO TABLE lt_tables
    WHERE as4local = 'A'
      AND as4vers = '0000'
      AND tabclass IN ('TRANSP', 'CLUSTER', 'POOL')
    ORDER BY tabname.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
FORM display_results USING it_results TYPE tt_results.
  DATA: lv_success TYPE i,
        lv_failed TYPE i,
        lv_total TYPE i.

  " Count results
  DESCRIBE TABLE it_results LINES lv_total.
  LOOP AT it_results INTO DATA(ls_result).
    IF ls_result-status = 'S'.
      lv_success = lv_success + 1.
    ELSE.
      lv_failed = lv_failed + 1.
    ENDIF.
  ENDLOOP.

  " Display summary
  WRITE: / '=== PROCESSING SUMMARY ==='.
  WRITE: / 'Total tables processed: ', lv_total.
  WRITE: / 'Successful:             ', lv_success.
  WRITE: / 'Failed:                 ', lv_failed.
  WRITE: / .

  " Display detailed results with clear status labels
  WRITE: / '=== DETAILED RESULTS ==='.
  LOOP AT it_results INTO ls_result.
    IF ls_result-status = 'S'.
      WRITE: / 'SUCCESS: ', ls_result-table_name.
      WRITE: / '  Message:  ', ls_result-message.
      WRITE: / '  File path:', ls_result-file_path.
    ELSE.
      WRITE: / 'FAILED:  ', ls_result-table_name.
      WRITE: / '  Message:  ', ls_result-message.
    ENDIF.
    WRITE: / . " Empty line for better readability
  ENDLOOP.

  " Display final message
  WRITE: / '=== FINAL STATUS ==='.
  IF lv_failed = 0.
    WRITE: / 'SUCCESS: All tables downloaded successfully!'.
  ELSEIF lv_success = 0.
    WRITE: / 'ERROR: No tables could be downloaded.'.
  ELSE.
    WRITE: / 'PARTIAL SUCCESS: Some tables downloaded successfully.'.
  ENDIF.
ENDFORM.
