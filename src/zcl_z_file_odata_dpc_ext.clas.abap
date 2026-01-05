class ZCL_Z_FILE_ODATA_DPC_EXT definition
  public
  inheriting from ZCL_Z_FILE_ODATA_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.

  methods ZFILE_INPUTSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_FILE_ODATA_DPC_EXT IMPLEMENTATION.


METHOD ZFILE_INPUTSET_CREATE_ENTITY.

  " Local type definitions
  TYPES: BEGIN OF ty_result,
           table_name TYPE tabname,
           status     TYPE c LENGTH 1,  " S=Success, E=Error
           message    TYPE string,
           file_path  TYPE string,
         END OF ty_result.

  TYPES: tt_results TYPE TABLE OF ty_result.

  " Data declarations
  DATA: ls_input      TYPE zcl_z_file_odata_mpc=>ts_zfile_input,
        lt_tables     TYPE TABLE OF tabname,
        lv_all_tables TYPE abap_bool,
        lt_results    TYPE tt_results,
        ls_result     TYPE ty_result.

  FIELD-SYMBOLS: <fs_table> TYPE tabname.

  " 1. Read input from OData request
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

  " 2. Parse input tables
  IF ls_input-all_tables = abap_true.
    lv_all_tables = abap_true.

    " Get all tables
    SELECT tabname FROM dd02l
      INTO TABLE lt_tables
      WHERE as4local = 'A'
        AND as4vers = '0000'
        AND tabclass IN ('TRANSP', 'CLUSTER', 'POOL')
      ORDER BY tabname.
  ELSE.
    " Parse tables from JSON string
    DATA: lv_input TYPE string,
          lv_table TYPE tabname.

    lv_input = ls_input-tables_string.
    TRANSLATE lv_input TO UPPER CASE.
    CONDENSE lv_input.

    " Remove brackets and quotes from JSON array
    REPLACE ALL OCCURRENCES OF '[' IN lv_input WITH ''.
    REPLACE ALL OCCURRENCES OF ']' IN lv_input WITH ''.
    REPLACE ALL OCCURRENCES OF '''' IN lv_input WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN lv_input WITH ''.

    " Split by comma
    SPLIT lv_input AT ',' INTO TABLE lt_tables.

    " Remove leading/trailing spaces
    LOOP AT lt_tables INTO lv_table.
      CONDENSE lv_table.
      MODIFY lt_tables FROM lv_table.
    ENDLOOP.

    " Remove empty entries
    DELETE lt_tables WHERE table_line IS INITIAL.
  ENDIF.

  " 3. Process each table
  LOOP AT lt_tables ASSIGNING <fs_table>.
    CLEAR ls_result.
    ls_result-table_name = <fs_table>.

    " Check if table exists
    SELECT COUNT(*) FROM dd02l
      WHERE tabname = @<fs_table>
        AND as4local = 'A'
        AND as4vers = '0000'.
    IF sy-subrc = 0 AND sy-dbcnt > 0.
      ls_result-status = 'S'.
      ls_result-message = 'Table exists'.

      " Table exists, download it
      DATA: lt_fields TYPE TABLE OF dfies,
            ls_field  TYPE dfies,
            lr_table  TYPE REF TO data,
            lt_excel_data TYPE TABLE OF string,
            lv_line   TYPE string,
            lv_value  TYPE string,
            lv_server_path TYPE string,
            lv_filename TYPE string.

      FIELD-SYMBOLS: <fs_table_data> TYPE STANDARD TABLE,
                     <fs_line>       TYPE ANY,
                     <fs_field>      TYPE ANY.

      " Get table structure
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = <fs_table>
          all_types      = 'X'
        TABLES
          dfies_tab      = lt_fields
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        ls_result-status = 'E'.
        ls_result-message = 'Error retrieving table structure'.
      ELSE.
        " Create dynamic internal table
        CREATE DATA lr_table TYPE TABLE OF (<fs_table>).
        ASSIGN lr_table->* TO <fs_table_data>.

        " Select data
        SELECT * FROM (<fs_table>) INTO TABLE <fs_table_data>.
        IF sy-subrc <> 0 OR <fs_table_data> IS INITIAL.
          ls_result-status = 'E'.
          ls_result-message = 'No data found in table'.
        ELSE.
          " Create Excel data
          CLEAR lv_line.
          LOOP AT lt_fields INTO ls_field.
            CONCATENATE lv_line ls_field-fieldname
                       cl_abap_char_utilities=>horizontal_tab INTO lv_line.
          ENDLOOP.
          APPEND lv_line TO lt_excel_data.

          LOOP AT <fs_table_data> ASSIGNING <fs_line>.
            CLEAR lv_line.
            LOOP AT lt_fields INTO ls_field.
              ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <fs_line> TO <fs_field>.
              IF sy-subrc = 0.
                lv_value = <fs_field>.
                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_value WITH ' '.
              ELSE.
                lv_value = 'N/A'.
              ENDIF.
              CONCATENATE lv_line lv_value
                         cl_abap_char_utilities=>horizontal_tab INTO lv_line.
            ENDLOOP.
            APPEND lv_line TO lt_excel_data.
          ENDLOOP.

          " Create filename with timestamp
          CONCATENATE <fs_table> '_' sy-datum '_' sy-uzeit '.xls' INTO lv_filename.

          " Create full server path in /tmp/ directory
          lv_server_path = '/tmp/' && lv_filename.

          " Try to save to server
          OPEN DATASET lv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc = 0.
            " Save data to server file
            LOOP AT lt_excel_data INTO lv_line.
              TRANSFER lv_line TO lv_server_path.
            ENDLOOP.
            CLOSE DATASET lv_server_path.

            ls_result-status = 'S'.
            ls_result-message = 'File downloaded successfully'.
            ls_result-file_path = lv_server_path.
          ELSE.
            ls_result-status = 'E'.
            ls_result-message = 'Could not save file to server'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      " Table doesn't exist
      ls_result-status = 'E'.
      ls_result-message = 'Table does not exist'.
    ENDIF.

    APPEND ls_result TO lt_results.
  ENDLOOP.

  " 4. Store results in memory or database for later retrieval
  " For now, we'll use a simple approach - store in a global variable
  " In production, use proper storage like database table or shared memory

  " 5. Return the input entity as response
  " CREATE_ENTITY must return the same entity type that was created
  er_entity = ls_input.

ENDMETHOD.
ENDCLASS.
