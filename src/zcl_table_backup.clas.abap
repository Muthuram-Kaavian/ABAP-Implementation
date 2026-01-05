CLASS zcl_table_backup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "=== Result row returned to OData ===
    TYPES: BEGIN OF ty_result,
             tablename TYPE tabname,
             status    TYPE c LENGTH 1,     " 'S' or 'E'
             message   TYPE string,
             filepath  TYPE string,
           END OF ty_result.

    TYPES: tt_results  TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.
    TYPES: tt_tabnames TYPE STANDARD TABLE OF tabname  WITH EMPTY KEY.

    " Named DFIES table type for RETURNING (no generic table in RETURNING)
    TYPES: tt_dfies    TYPE STANDARD TABLE OF dfies WITH EMPTY KEY.

    "=== Public API ===
    CLASS-METHODS backup_table
      IMPORTING  iv_tabname TYPE tabname
      RETURNING VALUE(rs)   TYPE ty_result.

    CLASS-METHODS backup_tables
      IMPORTING  it_tabnames TYPE tt_tabnames
      RETURNING VALUE(rt)    TYPE tt_results.

  PRIVATE SECTION.
    " >>> updated to your server path (with trailing slash as requested)
    CONSTANTS: gc_folder TYPE string VALUE '/usr/sap/S4H/D00/work/Z_UPGRADE_BACKUP/',
               gc_ext    TYPE string VALUE '.xls'.  " keep .xls so Excel opens it

    CLASS-METHODS table_exists
      IMPORTING  iv_tabname   TYPE tabname
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS get_fields
      IMPORTING  iv_tabname  TYPE tabname
      RETURNING VALUE(rt_df) TYPE tt_dfies.

    CLASS-METHODS build_fullpath
      IMPORTING  iv_tabname TYPE tabname
      RETURNING VALUE(rv)   TYPE string.

    CLASS-METHODS write_table_file
      IMPORTING  iv_tabname TYPE tabname
      CHANGING   cs_res     TYPE ty_result.
ENDCLASS.



CLASS ZCL_TABLE_BACKUP IMPLEMENTATION.


  METHOD backup_table.
    DATA lv_tab     TYPE tabname.
    DATA lv_path_c  TYPE c LENGTH 1024.

    lv_tab = iv_tabname.
    TRANSLATE lv_tab TO UPPER CASE.
    CONDENSE   lv_tab.

    rs-tablename = lv_tab.

    IF lv_tab IS INITIAL.
      rs-status  = 'E'.
      rs-message = 'Table name is empty'.
      RETURN.
    ENDIF.

    IF table_exists( lv_tab ) = abap_false.
      rs-status  = 'E'.
      rs-message = |Table { lv_tab } does not exist|.
      RETURN.
    ENDIF.

    " Read authorization
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'TABLE' FIELD lv_tab
      ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      rs-status  = 'E'.
      rs-message = |No auth to read { lv_tab } (S_TABU_NAM 03)|.
      RETURN.
    ENDIF.

    rs-filepath = build_fullpath( lv_tab ).

    " Char-like path for dataset ops
    lv_path_c = rs-filepath.

    " Write authorization
    AUTHORITY-CHECK OBJECT 'S_DATASET'
      ID 'PROGRAM'   FIELD sy-repid
      ID 'OPERATION' FIELD 'WRITE'
      ID 'FILENAME'  FIELD lv_path_c.
    IF sy-subrc <> 0.
      rs-status  = 'E'.
      rs-message = |No auth to write { rs-filepath } (S_DATASET WRITE)|.
      RETURN.
    ENDIF.

    " Write file
    write_table_file( EXPORTING iv_tabname = lv_tab CHANGING cs_res = rs ).
  ENDMETHOD.


  METHOD backup_tables.
    DATA lv_tab TYPE tabname.
    LOOP AT it_tabnames INTO lv_tab.
      APPEND backup_table( lv_tab ) TO rt.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_fullpath.
    DATA lv_fname TYPE string.
    " <folder>/<TAB>_<YYYYMMDD>_<HHMMSS>.xls
    lv_fname = |{ iv_tabname }_{ sy-datum }_{ sy-uzeit }{ gc_ext }|.
    " since gc_folder ends with '/', just concatenate directly
    rv = gc_folder && lv_fname.
  ENDMETHOD.


  METHOD get_fields.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = iv_tabname
        all_types = 'X'
      TABLES
        dfies_tab = rt_df
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      CLEAR rt_df.
    ENDIF.
  ENDMETHOD.


  METHOD table_exists.
    SELECT COUNT(*) FROM dd02l
      WHERE tabname = @iv_tabname
        AND as4local = 'A'
        AND as4vers  = '0000'.
    rv_yes = xsdbool( sy-subrc = 0 AND sy-dbcnt > 0 ).
  ENDMETHOD.


  METHOD write_table_file.
    DATA lt_fields TYPE tt_dfies.
    DATA lr_tab    TYPE REF TO data.
    DATA lv_line   TYPE string.
    DATA lv_val    TYPE string.
    DATA lv_path_c TYPE c LENGTH 1024.
    DATA lx        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_tab>  TYPE STANDARD TABLE,
                   <ls_row>  TYPE any,
                   <lv_cell> TYPE any,
                   <f>       TYPE dfies.

    lt_fields = get_fields( iv_tabname ).
    IF lt_fields IS INITIAL.
      cs_res-status  = 'E'.
      cs_res-message = 'Error retrieving table structure'.
      RETURN.
    ENDIF.

    " Char-like file path for dataset ops
    lv_path_c = cs_res-filepath.

    " Create dynamic internal table of <iv_tabname>
    CREATE DATA lr_tab TYPE TABLE OF (iv_tabname).
    ASSIGN lr_tab->* TO <lt_tab>.

    OPEN DATASET lv_path_c FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      cs_res-status  = 'E'.
      cs_res-message = |Could not open file { cs_res-filepath }|.
      RETURN.
    ENDIF.

    " Header (tab-separated)
    CLEAR lv_line.
    LOOP AT lt_fields ASSIGNING <f>.
      IF lv_line IS INITIAL.
        lv_line = <f>-fieldname.
      ELSE.
        lv_line = lv_line && cl_abap_char_utilities=>horizontal_tab && <f>-fieldname.
      ENDIF.
    ENDLOOP.
    TRANSFER lv_line TO lv_path_c.

    " Stream rows in packages
    TRY.
        SELECT * FROM (iv_tabname)
          INTO TABLE @<lt_tab>
          PACKAGE SIZE 10000.

          IF <lt_tab> IS INITIAL AND sy-dbcnt = 0.
            CLOSE DATASET lv_path_c.
            cs_res-status  = 'E'.
            cs_res-message = 'No data found in table'.
            RETURN.
          ENDIF.

          LOOP AT <lt_tab> ASSIGNING <ls_row>.
            CLEAR lv_line.
            LOOP AT lt_fields ASSIGNING <f>.
              ASSIGN COMPONENT <f>-fieldname OF STRUCTURE <ls_row> TO <lv_cell>.
              IF sy-subrc = 0.
                lv_val = <lv_cell>.
              ELSE.
                lv_val = 'N/A'.
              ENDIF.
              " sanitize: remove any tabs from values
              REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_val WITH space.
              IF lv_line IS INITIAL.
                lv_line = lv_val.
              ELSE.
                lv_line = lv_line && cl_abap_char_utilities=>horizontal_tab && lv_val.
              ENDIF.
            ENDLOOP.
            TRANSFER lv_line TO lv_path_c.
          ENDLOOP.

          CLEAR <lt_tab>.
        ENDSELECT.

        CLOSE DATASET lv_path_c.
        cs_res-status  = 'S'.
        cs_res-message = 'File downloaded successfully'.

      CATCH cx_root INTO lx.
        CLOSE DATASET lv_path_c.
        cs_res-status  = 'E'.
        cs_res-message = lx->get_text( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
