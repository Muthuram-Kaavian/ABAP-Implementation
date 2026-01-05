class ZCL_ZFILE_PARAMS_DPC_EXT definition
  public
  inheriting from ZCL_ZFILE_PARAMS_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFILE_PARAMS_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  DATA: lt_tables          TYPE TABLE OF tabname,
        lt_tables_raw      TYPE TABLE OF tabname,
        ls_parameter       TYPE /iwbep/s_mgw_name_value_pair,
        lt_results         TYPE STANDARD TABLE OF zcl_ztable_export_srv_mpc=>ts_tableexportstatus,
        ls_result          LIKE LINE OF lt_results.

  FIELD-SYMBOLS: <fs_table> TYPE tabname.

  IF iv_action_name = 'ExportTables'.

    "******************************************************************"
    "* START OF MODIFICATIONS                                         *"
    "******************************************************************"

    " 1. Read input parameters: TableNames and FilePath
    DATA lv_table_names_raw TYPE string.
    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'TableNames'.
    IF sy-subrc = 0.
      lv_table_names_raw = ls_parameter-value.
    ENDIF.

    DATA lv_filepath_raw TYPE string.
    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'FilePath'.
    IF sy-subrc = 0.
      lv_filepath_raw = ls_parameter-value.
    ENDIF.

    " 2. Validation: Check if mandatory parameters are empty or contain only whitespace
    DATA(lv_clean_tables) = lv_table_names_raw.
    CONDENSE lv_clean_tables.

    DATA(lv_validated_path) = lv_filepath_raw.
    CONDENSE lv_validated_path.

    IF lv_clean_tables IS INITIAL.
      ls_result-status  = 'FAILED'.
      ls_result-message = 'Parameter "TableNames" must not be empty.'.
      APPEND ls_result TO lt_results.
      copy_data_to_ref( EXPORTING is_data = lt_results CHANGING cr_data = er_data ).
      RETURN.
    ENDIF.

    IF lv_validated_path IS INITIAL.
      ls_result-status  = 'FAILED'.
      ls_result-message = 'Parameter "FilePath" must not be empty.'.
      APPEND ls_result TO lt_results.
      copy_data_to_ref( EXPORTING is_data = lt_results CHANGING cr_data = er_data ).
      RETURN.
    ENDIF.

    " 3. AL11 Path Validation: Check if the provided server path exists
    DATA lt_dir_list TYPE TABLE OF epsfili.

    " *** FIX: Use a correctly typed variable for the older function module. ***
    DATA lv_fm_path TYPE epsf-epsdirnam.
    lv_fm_path = lv_validated_path.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name              = lv_fm_path " Pass the correctly typed variable
      TABLES
        dir_list              = lt_dir_list
      EXCEPTIONS
        invalid_eps_subdir    = 1
        sapgparam_failed      = 2
        build_directory_failed = 3
        no_authorization      = 4
        read_directory_failed = 5
        too_many_read_errors  = 6
        empty_directory_list  = 7 " An empty directory is valid, so this exception is ignored.
        OTHERS                = 8.

    IF sy-subrc <> 0 AND sy-subrc <> 7.
      ls_result-status  = 'FAILED'.
      ls_result-message = |Path '{ lv_validated_path }' does not exist on the server or you lack authorization.|.
      APPEND ls_result TO lt_results.
      copy_data_to_ref( EXPORTING is_data = lt_results CHANGING cr_data = er_data ).
      RETURN.
    ENDIF.

    DATA(lv_len) = strlen( lv_validated_path ).
    IF lv_len > 0.
      DATA(lv_offset) = lv_len - 1.
      IF lv_validated_path+lv_offset(1) = '/'.
        lv_validated_path = lv_validated_path(lv_offset).
      ENDIF.
    ENDIF.

    "******************************************************************"
    "* END OF MODIFICATIONS                                           *"
    "******************************************************************"

    DATA(lv_check_wildcard) = lv_clean_tables.
    REPLACE ALL OCCURRENCES OF '''' IN lv_check_wildcard WITH ''.
    REPLACE ALL OCCURRENCES OF '"'  IN lv_check_wildcard WITH ''.

    IF lv_check_wildcard = '*'.
      SELECT tabname
        FROM dd02l
        WHERE ( tabname LIKE 'Z%' OR tabname LIKE 'Y%' )
          AND as4local = 'A'
          AND tabclass = 'TRANSP'
        INTO TABLE @lt_tables.
    ELSE.
      SPLIT lv_table_names_raw AT ',' INTO TABLE lt_tables_raw.
      LOOP AT lt_tables_raw ASSIGNING <fs_table>.
        DATA(lv_tab) = |{ <fs_table> }|.
        CONDENSE lv_tab.
        REPLACE ALL OCCURRENCES OF '''' IN lv_tab WITH ''.
        REPLACE ALL OCCURRENCES OF '"'  IN lv_tab WITH ''.
        TRANSLATE lv_tab TO UPPER CASE.
        IF lv_tab IS NOT INITIAL.
          APPEND lv_tab TO lt_tables.
        ENDIF.
      ENDLOOP.
      SORT lt_tables.
      DELETE ADJACENT DUPLICATES FROM lt_tables.
    ENDIF.

    IF lt_tables IS INITIAL.
      copy_data_to_ref( EXPORTING is_data = lt_results CHANGING cr_data = er_data ).
      RETURN.
    ENDIF.

    LOOP AT lt_tables ASSIGNING <fs_table>.
      CLEAR ls_result.
      ls_result-tablename = <fs_table>.
      CLEAR ls_result-filepath.

      IF <fs_table>(1) <> 'Z' AND <fs_table>(1) <> 'Y'.
        ls_result-status  = 'FAILED'.
        ls_result-message = 'Backup is only allowed for custom tables (starting with Z or Y).'.
        APPEND ls_result TO lt_results.
        CONTINUE.
      ENDIF.

      SELECT SINGLE tabname
        FROM dd02l
        WHERE tabname  = @<fs_table>
          AND as4local = 'A'
          AND as4vers  = '0000'
        INTO @DATA(lv_tabname).

      IF sy-subrc <> 0.
        ls_result-status  = 'FAILED'.
        ls_result-message = 'Custom table does not exist in the data dictionary.'.
        APPEND ls_result TO lt_results.
        CONTINUE.
      ENDIF.

      DATA: lr_table TYPE REF TO data.
      FIELD-SYMBOLS: <fs_dyn_table> TYPE STANDARD TABLE,
                     <fs_line>      TYPE any,
                     <fs_field>     TYPE any.

      TRY.
          CREATE DATA lr_table TYPE TABLE OF (<fs_table>).
          ASSIGN lr_table->* TO <fs_dyn_table>.
        CATCH cx_sy_create_data_error INTO DATA(lx_cd).
          ls_result-status  = 'FAILED'.
          ls_result-message = |Type creation failed: { lx_cd->get_text( ) }|.
          APPEND ls_result TO lt_results.
          CONTINUE.
      ENDTRY.

      TRY.
          SELECT * FROM (<fs_table>) INTO TABLE <fs_dyn_table>.
        CATCH cx_sy_dynamic_osql_error INTO DATA(lx_osql).
          ls_result-status  = 'FAILED'.
          ls_result-message = |Error during dynamic SELECT. { lx_osql->get_text( ) }|.
          APPEND ls_result TO lt_results.
          CONTINUE.
      ENDTRY.

      DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
      TRY.
          lo_struct_descr = CAST cl_abap_structdescr(
                                  cl_abap_typedescr=>describe_by_name( <fs_table> ) ).
        CATCH cx_root INTO DATA(lx_any).
          ls_result-status  = 'FAILED'.
          ls_result-message = |Could not get structure: { lx_any->get_text( ) }|.
          APPEND ls_result TO lt_results.
          CONTINUE.
      ENDTRY.

      DATA(lt_components) = lo_struct_descr->get_components( ).
      DATA lt_excel_data TYPE TABLE OF string.
      DATA(lv_header) = REDUCE string(
        INIT h = `` FOR comp IN lt_components
        NEXT h = COND string( WHEN h IS INITIAL
                                THEN comp-name
                                ELSE h && cl_abap_char_utilities=>horizontal_tab && comp-name )
      ).
      APPEND lv_header TO lt_excel_data.

      DATA: lv_line        TYPE string,
            lv_field_value TYPE string.

      LOOP AT <fs_dyn_table> ASSIGNING <fs_line>.
        CLEAR lv_line.
        LOOP AT lt_components INTO DATA(ls_component).
          ASSIGN COMPONENT ls_component-name OF STRUCTURE <fs_line> TO <fs_field>.
          IF sy-subrc = 0.
            DATA(lo_field_descr) = ls_component-type.
            CASE lo_field_descr->kind.
              WHEN cl_abap_typedescr=>kind_elem.
                DATA(lo_elem_descr) = CAST cl_abap_elemdescr( lo_field_descr ).
                CASE lo_elem_descr->type_kind.
                  WHEN cl_abap_elemdescr=>typekind_char
                    OR cl_abap_elemdescr=>typekind_num
                    OR cl_abap_elemdescr=>typekind_date
                    OR cl_abap_elemdescr=>typekind_time
                    OR cl_abap_elemdescr=>typekind_int
                    OR cl_abap_elemdescr=>typekind_int1
                    OR cl_abap_elemdescr=>typekind_int2
                    OR cl_abap_elemdescr=>typekind_packed
                    OR cl_abap_elemdescr=>typekind_float.
                    lv_field_value = |{ <fs_field> }|.
                  WHEN OTHERS.
                    lv_field_value = `<UNSUPPORTED_TYPE>`.
                ENDCASE.
              WHEN OTHERS.
                lv_field_value = `<DEEP_TYPE>`.
            ENDCASE.
          ELSE.
            lv_field_value = ``.
          ENDIF.

          IF lv_line IS INITIAL.
            lv_line = lv_field_value.
          ELSE.
            lv_line = lv_line && cl_abap_char_utilities=>horizontal_tab && lv_field_value.
          ENDIF.
        ENDLOOP.
        APPEND lv_line TO lt_excel_data.
      ENDLOOP.

      DATA: lv_filename    TYPE string,
            lv_server_path TYPE string.

      CONCATENATE <fs_table> '_' sy-datum '_' sy-uzeit '.xls' INTO lv_filename.
      CONCATENATE lv_validated_path '/' lv_filename INTO lv_server_path.

      OPEN DATASET lv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.
        LOOP AT lt_excel_data INTO DATA(excel_line).
          TRANSFER excel_line TO lv_server_path.
        ENDLOOP.
        CLOSE DATASET lv_server_path.
        ls_result-status   = 'SUCCESS'.
        ls_result-message  = 'Backup successfully stored in AL11 directory (SAP server)'.
        ls_result-filepath = lv_server_path.
      ELSE.
        ls_result-status  = 'FAILED'.
        ls_result-message = 'Could not open dataset on server. Check path and authorizations.'.
        CLEAR ls_result-filepath.
      ENDIF.

      APPEND ls_result TO lt_results.

    ENDLOOP.

    copy_data_to_ref(
      EXPORTING is_data = lt_results
      CHANGING  cr_data = er_data
    ).

  ENDIF.

ENDMETHOD.
ENDCLASS.
