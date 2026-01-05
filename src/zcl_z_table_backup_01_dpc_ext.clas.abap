class ZCL_Z_TABLE_BACKUP_01_DPC_EXT definition
  public
  inheriting from ZCL_Z_TABLE_BACKUP_01_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_TABLE_BACKUP_01_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

  " Deep insert: BackupJob (header) with ToItems[] -> returns ToResults[]

  DATA lx_any   TYPE REF TO cx_root.
  DATA lx_busi  TYPE REF TO /iwbep/cx_mgw_busi_exception.
  DATA lv_msg   TYPE string.
  DATA lv_msg_c TYPE c LENGTH 255.  " <-- char-like buffer for message text

  TRY.

      CASE iv_entity_set_name.

        WHEN 'BackupJobs' OR 'BackupJobSet'.

          "=== Local OData-mirroring types ===
          TYPES: BEGIN OF ty_item_in,
                   tablename TYPE tabname,
                 END OF ty_item_in.
          TYPES: tt_item_in TYPE STANDARD TABLE OF ty_item_in WITH EMPTY KEY.

          TYPES: BEGIN OF ty_result_out,
                   tablename TYPE tabname,
                   status    TYPE c LENGTH 1,
                   message   TYPE string,
                   filepath  TYPE string,
                 END OF ty_result_out.
          TYPES: tt_result_out TYPE STANDARD TABLE OF ty_result_out WITH EMPTY KEY.

          TYPES: BEGIN OF ty_job_in,
                   toitems TYPE tt_item_in,              " nav: ToItems (input)
                 END OF ty_job_in.

          TYPES: BEGIN OF ty_job_out,
                   jobid       TYPE sysuuid_c,           " C32 GUID
                   requestedat TYPE timestampl,
                   finishedat  TYPE timestampl,
                   total       TYPE i,
                   success     TYPE i,
                   failed      TYPE i,
                   toresults   TYPE tt_result_out,       " nav: ToResults (output)
                 END OF ty_job_out.

          TYPES: tt_tabnames TYPE STANDARD TABLE OF tabname WITH EMPTY KEY.

          "=== Work vars ===
          DATA ls_job_in      TYPE ty_job_in.
          DATA ls_job_out     TYPE ty_job_out.
          DATA lt_tabnames    TYPE tt_tabnames.
          DATA lt_results_int TYPE zcl_table_backup=>tt_results.
          DATA lv_jobid       TYPE sysuuid_c.
          DATA ls_item        TYPE ty_item_in.
          DATA lv_t           TYPE tabname.
          DATA lv_success     TYPE i.
          DATA ls_rout        TYPE ty_result_out.
          FIELD-SYMBOLS <r>   TYPE zcl_table_backup=>ty_result.

          " 1) Read payload
          io_data_provider->read_entry_data( IMPORTING es_data = ls_job_in ).

          " 2) Collect & validate table names
          LOOP AT ls_job_in-toitems INTO ls_item.
            lv_t = ls_item-tablename.
            TRANSLATE lv_t TO UPPER CASE.
            CONDENSE   lv_t.
            IF lv_t IS NOT INITIAL.
              APPEND lv_t TO lt_tabnames.
            ENDIF.
          ENDLOOP.

          IF lt_tabnames IS INITIAL.
            mo_context->get_message_container( )->add_message_text_only(
              iv_msg_type = 'E'
              iv_msg_text = 'No valid TableName entries in ToItems' ).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
          ENDIF.

          " 3) Execute backups via utility class
          lt_results_int = zcl_table_backup=>backup_tables( lt_tabnames ).

          " 4) Header fields
          TRY.
              lv_jobid = cl_system_uuid=>create_uuid_c32_static( ).
            CATCH cx_uuid_error.
              CALL FUNCTION 'GUID_CREATE'
                IMPORTING ev_guid_32 = lv_jobid.
          ENDTRY.
          ls_job_out-jobid = lv_jobid.

          GET TIME STAMP FIELD ls_job_out-requestedat.

          ls_job_out-total = lines( lt_results_int ).

          " count successes (no REDUCE for older releases)
          lv_success = 0.
          LOOP AT lt_results_int ASSIGNING <r>.
            IF <r>-status = 'S'.
              lv_success = lv_success + 1.
            ENDIF.
          ENDLOOP.
          ls_job_out-success = lv_success.
          ls_job_out-failed  = ls_job_out-total - ls_job_out-success.

          GET TIME STAMP FIELD ls_job_out-finishedat.

          " 5) Map internal -> ToResults[]
          LOOP AT lt_results_int ASSIGNING <r>.
            CLEAR ls_rout.
            ls_rout-tablename = <r>-tablename.
            ls_rout-status    = <r>-status.
            ls_rout-message   = <r>-message.
            ls_rout-filepath  = <r>-filepath.
            APPEND ls_rout TO ls_job_out-toresults.
          ENDLOOP.

          " 6) Return deep entity
          copy_data_to_ref(
            EXPORTING is_data = ls_job_out
            CHANGING  cr_data = er_deep_entity ).

        WHEN OTHERS.

          " Pass-through for other sets (io_expand often mandatory)
          super->/iwbep/if_mgw_appl_srv_runtime~create_deep_entity(
            EXPORTING
              iv_entity_name         = iv_entity_name
              iv_entity_set_name     = iv_entity_set_name
              iv_source_name         = iv_source_name
              it_key_tab             = it_key_tab
              it_navigation_path     = it_navigation_path
              io_data_provider       = io_data_provider
              io_expand              = io_expand
            IMPORTING
              er_deep_entity         = er_deep_entity ).

      ENDCASE.

     CATCH /iwbep/cx_mgw_busi_exception INTO lx_busi.
      RAISE EXCEPTION lx_busi.

    CATCH cx_root INTO lx_any.
      DATA lv_msg_bapi TYPE bapi_msg.   " char(220)

      lv_msg      = lx_any->if_message~get_text( ).
      lv_msg_bapi = lv_msg.             " convert STRING -> C(220)

      mo_context->get_message_container( )->add_message_text_only(
        iv_msg_type = 'E'
        iv_msg_text = lv_msg_bapi ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
  ENDTRY.




ENDMETHOD.
ENDCLASS.
