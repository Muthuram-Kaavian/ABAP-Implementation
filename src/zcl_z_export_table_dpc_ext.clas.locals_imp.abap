*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS zcl_z_export_table_dpc DEFINITION
  PUBLIC
  INHERITING FROM zcl_z_export_table_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      /iwbep/if_mgw_appl_srv_runtime~get_entityset REDEFINITION,
      /iwbep/if_mgw_appl_srv_runtime~get_entity REDEFINITION.

  PRIVATE SECTION.
    " Add these method declarations:
    METHODS:
      get_all_tables
        CHANGING
          ct_tables TYPE TABLE OF tabname,

      check_table_exists
        IMPORTING
          iv_tabname TYPE tabname
        CHANGING
          cs_result  TYPE zcl_z_export_table_mpc=>ts_z_export_table,

      download_table_to_server
        IMPORTING
          iv_tabname TYPE tabname
        CHANGING
          cs_result  TYPE zcl_z_export_table_mpc=>ts_z_export_table.

ENDCLASS.
