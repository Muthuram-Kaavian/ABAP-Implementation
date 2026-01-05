class ZCL_Z_ODATA_TABLE_SERV_DPC_EXT definition
  public
  inheriting from ZCL_Z_ODATA_TABLE_SERV_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_ODATA_TABLE_SERV_DPC_EXT IMPLEMENTATION.


METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.

  DATA: ls_request  TYPE zcl_z_odata_table_serv_mpc=>ts_z_table_data,
        ls_response TYPE zcl_z_odata_table_serv_mpc=>ts_z_table_data.

  " 1. GET TABLE NAME FROM REQUEST BODY
  io_data_provider->read_entry_data( IMPORTING es_data = ls_request ).

  " 2. CHECK IF TABLE EXISTS
  SELECT SINGLE tabname FROM dd02l
    INTO @DATA(lv_tabname)
    WHERE tabname = @ls_request-tablename
      AND as4local = 'A'.

  IF sy-subrc <> 0.
    " Table doesn't exist
    ls_response-tablename = ls_request-tablename.
    ls_response-data = 'Table does not exist'.
  ELSE.
    " Table exists
    ls_response-tablename = ls_request-tablename.
    ls_response-data = |Table { ls_request-tablename } exists and is active|.
  ENDIF.

  " 3. RETURN THE RESPONSE
  copy_data_to_ref(
    EXPORTING
      is_data = ls_response
    CHANGING
      cr_data = er_entity
  ).

ENDMETHOD.
ENDCLASS.
