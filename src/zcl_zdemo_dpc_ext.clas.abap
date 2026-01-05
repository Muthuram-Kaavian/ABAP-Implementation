class ZCL_ZDEMO_DPC_EXT definition
  public
  inheriting from ZCL_ZDEMO_DPC
  create public .

public section.
protected section.

  methods TABLEREQUESTSET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZDEMO_DPC_EXT IMPLEMENTATION.


METHOD TABLEREQUESTSET_GET_ENTITY.

  DATA: lv_table_name TYPE string.

  " Get keys and extract table name
  DATA(lt_keys) = io_tech_request_context->get_keys( ).
  READ TABLE lt_keys INDEX 1 INTO DATA(ls_first_key).
  IF sy-subrc = 0.
    lv_table_name = ls_first_key-value.
  ELSE.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'No table name provided'.
  ENDIF.

  TRANSLATE lv_table_name TO UPPER CASE.

  " Check if table exists
  SELECT SINGLE tabname
    FROM dd02l
    INTO @DATA(lv_tabname)
    WHERE tabname = @lv_table_name.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = |Table { lv_table_name } does not exist|.
  ENDIF.

  " Get table description
  SELECT SINGLE ddtext
    FROM dd02t
    INTO @DATA(lv_ddtext)
    WHERE tabname = @lv_table_name
      AND ddlanguage = @sy-langu.
  IF sy-subrc <> 0.
    lv_ddtext = 'No description available'.
  ENDIF.

  " Get field count
  SELECT COUNT(*)
    FROM dd03l
    INTO @DATA(lv_field_count)
    WHERE tabname = @lv_table_name.

  " Prepare response - add fields that exist in your SEGW entity
  er_entity-tablename = lv_table_name.

  " Only add these if they exist in your SEGW entity:
  " er_entity-fieldcount = lv_field_count.
  " er_entity-tabledescription = lv_ddtext.
  " er_entity-fielddetails = |Table { lv_table_name } has { lv_field_count } fields|.

ENDMETHOD.
ENDCLASS.
