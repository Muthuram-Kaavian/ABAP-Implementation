class ZCL_ZODATA_HELLO_DPC_EXT definition
  public
  inheriting from ZCL_ZODATA_HELLO_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZODATA_HELLO_DPC_EXT IMPLEMENTATION.


METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.

  DATA: ls_input_data TYPE zcl_zodata_hello_mpc=>ts_hello,
        ls_response   TYPE zcl_zodata_hello_mpc=>ts_hello.

  " 1. GET THE NAME FROM REQUEST BODY
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).

  " 2. PROCESS THE NAME - APPEND "HELLO"
  IF ls_input_data-name IS NOT INITIAL.
    ls_response-name = |Hello { ls_input_data-name }|.
  ELSE.
    ls_response-name = 'Hello Guest'.
  ENDIF.

  " 3. RETURN THE PROCESSED RESPONSE
  copy_data_to_ref(
    EXPORTING
      is_data = ls_response
    CHANGING
      cr_data = er_entity
  ).

ENDMETHOD.


METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.

  DATA: ls_entity TYPE zcl_zodata_hello_mpc=>ts_hello,
        lt_entity TYPE TABLE OF zcl_zodata_hello_mpc=>ts_hello.

  " Try to get filter for Name field
  TRY.
      DATA(lo_filter) = io_tech_request_context->get_filter( ).
      DATA(lt_filters) = lo_filter->get_filter_select_options( ).

      READ TABLE lt_filters WITH KEY property = 'NAME' INTO DATA(ls_name_filter).
      IF sy-subrc = 0.
        ls_entity-name = ls_name_filter-select_options[ 1 ]-low.
      ELSE.
        ls_entity-name = 'Guest'.
      ENDIF.
    CATCH cx_root.
      ls_entity-name = 'Guest'.
  ENDTRY.

  APPEND ls_entity TO lt_entity.

  " Return the data - CORRECTED PARAMETER NAME
  copy_data_to_ref(
    EXPORTING
      is_data = lt_entity
    CHANGING
      cr_data = er_entityset    " ← Changed from et_entityset to er_entityset
  ).

ENDMETHOD.
ENDCLASS.
