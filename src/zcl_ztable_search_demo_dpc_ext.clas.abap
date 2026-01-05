class ZCL_ZTABLE_SEARCH_DEMO_DPC_EXT definition
  public
  inheriting from ZCL_ZTABLE_SEARCH_DEMO_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZTABLE_SEARCH_DEMO_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  " The EXECUTE_ACTION method can handle multiple Function Imports.
  " We must first check which action was called by the URL.
  IF iv_action_name = 'GetTables'.

    " Declare a variable to hold the input search pattern.
    DATA lv_search_pattern TYPE string.

    " Declare an internal table with the same structure as our OData Entity Type.
    " This will hold the final list of tables to be returned.
    DATA lt_entityset TYPE zcl_ztable_search_demo_mpc=>tt_table.

    "--------------------------------------------------------------------
    " 1. Read the input parameter provided in the URL
    "--------------------------------------------------------------------
    " The framework passes all function import parameters in the IT_PARAMETER table.
    READ TABLE it_parameter INTO DATA(ls_parameter) WITH KEY name = 'SearchPattern'.
    IF sy-subrc = 0.
      " The value from the URL is in the 'VALUE' component of the table row.
      lv_search_pattern = ls_parameter-value.
    ENDIF.

    "--------------------------------------------------------------------
    " 2. Validate the input
    "--------------------------------------------------------------------
    " Ensure we have a valid pattern to search for. If not, exit gracefully.
    IF lv_search_pattern IS INITIAL.
      " You could also raise an exception here for better error handling.
      " For example: RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception ...
      RETURN.
    ENDIF.

    "--------------------------------------------------------------------
    " 3. Select data from the database based on the search pattern
    "--------------------------------------------------------------------
    " DD02L is the SAP Data Dictionary table that stores table definitions.
    SELECT tabname
      FROM dd02l
      INTO TABLE @DATA(lt_dd02l)
      WHERE tabname LIKE @lv_search_pattern
        AND as4local = 'A'. " 'A' for Active objects

    "--------------------------------------------------------------------
    " 4. Prepare the data for the OData response
    "--------------------------------------------------------------------
    IF sy-subrc = 0.
      " The CORRESPONDING operator is a clean way to move data between
      " structures/tables that have identically named fields (in this case, 'TABNAME').
      lt_entityset = CORRESPONDING #( lt_dd02l ).
    ENDIF.

    "--------------------------------------------------------------------
    " 5. Pass the results back to the SAP Gateway framework
    "--------------------------------------------------------------------
    " Unlike GET_ENTITYSET, you do not fill ET_ENTITYSET directly.
    " Instead, you must use the 'copy_data_to_ref' helper method to
    " assign your final data to the generic ER_DATA reference.
    copy_data_to_ref(
      EXPORTING
        is_data = lt_entityset " Your final, structured internal table
      CHANGING
        cr_data = er_data      " The generic data reference for the response
    ).

  ENDIF.

ENDMETHOD.
ENDCLASS.
