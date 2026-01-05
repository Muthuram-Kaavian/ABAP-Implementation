class ZCL_ZSIMP_CHECK_DPC_EXT definition
  public
  inheriting from ZCL_ZSIMP_CHECK_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSIMP_CHECK_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    " --- DATA DECLARATIONS ---
    DATA: lt_key_tab        TYPE /iwbep/t_mgw_tech_pairs, " Fixed Type
          ls_key            TYPE /iwbep/s_mgw_tech_pair,  " Fixed Type
          lv_input_name     TYPE string,
          lv_found_stack_id TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

    " Data for Lookup
    DATA: lt_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
          ls_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str.

    " Data for Results
    DATA: lt_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
          ls_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str.

    " Data for Buffer
    DATA: lv_string_buffer  TYPE string,
          lv_xstring_buffer TYPE xstring,
          lv_row            TYPE string,
          lv_tab            TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
          lv_crlf           TYPE c VALUE cl_abap_char_utilities=>cr_lf.

    " Stream Structure (Fixed: Explicit Declaration)
    DATA: ls_stream         TYPE ty_s_media_resource.

    " -------------------------------------------------------------
    " 1. Retrieve the Input Parameter (System Name) from URL Keys
    " -------------------------------------------------------------
    " Get keys returns technical pairs in this context
    lt_key_tab = io_tech_request_context->get_keys( ).

    READ TABLE lt_key_tab INTO ls_key WITH KEY name = 'TargetStack'. " Try Upper Case Key first
    IF sy-subrc <> 0.
       READ TABLE lt_key_tab INTO ls_key WITH KEY name = 'TargetStack'.
    ENDIF.

    IF sy-subrc = 0.
      lv_input_name = ls_key-value.
    ENDIF.

    TRANSLATE lv_input_name TO UPPER CASE.

    IF lv_input_name IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                  message = 'Target System Name is required'.
    ENDIF.

    " -------------------------------------------------------------
    " 2. LOOKUP LOGIC: Map Name -> Stack ID
    " -------------------------------------------------------------
    CALL METHOD /sdf/cl_rc_chk_utility=>get_target_s4_version
      IMPORTING
        et_version = lt_target_stack
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                  message = 'Error retrieving available target stacks'.
    ENDIF.

    SORT lt_target_stack BY stack_number DESCENDING.

    LOOP AT lt_target_stack INTO ls_target_stack.
      DATA(lv_compare_name) = ls_target_stack-prod_ver_name.
      TRANSLATE lv_compare_name TO UPPER CASE.

      IF lv_compare_name CP lv_input_name.
        lv_found_stack_id = ls_target_stack-stack_number.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_found_stack_id IS INITIAL.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                  message = 'Target System Name not found in available stacks'.
    ENDIF.

    " -------------------------------------------------------------
    " 3. Call Standard Logic using the Found ID
    " -------------------------------------------------------------
    CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
      EXPORTING
        iv_target_stack   = lv_found_stack_id
      IMPORTING
        et_rel_chk_result = lt_check_result.

    IF lt_check_result IS INITIAL.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                  message = 'No results found. Run /SDF/RC_START_CHECK manually.'.
    ENDIF.

    " -------------------------------------------------------------
    " 4. Build Excel Buffer
    " -------------------------------------------------------------
    CONCATENATE 'RELEVANCE' 'ID' 'TITLE' 'LOB' 'BUSINESS_AREA' 'NOTE' 'CATEGORY'
                'COMPONENT' 'STATUS' 'APP_AREA' 'SUMMARY' 'CONSISTENCY'
           INTO lv_row SEPARATED BY lv_tab.
    CONCATENATE lv_string_buffer lv_row lv_crlf INTO lv_string_buffer.

    LOOP AT lt_check_result INTO ls_check_result.
      CLEAR lv_row.

      DATA(lv_lob) = ls_check_result-lob_technology_des.
      IF lv_lob IS INITIAL. lv_lob = ls_check_result-app_area. ENDIF.

      DATA(lv_rel) = ls_check_result-relevant_stat.
      IF lv_rel IS INITIAL. lv_rel = '@5D@'. ENDIF.

      CONCATENATE lv_rel
                  ls_check_result-sitem_id
                  ls_check_result-title_en
                  lv_lob
                  ls_check_result-business_area_des
                  ls_check_result-buz_imp_note
                  ls_check_result-category_text
                  ls_check_result-app_components
                  ls_check_result-proc_status
                  ls_check_result-app_area
                  ls_check_result-summary
                  ls_check_result-consistency_stat_disp(4)
             INTO lv_row SEPARATED BY lv_tab.

      CONCATENATE lv_string_buffer lv_row lv_crlf INTO lv_string_buffer.
    ENDLOOP.

    " -------------------------------------------------------------
    " 5. Convert and Set Stream (Fixed: Syntax)
    " -------------------------------------------------------------
    TRY.
        lv_xstring_buffer = cl_bcs_convert=>string_to_xstring( iv_string = lv_string_buffer ).
      CATCH cx_bcs.
        " Fallback if BCS class fails (rare)
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING text   = lv_string_buffer
          IMPORTING buffer = lv_xstring_buffer.
    ENDTRY.

    ls_stream-value     = lv_xstring_buffer.
    ls_stream-mime_type = 'application/vnd.ms-excel'.

    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).

    " Set Filename Header
    DATA: ls_lheader TYPE ihttpnvp.
    ls_lheader-name  = 'Content-Disposition'.
    CONCATENATE 'attachment; filename="Check_Results_' lv_input_name '.xls"' INTO ls_lheader-value.

    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_lheader ).

  ENDMETHOD.
ENDCLASS.
