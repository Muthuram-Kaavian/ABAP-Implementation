class ZCL_ZSIMP_REPORT_DPC_EXT definition
  public
  inheriting from ZCL_ZSIMP_REPORT_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSIMP_REPORT_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
  " First declare TYPES, then DATA
  TYPES: BEGIN OF ty_output,
           relevance        TYPE char4,
           id               TYPE string,
           title            TYPE string,
           lob              TYPE string,
           business_area    TYPE string,
           note             TYPE string,
           category         TYPE string,
           component        TYPE string,
           status           TYPE string,
           application_area TYPE string,
           summary          TYPE string,
           consistency      TYPE char4,
         END OF ty_output.

  " Then declare ALL DATA variables
  DATA: lv_target_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr,
        lt_output       TYPE TABLE OF ty_output,
        ls_results      TYPE ZCL_ZSIMP_REPORT_MPC=>ts_checkresult,
        lv_success      TYPE abap_bool,
        lt_check_result TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
        ls_check_result TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str,
        ls_output       TYPE ty_output,
        lt_excel        TYPE TABLE OF string,
        lv_row          TYPE string,
        lv_tab          TYPE c,
        lv_excel_str    TYPE string,
        ls_param        TYPE /iwbep/s_mgw_name_value_pair.

  CASE iv_action_name.
    WHEN 'ExecuteCheck'.
      " Read import parameter
      READ TABLE it_parameter INTO ls_param WITH KEY name = 'TargetStack'.
      IF sy-subrc = 0.
        lv_target_stack = ls_param-value.
      ENDIF.

      " Validate input
      IF lv_target_stack IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = 'TargetStack is required'.
      ENDIF.

      " Execute the check and get results
      lv_success = abap_false.
      CLEAR: lt_check_result, lt_output.

      " Fetch the results from SDF utility
      TRY.
          CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
            EXPORTING
              iv_target_stack   = lv_target_stack
            IMPORTING
              et_rel_chk_result = lt_check_result.

          IF lt_check_result IS NOT INITIAL.
            " Prepare output
            LOOP AT lt_check_result INTO ls_check_result.
              CLEAR ls_output.

              ls_output-id            = ls_check_result-sitem_id.
              ls_output-title         = ls_check_result-title_en.
              ls_output-business_area = ls_check_result-business_area_des.

              " LoB Logic
              IF ls_check_result-lob_technology_des IS NOT INITIAL.
                ls_output-lob = ls_check_result-lob_technology_des.
              ELSE.
                ls_output-lob = ls_check_result-app_area.
              ENDIF.

              " Additional Fields
              ls_output-note             = ls_check_result-buz_imp_note.
              ls_output-category         = ls_check_result-category_text.
              ls_output-component        = ls_check_result-app_components.
              ls_output-status           = ls_check_result-proc_status.
              ls_output-application_area = ls_check_result-app_area.
              ls_output-summary          = ls_check_result-summary.

              " Icons
              IF ls_check_result-relevant_stat IS NOT INITIAL.
                ls_output-relevance = ls_check_result-relevant_stat.
              ELSE.
                ls_output-relevance = icon_light_out.
              ENDIF.

              ls_output-consistency = ls_check_result-consistency_stat_disp(4).
              APPEND ls_output TO lt_output.
            ENDLOOP.

            lv_success = abap_true.
          ENDIF.

        CATCH cx_root.
          lv_success = abap_false.
      ENDTRY.

      IF lv_success = abap_true AND lt_output IS NOT INITIAL.
        " Convert to Excel
        lv_tab = cl_abap_char_utilities=>horizontal_tab.

        " Create Header Row
        CONCATENATE 'RELEVANCE_STATUS' 'ID' 'TITLE' 'LOB' 'BUSINESS_AREA' 'NOTE' 'CATEGORY'
                    'COMPONENT' 'STATUS_TEXT' 'APPLICATION_AREA' 'SUMMARY' 'CONSISTENCY_STATUS'
          INTO lv_row SEPARATED BY lv_tab.
        APPEND lv_row TO lt_excel.

        " Create Data Rows
        LOOP AT lt_output INTO ls_output.
          CONCATENATE ls_output-relevance
                      ls_output-id
                      ls_output-title
                      ls_output-lob
                      ls_output-business_area
                      ls_output-note
                      ls_output-category
                      ls_output-component
                      ls_output-status
                      ls_output-application_area
                      ls_output-summary
                      ls_output-consistency
            INTO lv_row SEPARATED BY lv_tab.
          APPEND lv_row TO lt_excel.
        ENDLOOP.

        " Convert to XSTRING
        IF lt_excel IS NOT INITIAL.
          lv_excel_str = concat_lines_of( table = lt_excel sep = cl_abap_char_utilities=>cr_lf ).
          ls_results-filecontent = cl_bcs_convert=>string_to_xstring( iv_string = lv_excel_str ).
          ls_results-filename = |Simplification_Check_{ lv_target_stack }_{ sy-datum }_{ sy-uzeit }.xls|.
        ENDIF.

      ELSE.
        " No data found
        ls_results-filename = ''.
        ls_results-filecontent = ''.
      ENDIF.

      " Return only filename and filecontent
      copy_data_to_ref(
        EXPORTING
          is_data = ls_results
        CHANGING
          cr_data = er_data ).

  ENDCASE.
ENDMETHOD.
ENDCLASS.
