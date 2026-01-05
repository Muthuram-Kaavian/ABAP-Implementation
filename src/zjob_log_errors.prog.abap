*&---------------------------------------------------------------------*
*& Report Z_TEST_CONSISTENCY_CHECK
*&---------------------------------------------------------------------*
REPORT z_test_consistency_check.

TYPE-POOLS: vrm.
TYPE-POOLS: icon.

* Target stack metadata
DATA: lt_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
      ls_target_stack   TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str,
      lv_selected_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

* Drive from relevance result list
DATA: lt_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_tab,
      ls_check_result   TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str.

* Generic message structure for FM results
TYPES: BEGIN OF ty_consistency_result,
         msgty    TYPE symsgty,     " Message type (E, W, S, I)
         arbgb    TYPE symsgid,     " Message ID
         txtnr    TYPE symsgno,     " Message number
         msgv1    TYPE symsgv,      " Message variable 1
         msgv2    TYPE symsgv,      " Message variable 2
         msgv3    TYPE symsgv,      " Message variable 3
         msgv4    TYPE symsgv,      " Message variable 4
         message  TYPE string,      " Message text
         sitem_id TYPE string,      " Simplification item ID
         guid     TYPE guid_32,     " GUID
       END OF ty_consistency_result.

DATA: lt_cons_result TYPE TABLE OF ty_consistency_result.

* Output structure
TYPES: BEGIN OF ty_output,
         relevance        TYPE char4,    " icon code
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
         consistency      TYPE char4,    " icon from FM
         cons_messages    TYPE string,   " Consistency messages
       END OF ty_output.

DATA: lt_output TYPE TABLE OF ty_output,
      ls_output TYPE ty_output.

* For debugging
DATA: lv_fm_name TYPE string VALUE '/SDF/GEN_FUNCS_S4_CONS_EXIT'.

*--------------------------------------------------------------------*
* SELECTION SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr AS LISTBOX
             VISIBLE LENGTH 80 OBLIGATORY.
  PARAMETERS p_test  AS CHECKBOX DEFAULT 'X' USER-COMMAND test.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
* INITIALIZATION - POPULATE DROPDOWN
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM populate_dropdown.

*--------------------------------------------------------------------*
* MAIN
*--------------------------------------------------------------------*
START-OF-SELECTION.

  lv_selected_stack = p_stack.
  IF lv_selected_stack IS INITIAL.
    WRITE: / 'Error: No Stack Selected.'.
    RETURN.
  ENDIF.

  " Get relevant items
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_relevance_result_get
    EXPORTING
      iv_target_stack   = lv_selected_stack
    IMPORTING
      et_rel_chk_result = lt_check_result.

  IF lt_check_result IS INITIAL.
    WRITE: / 'No results. Run /SDF/RC_START_CHECK once for this stack, then retry.'.
    RETURN.
  ENDIF.

  " Display count
  WRITE: / 'Found', lines( lt_check_result ), 'simplification items.'.
  SKIP.

  LOOP AT lt_check_result INTO ls_check_result.
    CLEAR ls_output.

    " Basic mapping
    ls_output-id            = ls_check_result-sitem_id.
    ls_output-title         = ls_check_result-title_en.
    ls_output-business_area = ls_check_result-business_area_des.

    IF ls_check_result-lob_technology_des IS NOT INITIAL.
      ls_output-lob = ls_check_result-lob_technology_des.
    ELSE.
      ls_output-lob = ls_check_result-app_area.
    ENDIF.

    ls_output-note             = ls_check_result-buz_imp_note.
    ls_output-category         = ls_check_result-category_text.
    ls_output-component        = ls_check_result-app_components.
    ls_output-status           = ls_check_result-proc_status.
    ls_output-application_area = ls_check_result-app_area.
    ls_output-summary          = ls_check_result-summary.

    IF ls_check_result-relevant_stat IS NOT INITIAL.
      ls_output-relevance = ls_check_result-relevant_stat.
    ELSE.
      ls_output-relevance = icon_light_out.  " @5D@
    ENDIF.

    " --- Get Consistency Results ---
    PERFORM get_consistency_results
      USING    lv_selected_stack ls_check_result
      CHANGING ls_output-consistency
               ls_output-cons_messages.

    APPEND ls_output TO lt_output.

    " Display progress
    IF sy-tabix MOD 10 = 0.
      WRITE: / 'Processed', sy-tabix, 'of', lines( lt_check_result ), 'items...'.
    ENDIF.
  ENDLOOP.

  " Download to Excel
  PERFORM download_to_excel.

*&---------------------------------------------------------------------*
*&      Form  GET_CONSISTENCY_RESULTS
*&---------------------------------------------------------------------*
FORM get_consistency_results
  USING    iv_stack      TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
           is_rel        TYPE /sdf/cl_rc_chk_utility=>ty_check_result_str
  CHANGING cv_icon       TYPE char4
           cv_messages   TYPE string.

  DATA: lv_guid      TYPE guid_32,
        lv_sitem_id  TYPE string,
        lt_messages  TYPE TABLE OF bapiret2,
        lt_messages2 TYPE TABLE OF string,
        lv_msg_text  TYPE string,
        lv_index     TYPE sy-tabix.

  FIELD-SYMBOLS: <fs_guid> TYPE any,
                 <fs_msg>  TYPE bapiret2,
                 <fs_str>  TYPE any.

  " Clear results
  CLEAR: cv_icon, cv_messages, lt_messages, lt_messages2.

  " Get GUID if available
  lv_sitem_id = is_rel-sitem_id.

  ASSIGN COMPONENT 'SITEM_GUID' OF STRUCTURE is_rel TO <fs_guid>.
  IF <fs_guid> IS ASSIGNED.
    lv_guid = <fs_guid>.
    TRANSLATE lv_guid TO UPPER CASE.
  ENDIF.

  " TRY 1: Call FM with BAPIRET2 table (most common)
  TRY.
      CALL FUNCTION lv_fm_name
        EXPORTING
          iv_target_stack = iv_stack
          iv_sitem_id     = lv_sitem_id
          iv_sitem_guid   = lv_guid
        TABLES
          et_chk_result   = lt_messages
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc = 0 AND lt_messages IS NOT INITIAL.
        " Process BAPIRET2 messages
        LOOP AT lt_messages ASSIGNING <fs_msg>.
          lv_index = sy-tabix.

          " Build message text
          CONCATENATE <fs_msg>-type ': ' <fs_msg>-message INTO lv_msg_text.
          APPEND lv_msg_text TO lt_messages2.

          " Set icon based on highest severity
          IF <fs_msg>-type = 'E' OR <fs_msg>-type = 'A' OR <fs_msg>-type = 'X'.
            cv_icon = icon_red_light.  " @0A@
          ELSEIF <fs_msg>-type = 'W' AND cv_icon IS INITIAL.
            cv_icon = icon_yellow_light.  " @09@
          ELSEIF cv_icon IS INITIAL.
            cv_icon = icon_green_light.  " @08@
          ENDIF.
        ENDLOOP.
      ENDIF.

    CATCH cx_sy_dyn_call_param_missing
          cx_sy_dyn_call_illegal_type.
      " Try next method
  ENDTRY.

  " TRY 2: Call with generic table if first method failed
  IF lt_messages IS INITIAL.
    TRY.
        CALL FUNCTION lv_fm_name
          EXPORTING
            iv_target_stack = iv_stack
            iv_sitem_id     = lv_sitem_id
            iv_sitem_guid   = lv_guid
          TABLES
            et_chk_result   = lt_cons_result
          EXCEPTIONS
            OTHERS          = 2.

        IF sy-subrc = 0 AND lt_cons_result IS NOT INITIAL.
          " Process generic messages
          LOOP AT lt_cons_result ASSIGNING <fs_str>.
            lv_index = sy-tabix.

            " Try to get message type dynamically
            ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <fs_str> TO <fs_guid>.
            IF sy-subrc <> 0.
              ASSIGN COMPONENT 'TYPE' OF STRUCTURE <fs_str> TO <fs_guid>.
            ENDIF.

            " Build message text
            IF <fs_guid> IS ASSIGNED.
              CONCATENATE <fs_guid> ': Item ' lv_sitem_id INTO lv_msg_text.
              APPEND lv_msg_text TO lt_messages2.

              " Set icon
              IF <fs_guid> = 'E' OR <fs_guid> = 'A' OR <fs_guid> = 'X'.
                cv_icon = icon_red_light.
              ELSEIF <fs_guid> = 'W' AND cv_icon IS INITIAL.
                cv_icon = icon_yellow_light.
              ELSEIF cv_icon IS INITIAL.
                cv_icon = icon_green_light.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      CATCH cx_sy_dyn_call_param_missing
            cx_sy_dyn_call_illegal_type.
        " Try final method
    ENDTRY.
  ENDIF.

  " TRY 3: Call without GUID parameter (simplest signature)
  IF lt_messages IS INITIAL AND lt_cons_result IS INITIAL.
    TRY.
        CALL FUNCTION lv_fm_name
          EXPORTING
            iv_target_stack = iv_stack
            iv_sitem_id     = lv_sitem_id
          TABLES
            et_chk_result   = lt_messages
          EXCEPTIONS
            OTHERS          = 3.

        IF sy-subrc = 0 AND lt_messages IS NOT INITIAL.
          LOOP AT lt_messages ASSIGNING <fs_msg>.
            CONCATENATE <fs_msg>-type ': ' <fs_msg>-message INTO lv_msg_text.
            APPEND lv_msg_text TO lt_messages2.

            IF <fs_msg>-type = 'E' OR <fs_msg>-type = 'A' OR <fs_msg>-type = 'X'.
              cv_icon = icon_red_light.
            ELSEIF <fs_msg>-type = 'W' AND cv_icon IS INITIAL.
              cv_icon = icon_yellow_light.
            ELSEIF cv_icon IS INITIAL.
              cv_icon = icon_green_light.
            ENDIF.
          ENDLOOP.
        ENDIF.

      CATCH cx_sy_dyn_call_param_missing
            cx_sy_dyn_call_illegal_type.
        " All methods failed
    ENDTRY.
  ENDIF.

  " Combine all messages into single string
  IF lt_messages2 IS NOT INITIAL.
    CONCATENATE LINES OF lt_messages2 INTO cv_messages SEPARATED BY ' | '.
  ENDIF.

  " Set default icon if none found
  IF cv_icon IS INITIAL.
    IF cv_messages IS NOT INITIAL.
      cv_icon = icon_green_light.  " Has messages but no type
    ELSE.
      cv_icon = icon_light_out.    " No messages
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POPULATE_DROPDOWN
*&---------------------------------------------------------------------*
FORM populate_dropdown.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  CALL METHOD /sdf/cl_rc_chk_utility=>get_target_s4_version
    IMPORTING
      et_version = lt_target_stack
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    WRITE: / 'Error getting target stacks.'.
    RETURN.
  ENDIF.

  SORT lt_target_stack BY stack_number DESCENDING.

  LOOP AT lt_target_stack INTO ls_target_stack.
    CLEAR ls_value.
    ls_value-key = ls_target_stack-stack_number.
    CONCATENATE ls_target_stack-prod_ver_name
                ' [' ls_target_stack-stack_name ']'
           INTO ls_value-text.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STACK'
      values = lt_values.

  " Set default to first entry
  READ TABLE lt_target_stack INTO ls_target_stack INDEX 1.
  IF sy-subrc = 0.
    p_stack = ls_target_stack-stack_number.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_EXCEL
*&---------------------------------------------------------------------*

FORM download_to_excel.
  DATA: lv_fullpath TYPE string,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lt_excel    TYPE TABLE OF string,
        lv_row      TYPE string,
        lv_tab      TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
        lv_utf_bom  TYPE xstring.

  " Header with new column for messages
  CONCATENATE 'RELEVANCE_STATUS' lv_tab
              'ID' lv_tab
              'TITLE' lv_tab
              'LOB' lv_tab
              'BUSINESS_AREA' lv_tab
              'NOTE' lv_tab
              'CATEGORY' lv_tab
              'COMPONENT' lv_tab
              'STATUS_TEXT' lv_tab
              'APPLICATION_AREA' lv_tab
              'SUMMARY' lv_tab
              'CONSISTENCY_STATUS' lv_tab
              'CONSISTENCY_MESSAGES'
    INTO lv_row.
  APPEND lv_row TO lt_excel.

  " Rows
  LOOP AT lt_output INTO ls_output.
    " Replace tabs and newlines in messages
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab
      IN ls_output-cons_messages WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN ls_output-cons_messages WITH ' | '.
    REPLACE ALL OCCURRENCES OF `"` IN ls_output-cons_messages WITH `'`.

    CONCATENATE ls_output-relevance lv_tab
                ls_output-id lv_tab
                ls_output-title lv_tab
                ls_output-lob lv_tab
                ls_output-business_area lv_tab
                ls_output-note lv_tab
                ls_output-category lv_tab
                ls_output-component lv_tab
                ls_output-status lv_tab
                ls_output-application_area lv_tab
                ls_output-summary lv_tab
                ls_output-consistency lv_tab
                ls_output-cons_messages
      INTO lv_row.
    APPEND lv_row TO lt_excel.
  ENDLOOP.

  " Save dialog - use .xls for compatibility
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      window_title      = 'Save Consistency Check Results'
      default_extension = 'xls'  " Changed from xlsx to xls
      default_file_name = 'S4_Consistency_Results.xls'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath ).

  IF lv_fullpath IS INITIAL.
    MESSAGE 'Save cancelled by user' TYPE 'S'.
    RETURN.
  ENDIF.

  " Download as UTF-8 with BOM for Excel compatibility
  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                = lv_fullpath
      filetype                = 'ASC'        " ASCII/Text file
      codepage                = '4110'       " UTF-8
      write_bom               = 'X'          " Add UTF-8 BOM for Excel
      trunc_trailing_blanks   = 'X'          " Remove trailing blanks
      write_lf                = 'X'          " Use line feeds
    CHANGING
      data_tab                = lt_excel
    EXCEPTIONS
      OTHERS                  = 1 ).

  IF sy-subrc = 0.
    WRITE: / 'File downloaded successfully to:', lv_fullpath.
    WRITE: / 'Total records:', lines( lt_output ).
    WRITE: / 'Tip: Open this .xls file with Excel, it will open as tab-delimited text.'.
  ELSE.
    WRITE: / 'Error downloading file. RC=', sy-subrc.
  ENDIF.
ENDFORM.
