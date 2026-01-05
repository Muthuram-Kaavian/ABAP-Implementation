*&---------------------------------------------------------------------*
*& Report  Z_SHOW_CONS_XML
*&---------------------------------------------------------------------*
*& Show raw XML (CHK_CLASS_RESULT_XSTR) for consistency check items
*& Choose target stack from a listbox
*&---------------------------------------------------------------------*
REPORT z_show_cons_xml.

*---------------------------------------------------------------------*
* DATA
*---------------------------------------------------------------------*
" Target stacks from utility class
DATA: gt_target_stacks TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_tab,
      gs_target_stack  TYPE /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str.

" Selected stack number (internal key)
DATA: gv_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

" Consistency check results
DATA: lt_result TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_tab,
      ls_raw    TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_str.

" Conversion XSTRING -> STRING
DATA: lv_xml  TYPE string,
      lv_xstr TYPE xstring.

" Listbox helper
DATA: gt_list TYPE vrm_values,
      gs_list LIKE LINE OF gt_list.

*---------------------------------------------------------------------*
* SELECTION SCREEN
*  TEXT-001: 'Target S/4HANA stack'
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tstack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
            AS LISTBOX VISIBLE LENGTH 60 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM get_target_stacks.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_listbox.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  gv_stack = p_tstack.

  WRITE: / 'to check the consistency check'.
  SKIP.

  " Get consistency results for selected stack
  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_consistency_result_get
    EXPORTING
      iv_target_stack    = gv_stack
    IMPORTING
      et_cons_chk_result = lt_result.

  IF lt_result IS INITIAL.
    WRITE: / 'No results found for target stack: ', gv_stack.
    EXIT.
  ENDIF.

  LOOP AT lt_result INTO ls_raw.

    IF ls_raw-chk_clas_result_xstr IS NOT INITIAL.

      lv_xstr = ls_raw-chk_clas_result_xstr.
      CLEAR lv_xml.

      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring = lv_xstr
        IMPORTING
          ex_string  = lv_xml.

      WRITE: / 'XML FOR CHECK ITEM:', ls_raw-sitem_id.
      SKIP.
      WRITE: / lv_xml.
      SKIP 2.

    ENDIF.

  ENDLOOP.

*---------------------------------------------------------------------*
* Get list of target stacks
*---------------------------------------------------------------------*
FORM get_target_stacks.

  CLEAR gt_target_stacks.

  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING
      et_version = gt_target_stacks
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).

  IF sy-subrc <> 0.
    MESSAGE 'Cannot read S/4HANA target stacks' TYPE 'E'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Fill listbox for P_TSTACK
*---------------------------------------------------------------------*
FORM fill_listbox.

  CLEAR gt_list.

  LOOP AT gt_target_stacks INTO gs_target_stack.

    CLEAR gs_list.
    gs_list-key = gs_target_stack-stack_number.

    CONCATENATE gs_target_stack-prod_ver_number
                gs_target_stack-prod_desc
                '['
                gs_target_stack-stack_name
                ']'
           INTO gs_list-text
           SEPARATED BY space.

    APPEND gs_list TO gt_list.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TSTACK'
      values = gt_list.

ENDFORM.
