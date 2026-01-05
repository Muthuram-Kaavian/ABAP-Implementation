REPORT z_execute.

TYPE-POOLS: lvc.  " for lvc_fname, lvc_title

*----------------------------------------------------------------------
* Types
*----------------------------------------------------------------------
TYPES: BEGIN OF ty_row,
         relevance        TYPE c LENGTH 1,  " R / N
         last_cons        TYPE c LENGTH 1,  " S / W / E / ' '
         exemption        TYPE c LENGTH 1,  " X or ' '
         sitem_id         TYPE string,      " SI22, ...
         title            TYPE string,      " S4TWL - ...
         lob_technology   TYPE string,      " LoB/Technology
         business_area    TYPE string,      " Business Area
       END OF ty_row.

DATA gt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

*----------------------------------------------------------------------
* Stack selection
*----------------------------------------------------------------------
DATA: gt_target_stacks TYPE TABLE OF /sdf/cl_rc_chk_utility=>ty_conv_target_stack_str,
      gv_target_stack  TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_tstack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr
           AS LISTBOX VISIBLE LENGTH 60 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
" Create TEXT-001 = 'Target SAP S/4HANA Version' in Text Elements.

INITIALIZATION.
  PERFORM get_target_stacks.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_stack_listbox.

START-OF-SELECTION.
  gv_target_stack = p_tstack.
  PERFORM run_consistency_foreground.
  PERFORM fetch_results_into_gt_rows.
  PERFORM show_alv.

*----------------------------------------------------------------------
* Get target stacks from utility class
*----------------------------------------------------------------------
FORM get_target_stacks.
  /sdf/cl_rc_chk_utility=>get_target_s4_version(
    IMPORTING et_version = gt_target_stacks
    EXCEPTIONS error = 1 OTHERS = 2 ).
ENDFORM.

FORM fill_stack_listbox.
  DATA: lt_list TYPE vrm_values,
        ls_list LIKE LINE OF lt_list.

  LOOP AT gt_target_stacks INTO DATA(ls).
    CLEAR ls_list.
    ls_list-key = ls-stack_number.
    CONCATENATE ls-prod_ver_name '[' ls-stack_name ']' INTO ls_list-text SEPARATED BY space.
    APPEND ls_list TO lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id = 'P_TSTACK' values = lt_list.
ENDFORM.

*----------------------------------------------------------------------
* Try to execute "Check Consistency for All" in foreground (optional)
*----------------------------------------------------------------------
FORM run_consistency_foreground.
  " IF FOUND is valid only for PERFORM (forms), not for CALL FUNCTION.
  PERFORM chk_consistency_4_all_items IN PROGRAM /sdf/rc_start_check IF FOUND.
ENDFORM.

*----------------------------------------------------------------------
* Fetch results (tries known APIs, else fallback)
*----------------------------------------------------------------------
FORM fetch_results_into_gt_rows.
  DATA lt_any TYPE STANDARD TABLE OF ty_row.

  PERFORM try_api__rc_get_result_list      CHANGING lt_any.
  IF lt_any IS INITIAL.
    PERFORM try_api__s4_rel_chk_read       CHANGING lt_any.
  ENDIF.
  IF lt_any IS INITIAL.
    PERFORM try_api__gen_funcs_results_get CHANGING lt_any.
  ENDIF.
  IF lt_any IS INITIAL.
    PERFORM build_fallback_demo            CHANGING lt_any.
  ENDIF.

  gt_rows = lt_any.
ENDFORM.

*----------------------------------------------------------------------
* TRY API #1 (stub) – replace with your real FM call if available
*----------------------------------------------------------------------
FORM try_api__rc_get_result_list CHANGING ct_rows TYPE STANDARD TABLE.
  " Example of a correct static call if your system has this FM:
  " TYPES: BEGIN OF ty_src,  " adjust to FM's export table line type
  "          relevance       TYPE c,
  "          last_con_status TYPE c,
  "          exemption       TYPE c,
  "          sitem_id        TYPE string,
  "          title           TYPE string,
  "          lob_technology  TYPE string,
  "          business_area   TYPE string,
  "        END OF ty_src.
  " DATA lt_src TYPE STANDARD TABLE OF ty_src.
  " CALL FUNCTION '/SDF/RC_GET_RESULT_LIST'
  "   EXPORTING iv_target_stack = gv_target_stack
  "   TABLES    et_results      = lt_src
  "   EXCEPTIONS OTHERS         = 1.
  " IF sy-subrc = 0.
  "   PERFORM map_any_to_ty_row TABLES lt_src CHANGING ct_rows.
  " ENDIF.
ENDFORM.

*----------------------------------------------------------------------
* TRY API #2 (stub)
*----------------------------------------------------------------------
FORM try_api__s4_rel_chk_read CHANGING ct_rows TYPE STANDARD TABLE.
  " Replace stub with your FM if present, similar to the example above.
ENDFORM.

*----------------------------------------------------------------------
* TRY API #3 (stub)
*----------------------------------------------------------------------
FORM try_api__gen_funcs_results_get CHANGING ct_rows TYPE STANDARD TABLE.
  " Replace stub with your FM if present, similar to the example above.
ENDFORM.

*----------------------------------------------------------------------
* Map "any" result structure to our ty_row (handles common names)
*----------------------------------------------------------------------
FORM map_any_to_ty_row TABLES it_any CHANGING ct_rows TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa> TYPE any.
  DATA ls TYPE ty_row.

  LOOP AT it_any ASSIGNING <wa>.
    CLEAR ls.
    PERFORM move_if_exists USING <wa> 'RELEVANCE'       CHANGING ls-relevance.
    PERFORM move_if_exists USING <wa> 'LAST_CON_STATUS' CHANGING ls-last_cons.
    PERFORM move_if_exists USING <wa> 'LAST_CONS'       CHANGING ls-last_cons.
    PERFORM move_if_exists USING <wa> 'EXEMPTION'       CHANGING ls-exemption.
    PERFORM move_if_exists USING <wa> 'SITEM_ID'        CHANGING ls-sitem_id.
    PERFORM move_if_exists USING <wa> 'ID'              CHANGING ls-sitem_id.
    PERFORM move_if_exists USING <wa> 'TITLE'           CHANGING ls-title.
    PERFORM move_if_exists USING <wa> 'LOB_TECHNOLOGY'  CHANGING ls-lob_technology.
    PERFORM move_if_exists USING <wa> 'LOB'             CHANGING ls-lob_technology.
    PERFORM move_if_exists USING <wa> 'BUSINESS_AREA'   CHANGING ls-business_area.
    PERFORM move_if_exists USING <wa> 'BUS_AREA'        CHANGING ls-business_area.
    APPEND ls TO ct_rows.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------
* Move component only if it exists in ANY structure
*----------------------------------------------------------------------
FORM move_if_exists USING    ps_any  TYPE any
                             iv_comp TYPE lvc_fname
                    CHANGING pv_val  TYPE any.
  FIELD-SYMBOLS <comp> TYPE any.
  ASSIGN COMPONENT iv_comp OF STRUCTURE ps_any TO <comp>.
  IF sy-subrc = 0 AND <comp> IS ASSIGNED.
    pv_val = <comp>.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------
* Fallback demo data (to validate ALV layout)
*----------------------------------------------------------------------
FORM build_fallback_demo CHANGING ct_rows TYPE STANDARD TABLE.
  DATA ls TYPE ty_row.
  CLEAR ct_rows.

  ls-relevance = 'R'. ls-last_cons = 'S'. ls-exemption = space.
  ls-sitem_id = 'SI22'. ls-title = 'S4TWL - GENERIC_CHECKS'.
  ls-lob_technology = 'IT Management'. ls-business_area = 'Admin'.
  APPEND ls TO ct_rows.

  ls-relevance = 'R'. ls-last_cons = 'W'. ls-exemption = space.
  ls-sitem_id = 'SI07'. ls-title = 'S4TWL - Logistics_TM'.
  ls-lob_technology = 'Supply Chain'. ls-business_area = 'Delivery'.
  APPEND ls TO ct_rows.

  ls-relevance = 'R'. ls-last_cons = 'E'. ls-exemption = space.
  ls-sitem_id = 'SI10_FIN_General'. ls-title = 'S4TWL - DMEE'.
  ls-lob_technology = 'Finance'. ls-business_area = 'Finance'.
  APPEND ls TO ct_rows.
ENDFORM.

*----------------------------------------------------------------------
* Display ALV like the RC list
*----------------------------------------------------------------------
FORM show_alv.
  DATA: lr_alv  TYPE REF TO cl_salv_table,
        lr_cols TYPE REF TO cl_salv_columns_table,
        lv_head TYPE lvc_title.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lr_alv
        CHANGING  t_table      = gt_rows ).

      lr_cols = lr_alv->get_columns( ).
      lr_cols->set_optimize( abap_true ).

      PERFORM set_col_text USING lr_cols 'RELEVANCE'       'Relevance'   'Relevance'       'Relevance'.
      PERFORM set_col_text USING lr_cols 'LAST_CONS'       'Last Cons.'  'Last Cons.'      'Last Cons.'.
      PERFORM set_col_text USING lr_cols 'LAST_CON_STATUS' 'Last Cons.'  'Last Cons.'      'Last Cons.'.
      PERFORM set_col_text USING lr_cols 'EXEMPTION'       'Exempt.'     'Exemption'       'Exemption'.
      PERFORM set_col_text USING lr_cols 'SITEM_ID'        'ID'          'ID'              'ID'.
      PERFORM set_col_text USING lr_cols 'TITLE'           'Title'       'Title'           'Title'.
      PERFORM set_col_text USING lr_cols 'LOB_TECHNOLOGY'  'LoB/Tech.'   'LoB/Technology'  'LoB/Technology'.
      PERFORM set_col_text USING lr_cols 'BUSINESS_AREA'   'Bus.Area'    'Business Area'   'Business Area'.

      CONCATENATE 'Results - Target Stack' gv_target_stack INTO lv_head SEPARATED BY space.
      lr_alv->get_display_settings( )->set_list_header( lv_head ).

      lr_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*----------------------------------------------------------------------
* Column text helper
*----------------------------------------------------------------------
FORM set_col_text USING    ir_cols     TYPE REF TO cl_salv_columns_table
                           iv_colname  TYPE lvc_fname
                           iv_short    TYPE scrtext_s
                           iv_medium   TYPE scrtext_m
                           iv_long     TYPE scrtext_l.
  DATA lr_col TYPE REF TO cl_salv_column.
  TRY.
      lr_col ?= ir_cols->get_column( iv_colname ).
      lr_col->set_short_text(  iv_short  ).
      lr_col->set_medium_text( iv_medium ).
      lr_col->set_long_text(   iv_long   ).
    CATCH cx_salv_not_found.
  ENDTRY.
ENDFORM.
