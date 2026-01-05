class ZCL_ZRC_ERRLOG_DPC_EXT definition
  public
  inheriting from ZCL_ZRC_ERRLOG_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZRC_ERRLOG_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

  "--------------------------------------------------------------------
  " Serve only our entity set
  "--------------------------------------------------------------------
  DATA lv_entityset TYPE string.

  lv_entityset = io_tech_request_context->get_entity_set_name( ).
  TRANSLATE lv_entityset TO UPPER CASE.

  IF lv_entityset <> 'ERRORMESSAGESSET'.
    RETURN.
  ENDIF.

  "--------------------------------------------------------------------
  " Local row type for the OData entity
  "--------------------------------------------------------------------
  TYPES: BEGIN OF ty_em,
           system     TYPE string,
           timestamp  TYPE timestampl,
           msgtype    TYPE char1,     " E / W / I / S / ...
           msgid      TYPE symsgid,
           msgno      TYPE symsgno,
           msgtext    TYPE string,
           client     TYPE mandt,
           lognumber  TYPE string,    " SItem ID or similar
         END OF ty_em.

  DATA: lt_em TYPE TABLE OF ty_em,
        ls_em TYPE ty_em.

  "--------------------------------------------------------------------
  " Types / data for Simplification Item Check framework
  "--------------------------------------------------------------------
  DATA lv_target_stack TYPE /sdf/cl_rc_chk_utility=>ty_bormnr.
  DATA lt_header_info  TYPE salv_wd_t_string.
  DATA ls_header_info  TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_header_str.

  " Row type of the consistency result (non-generic)
  TYPES ty_cons_row TYPE /sdf/cl_rc_chk_utility=>ty_consis_chk_result_str.

  DATA: lt_cons_result TYPE STANDARD TABLE OF ty_cons_row,
        ls_cons_result TYPE ty_cons_row.

  "--------------------------------------------------------------------
  " Call framework API to get last consistency check result
  " (backend of /SDF/RC_START_CHECK "Display Last Check Result")
  "--------------------------------------------------------------------
  CLEAR: lv_target_stack, lt_cons_result, lt_header_info, ls_header_info.

  CALL METHOD /sdf/cl_rc_chk_utility=>sitem_consistency_result_get
    EXPORTING
      iv_target_stack     = lv_target_stack   " initial = last/default stack
    IMPORTING
      et_cons_chk_result  = lt_cons_result
      et_cons_header_info = lt_header_info
      es_header_info      = ls_header_info.

  IF lt_cons_result IS INITIAL.
    " No result available (check not run yet or no data)
    RETURN.
  ENDIF.

  "--------------------------------------------------------------------
  " Read ALL items (E, W, S, I...) generically
  "--------------------------------------------------------------------
  FIELD-SYMBOLS: <res_line> TYPE any,
                 <fs_sev>   TYPE any,
                 <fs_msg>   TYPE any,
                 <fs_sid>   TYPE any.

  DATA lv_sev     TYPE char1.
  DATA lv_msgtext TYPE string.
  DATA lv_sid     TYPE string.

  LOOP AT lt_cons_result ASSIGNING <res_line>.

    CLEAR: <fs_sev>, <fs_msg>, <fs_sid>.
    CLEAR: lv_sev, lv_msgtext, lv_sid.

    " --- Severity: look for MESSAGE_SEVERITY or SEVERITY ---
    ASSIGN COMPONENT 'MESSAGE_SEVERITY' OF STRUCTURE <res_line> TO <fs_sev>.
    IF sy-subrc <> 0.
      ASSIGN COMPONENT 'SEVERITY' OF STRUCTURE <res_line> TO <fs_sev>.
    ENDIF.

    IF <fs_sev> IS ASSIGNED.
      lv_sev = <fs_sev>.
    ELSE.
      lv_sev = space.   " no severity field -> treat as blank
    ENDIF.

    " --- Message text: try several common field names ---
    ASSIGN COMPONENT 'MESSAGE_TEXT' OF STRUCTURE <res_line> TO <fs_msg>.
    IF sy-subrc <> 0.
      ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <res_line> TO <fs_msg>.
    ENDIF.
    IF sy-subrc <> 0.
      ASSIGN COMPONENT 'DETAIL_TEXT' OF STRUCTURE <res_line> TO <fs_msg>.
    ENDIF.

    IF <fs_msg> IS ASSIGNED.
      lv_msgtext = <fs_msg>.
    ELSE.
      lv_msgtext = |(no message text field found in result row)|.
    ENDIF.

    " --- Simplification Item ID / key, if available ---
    ASSIGN COMPONENT 'SITEM_ID' OF STRUCTURE <res_line> TO <fs_sid>.
    IF sy-subrc <> 0.
      ASSIGN COMPONENT 'SITEM' OF STRUCTURE <res_line> TO <fs_sid>.
    ENDIF.

    IF <fs_sid> IS ASSIGNED.
      lv_sid = <fs_sid>.
    ELSE.
      CLEAR lv_sid.
    ENDIF.

    "------------------------------------------------------------------
    " Map to OData row (NO severity filter now – return all)
    "------------------------------------------------------------------
    CLEAR ls_em.
    ls_em-system     = sy-sysid.
    GET TIME STAMP FIELD ls_em-timestamp.
    ls_em-msgtype    = lv_sev.          " E / W / I / S / ...
    ls_em-msgid      = 'S4RC'.          " arbitrary
    ls_em-msgno      = '000'.           " arbitrary
    ls_em-msgtext    = lv_msgtext.
    ls_em-client     = sy-mandt.
    ls_em-lognumber  = lv_sid.

    APPEND ls_em TO lt_em.

  ENDLOOP.

  IF lt_em IS INITIAL.
    " Result exists but we couldn't map anything
    RETURN.
  ENDIF.

  "--------------------------------------------------------------------
  " Return entityset
  "--------------------------------------------------------------------
  me->copy_data_to_ref(
    EXPORTING
      is_data = lt_em
    CHANGING
      cr_data = er_entityset ).

ENDMETHOD.
ENDCLASS.
