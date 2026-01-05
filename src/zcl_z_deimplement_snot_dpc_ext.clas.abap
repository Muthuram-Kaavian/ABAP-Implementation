class ZCL_Z_DEIMPLEMENT_SNOT_DPC_EXT definition
  public
  inheriting from ZCL_Z_DEIMPLEMENT_SNOT_DPC
  create public .

public section.
protected section.

  methods Z_DEIMPLEMENTTYP_CREATE_ENTITY
    redefinition .
  methods Z_DEIMPLEMENTTYP_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_DEIMPLEMENT_SNOT_DPC_EXT IMPLEMENTATION.


METHOD z_deimplementtyp_create_entity.

  "=== 1) Use SEGW-generated types ===
  DATA: ls_input  TYPE zcl_z_deimplement_snot_mpc=>ts_z_deimplementtype,
        ls_output TYPE zcl_z_deimplement_snot_mpc=>ts_z_deimplementtype.

  "=== 2) Read POST payload ===
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

  "=== 3) Validate input ===
  IF ls_input-notenumber IS INITIAL OR ls_input-transport IS INITIAL.
    ls_output-step    = 'VALIDATE_INPUT'.
    ls_output-status  = 'ERROR'.
    ls_output-message = 'NoteNumber and Transport are mandatory'.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  "=== 4) Normalize inputs ===
  DATA: lv_note_number  TYPE cwbntnumm,
        lv_transport_in TYPE string.

  lv_note_number  = ls_input-notenumber.
  lv_transport_in = ls_input-transport.

  TRANSLATE lv_transport_in TO UPPER CASE.
  CONDENSE  lv_transport_in NO-GAPS.
  CONDENSE lv_note_number NO-GAPS.

  DATA lv_reqid TYPE scwb_request_id.
  lv_reqid = lv_transport_in.

  "=== 5) Check implementation status using SCWB_NOTE_IMPL_STATUS ===
  DATA: ls_note_details TYPE bcwbn_note,
        lv_impl_status  TYPE c.

  FIELD-SYMBOLS: <fs_key> TYPE any,
                 <fs_numm> TYPE any.

  " Populate the note number in KEY-NUMM structure
  ASSIGN COMPONENT 'KEY' OF STRUCTURE ls_note_details TO <fs_key>.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'NUMM' OF STRUCTURE <fs_key> TO <fs_numm>.
    IF sy-subrc = 0.
      <fs_numm> = lv_note_number.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
    EXPORTING
      is_note                    = ls_note_details
    IMPORTING
      ev_status                  = lv_impl_status
    EXCEPTIONS
      note_not_found             = 1
      inconsistent_delivery_data = 2
      undefined_component_state  = 3
      incomplete_note_data       = 4
      OTHERS                     = 5.

  "=== Handle implementation status cases ===
  CASE sy-subrc.
    WHEN 0. " Function call successful
      CASE lv_impl_status.
        WHEN 'I' OR 'E'.
          " ✅ Note is implemented - continue to deimplementation

        WHEN 'N'.
          " ❌ Note is NOT implemented - return error immediately
          ls_output-step        = 'CHECK_IMPLEMENTATION_STATUS'.
          ls_output-status      = 'ERROR'.
          ls_output-message     = |Note { lv_note_number } is not implemented - nothing to deimplement.|.
          ls_output-notenumber  = lv_note_number.
          ls_output-transport   = lv_reqid.
          er_entity = ls_output.
          RETURN.

        WHEN OTHERS.
          " ⚠️ Unknown status - return error
          ls_output-step        = 'CHECK_IMPLEMENTATION_STATUS'.
          ls_output-status      = 'ERROR'.
          ls_output-message     = |Note { lv_note_number } has unknown implementation status: { lv_impl_status }.|.
          ls_output-notenumber  = lv_note_number.
          ls_output-transport   = lv_reqid.
          er_entity = ls_output.
          RETURN.
      ENDCASE.

    WHEN 1. " ❌ Note not found
      ls_output-step        = 'CHECK_IMPLEMENTATION_STATUS'.
      ls_output-status      = 'ERROR'.
      ls_output-message     = |Note { lv_note_number } does not exist in the system.|.
      ls_output-notenumber  = lv_note_number.
      ls_output-transport   = lv_reqid.
      er_entity = ls_output.
      RETURN.

    WHEN OTHERS. " ⚠️ Other function module errors
      " Continue to API call and let it handle the validation
  ENDCASE.

  "=== 6) Basic transport validation ===
  SELECT SINGLE * FROM e070 INTO @DATA(ls_e070) WHERE trkorr = @lv_reqid.
  IF sy-subrc <> 0.
    ls_output-step    = 'VALIDATE_INPUT'.
    ls_output-status  = 'ERROR'.
    ls_output-message = |Transport { lv_reqid } not found in E070|.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  " Reject released transports only (allow locked ones)
  IF ls_e070-trstatus = 'R'.
    ls_output-step    = 'VALIDATE_INPUT'.
    ls_output-status  = 'ERROR'.
    ls_output-message = |Transport "{ ls_e070-trkorr }" is released, cannot be used.|.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  "=== 7) Determine parent/task and unlock ===
  DATA: gv_task TYPE trkorr,
        gv_parent TYPE trkorr,
        ls_req TYPE trwbo_request.

  IF ls_e070-strkorr IS INITIAL.
    CLEAR gv_task.
    gv_parent = ls_e070-trkorr.
  ELSE.
    gv_task   = ls_e070-trkorr.
    gv_parent = ls_e070-strkorr.
  ENDIF.


" 🔓 Unlock the provided transport (whether task or parent)
IF gv_task IS NOT INITIAL.
  " If it's a task, unlock only the task
  ls_req-h-trkorr = gv_task.
ELSE.
  " If it's a parent, unlock the parent
  ls_req-h-trkorr = gv_parent.
ENDIF.

CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
  CHANGING
    cs_request = ls_req.
COMMIT WORK AND WAIT.


  "=== 8) Call SNOTE API ===
  DATA: lt_notes   TYPE STANDARD TABLE OF scwb_api_notenumber,
        ls_note    TYPE scwb_api_notenumber,
        lt_api_msg TYPE scwb_api_t_msg,
        ls_msg     TYPE scwb_api_msg,
        lv_rc      TYPE i,
        lv_text    TYPE string,
        lv_buf     TYPE string,
        lv_status  TYPE string.

  ls_note-numm = lv_note_number.
  APPEND ls_note TO lt_notes.

  CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
    EXPORTING
      iv_reqid = lv_reqid
    IMPORTING
      ev_rc    = lv_rc
      et_msg   = lt_api_msg
    TABLES
      it_notes = lt_notes
    EXCEPTIONS
      OTHERS   = 1.

  "=== 9) Handle RC=99 (SNOTE created new transport) ===
  IF lv_rc = 99.
    DATA lt_e070 TYPE TABLE OF e070.
    SELECT * FROM e070 INTO TABLE lt_e070
      WHERE as4user = sy-uname
        AND trfunction = 'K'
        AND as4date = sy-datum.
    IF sy-subrc = 0.
      SORT lt_e070 BY as4date DESCENDING as4time DESCENDING.
      READ TABLE lt_e070 INDEX 1 INTO ls_e070.
      IF sy-subrc = 0.
        lv_reqid = ls_e070-trkorr.
        CLEAR: lv_rc, lt_api_msg.
        CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
          EXPORTING
            iv_reqid = lv_reqid
          IMPORTING
            ev_rc    = lv_rc
            et_msg   = lt_api_msg
          TABLES
            it_notes = lt_notes
          EXCEPTIONS
            OTHERS   = 1.
      ENDIF.
    ENDIF.
  ENDIF.

  "=== 10) Collect API messages ===
  LOOP AT lt_api_msg INTO ls_msg.
    CLEAR lv_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid = ls_msg-msgid
        msgnr = ls_msg-msgno
        msgv1 = ls_msg-msgv1
        msgv2 = ls_msg-msgv2
        msgv3 = ls_msg-msgv3
        msgv4 = ls_msg-msgv4
      IMPORTING
        message_text_output = lv_text
      EXCEPTIONS
        OTHERS = 1.

    IF lv_buf IS INITIAL.
      lv_buf = lv_text.
    ELSE.
      CONCATENATE lv_buf ';' lv_text INTO lv_buf SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  " Use actual API messages if available, otherwise show generic message
  IF lv_buf IS INITIAL.
    lv_buf = |Note { lv_note_number } deimplementation result code: { lv_rc }|.
  ENDIF.

  "=== 11) Determine status ===
  lv_status = COND string(
    WHEN lv_rc = 0  THEN 'SUCCESS'
    WHEN lv_rc = 99 THEN 'WARNING'
    ELSE                 'ERROR' ).

  "=== 12) Fill OData response ===
  ls_output-step        = 'NOTE_DEIMPLEMENT'.
  ls_output-status      = lv_status.
  ls_output-message     = lv_buf.
  ls_output-notenumber  = lv_note_number.
  ls_output-transport   = lv_reqid.

  er_entity = ls_output.

ENDMETHOD.


  method Z_DEIMPLEMENTTYP_GET_ENTITYSET.

  endmethod.
ENDCLASS.
