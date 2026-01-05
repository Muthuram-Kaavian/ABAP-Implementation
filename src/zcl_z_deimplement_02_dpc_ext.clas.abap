class ZCL_Z_DEIMPLEMENT_02_DPC_EXT definition
  public
  inheriting from ZCL_Z_DEIMPLEMENT_02_DPC
  create public .

public section.
protected section.

  methods Z_C_DEIMPLEMENTC_CREATE_ENTITY
    redefinition .
  methods Z_C_DEIMPLEMENTC_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_DEIMPLEMENT_02_DPC_EXT IMPLEMENTATION.


METHOD z_c_deimplementc_create_entity.
  DATA: ls_input  TYPE zcl_z_deimplement_01_mpc=>ts_z_c_deimplementchecktype,
        ls_output TYPE zcl_z_deimplement_01_mpc=>ts_z_c_deimplementchecktype.

  " Read POST payload
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

  " ---- Validate payload ----
  IF ls_input-notenumber IS INITIAL OR ls_input-transport IS INITIAL.
    ls_output-step        = 'VALIDATE_INPUT'.
    ls_output-status      = 'ERROR'.
    ls_output-message     = |NoteNumber and Transport are mandatory|.
    ls_output-notenumber  = ls_input-notenumber.
    ls_output-transport   = ls_input-transport.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  " ---- Use string concatenation trick ----
  DATA: lv_note_number  TYPE cwbntnumm,
        lv_transport    TYPE trkorr,
        gv_task         TYPE trkorr,
        gv_parent       TYPE trkorr,
        ls_e070         TYPE e070.

  " STRING CONCATENATION TRICK - forces pure CHAR
  DATA: lv_transport_char TYPE c LENGTH 20.
  CONCATENATE ls_input-transport '' INTO lv_transport_char.
  lv_transport = lv_transport_char.
  lv_note_number = ls_input-notenumber.

  " Database lookup with converted CHAR variable
  SELECT SINGLE * FROM e070 INTO ls_e070 WHERE trkorr = lv_transport.
  IF sy-subrc <> 0.
    ls_output-step        = 'VALIDATE_INPUT'.
    ls_output-status      = 'ERROR'.
    ls_output-message     = |Transport/Task { lv_transport } does not exist|.
    ls_output-notenumber  = lv_note_number.
    ls_output-transport   = ls_input-transport.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  " Resolve parent transport
  IF ls_e070-strkorr IS INITIAL.
    CLEAR gv_task.
    gv_parent = ls_e070-trkorr.
  ELSE.
    gv_task   = ls_e070-trkorr.
    gv_parent = ls_e070-strkorr.
  ENDIF.

  " ---- Unlock both task and parent ----
  DATA ls_req TYPE trwbo_request.
  IF gv_task IS NOT INITIAL.
    ls_req-h-trkorr = gv_task.
    CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
      CHANGING cs_request = ls_req.
  ENDIF.

  ls_req-h-trkorr = gv_parent.
  CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
    CHANGING cs_request = ls_req.

  COMMIT WORK AND WAIT.

  " ---- Call SNOTE API ----
  DATA: lv_rc      TYPE i,
        lt_api_msg TYPE scwb_api_t_msg,
        ls_esmsg   TYPE scwb_api_msg,
        lt_notes   TYPE STANDARD TABLE OF scwb_api_notenumber,
        ls_note    TYPE scwb_api_notenumber.

  CLEAR: lv_rc, lt_api_msg, ls_esmsg, lt_notes.
  ls_note-numm = lv_note_number.
  APPEND ls_note TO lt_notes.

  " Call SCWB with the concatenated CHAR variable
  CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
    EXPORTING
      iv_reqid = lv_transport_char
    IMPORTING
      ev_rc    = lv_rc
      es_msg   = ls_esmsg
      et_msg   = lt_api_msg
    TABLES
      it_notes = lt_notes
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    ls_output-step        = 'NOTE_DEIMPLEMENT'.
    ls_output-status      = 'ERROR'.
    ls_output-message     = |Function call failed with SY-SUBRC: { sy-subrc }|.
    ls_output-notenumber  = lv_note_number.
    ls_output-transport   = ls_input-transport.
    er_entity = ls_output.
    RETURN.
  ENDIF.

  IF sy-subrc = 0 AND ls_esmsg IS NOT INITIAL.
    APPEND ls_esmsg TO lt_api_msg.
  ENDIF.

  " If SNOTE created new transport (RC=99), detect & retry
  IF lv_rc = 99.
    DATA: lt_e070 TYPE TABLE OF e070,
          lv_min_time TYPE t.

    lv_min_time = sy-uzeit - 120.
    IF lv_min_time < 0. lv_min_time = 000000. ENDIF.

    SELECT * FROM e070 INTO TABLE lt_e070
      WHERE as4user    = sy-uname
        AND trfunction = 'K'
        AND as4date    = sy-datum
        AND as4time    >= lv_min_time.

    IF sy-subrc = 0.
      SORT lt_e070 BY as4date DESCENDING as4time DESCENDING.
      READ TABLE lt_e070 INDEX 1 INTO ls_e070.
      IF sy-subrc = 0.
        gv_parent = ls_e070-trkorr.

        " Convert new transport using concatenation
        CONCATENATE gv_parent '' INTO lv_transport_char.

        CLEAR: lv_rc, lt_api_msg, ls_esmsg.
        CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
          EXPORTING
            iv_reqid = lv_transport_char
          IMPORTING
            ev_rc    = lv_rc
            es_msg   = ls_esmsg
            et_msg   = lt_api_msg
          TABLES
            it_notes = lt_notes
          EXCEPTIONS
            OTHERS   = 1.

        IF sy-subrc = 0 AND ls_esmsg IS NOT INITIAL.
          APPEND ls_esmsg TO lt_api_msg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  " ---- Summarize API messages → Status + Message ----
  DATA: lv_buf  TYPE string,
        ls_msg  TYPE scwb_api_msg,
        lv_text TYPE string.

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
      EXCEPTIONS OTHERS = 1.
    IF lv_buf IS INITIAL.
      lv_buf = lv_text.
    ELSE.
      CONCATENATE lv_buf ';' lv_text INTO lv_buf SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  " ---- Determine status using traditional IF statements ----
  DATA: lv_status TYPE string.

  IF lines( lt_api_msg ) > 0.
    READ TABLE lt_api_msg INDEX 1 INTO ls_msg.
    IF sy-subrc = 0.
      CASE ls_msg-msgty.
        WHEN 'E'.
          lv_status = 'ERROR'.
        WHEN 'W'.
          lv_status = 'WARNING'.
        WHEN 'S'.
          lv_status = 'SUCCESS'.
        WHEN OTHERS.
          lv_status = 'INFO'.
      ENDCASE.
    ENDIF.
  ELSE.
    IF lv_rc = 0.
      lv_status = 'SUCCESS'.
    ELSE.
      lv_status = 'ERROR'.
    ENDIF.
  ENDIF.

  IF lv_buf IS INITIAL.
    IF lv_rc = 0.
      lv_buf = 'Note deimplemented successfully'.
    ELSE.
      lv_buf = |API RC { lv_rc }|.
    ENDIF.
  ENDIF.

  " ---- Fill response entity ----
  ls_output-step        = 'NOTE_DEIMPLEMENT'.
  ls_output-status      = lv_status.
  ls_output-message     = lv_buf.
  ls_output-transport   = ls_input-transport.
  ls_output-notenumber  = lv_note_number.

  " Return entity directly
  er_entity = ls_output.
ENDMETHOD.


METHOD z_c_deimplementc_get_entityset.
  " Empty method - just return nothing
  " This will give 200 status for CSRF token
ENDMETHOD.
ENDCLASS.
