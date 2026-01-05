class ZCL_Z_SNOTE_DEIMPLEMEN_DPC_EXT definition
  public
  inheriting from ZCL_Z_SNOTE_DEIMPLEMEN_DPC
  create public .

public section.
protected section.

  methods NOTEDEIMPLEMENTA_CREATE_ENTITY
    redefinition .
  methods NOTEDEIMPLEMENTA_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_SNOTE_DEIMPLEMEN_DPC_EXT IMPLEMENTATION.


METHOD notedeimplementa_create_entity.

  "------------------------------------------------------------
  " Types & data
  "------------------------------------------------------------
  DATA: ls_entity_data TYPE zcl_z_snote_deimplemen_mpc=>ts_notedeimplementation,
        lv_note_number TYPE cwbntnumm,
        lv_status      TYPE string,
        lv_message     TYPE string,
        lv_reqid       TYPE trkorr.          " CTS request number (optional)

  " SCWB API data
  DATA: lt_notes TYPE STANDARD TABLE OF scwb_api_notenumber,
        ls_note  TYPE scwb_api_notenumber,
        lv_rc    TYPE i.

  " Return message table from API
  DATA: lt_api_msg TYPE scwb_api_t_msg,
        ls_api_msg TYPE scwb_api_msg,
        lv_msgbuf  TYPE string.

  FIELD-SYMBOLS: <fs_req> TYPE any.

  "------------------------------------------------------------
  " 1) Read input payload from OData
  "------------------------------------------------------------
  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_entity_data
  ).

  lv_note_number = ls_entity_data-notenumber.

  " If your entity has an optional field TRANSPORTREQUEST, pick it up dynamically
  ASSIGN COMPONENT 'TRANSPORTREQUEST' OF STRUCTURE ls_entity_data TO <fs_req>.
  IF sy-subrc = 0 AND <fs_req> IS ASSIGNED.
    lv_reqid = <fs_req>.
  ENDIF.

  " Basic guard
  IF lv_note_number IS INITIAL.
    er_entity-status        = 'ERROR'.
    er_entity-message       = 'Missing SNOTE number'.
    RETURN.
  ENDIF.

  "------------------------------------------------------------
  " 2) Prepare SCWB API call
  "------------------------------------------------------------
  CLEAR ls_note.
  ls_note-numm = lv_note_number.
  APPEND ls_note TO lt_notes.

  "------------------------------------------------------------
  " 3) Call headless API to deimplement the note
  "------------------------------------------------------------
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

  "------------------------------------------------------------
  " 4) Process API messages - CORRECTED SECTION
  "    Use MSGTY and MSGID/MSGNO instead of TEXT
  "------------------------------------------------------------
  CLEAR lv_msgbuf.
  LOOP AT lt_api_msg INTO ls_api_msg.
    " Build message from available components
    IF lv_msgbuf IS NOT INITIAL.
      lv_msgbuf = |{ lv_msgbuf }; |.
    ENDIF.

    " Include message type for context
    CASE ls_api_msg-msgty.
      WHEN 'E'. lv_msgbuf = |{ lv_msgbuf }Error: |.
      WHEN 'W'. lv_msgbuf = |{ lv_msgbuf }Warning: |.
      WHEN 'I'. lv_msgbuf = |{ lv_msgbuf }Info: |.
      WHEN 'S'. lv_msgbuf = |{ lv_msgbuf }Success: |.
    ENDCASE.

    " Use message ID and number - you might want to get the actual text
    " from message class using MSGID and MSGNO
    lv_msgbuf = |{ lv_msgbuf }{ ls_api_msg-msgid }-{ ls_api_msg-msgno }|.

    " Optional: If you want to include the message variable
    IF ls_api_msg-msgv1 IS NOT INITIAL.
      lv_msgbuf = |{ lv_msgbuf } - { ls_api_msg-msgv1 }|.
    ENDIF.
  ENDLOOP.

  " Alternative: If you want to get the actual message text,
  " you could use the following function module:
  IF lt_api_msg IS NOT INITIAL.
    CLEAR lv_msgbuf.
    LOOP AT lt_api_msg INTO ls_api_msg.
      DATA(lv_msg_text) = |{ ls_api_msg-msgid }-{ ls_api_msg-msgno }|.
      " You could call FUNCTION 'MESSAGE_TEXT_BUILD' here to get actual text
      IF lv_msgbuf IS NOT INITIAL.
        lv_msgbuf = |{ lv_msgbuf }; |.
      ENDIF.
      lv_msgbuf = |{ lv_msgbuf }{ lv_msg_text }|.
    ENDLOOP.
  ENDIF.

  "------------------------------------------------------------
  " 5) Map result to OData response
  "------------------------------------------------------------
  IF sy-subrc = 0 AND lv_rc = 0.
    lv_status  = 'SUCCESS'.
    lv_message = |Note { lv_note_number } deimplemented successfully|.
    IF lv_msgbuf IS NOT INITIAL.
      lv_message = |{ lv_message } ({ lv_msgbuf })|.
    ENDIF.
  ELSE.
    lv_status = 'ERROR'.
    IF lv_msgbuf IS INITIAL.
      lv_message = |Deimplementation failed (rc={ lv_rc }, sy-subrc={ sy-subrc })|.
    ELSE.
      lv_message = |Deimplementation failed: { lv_msgbuf } (rc={ lv_rc })|.
    ENDIF.
  ENDIF.

  "------------------------------------------------------------
  " 6) Set output entity
  "------------------------------------------------------------
  er_entity-notenumber    = lv_note_number.
  er_entity-status        = lv_status.
  er_entity-message       = lv_message.

ENDMETHOD.


METHOD notedeimplementa_get_entityset.
  DATA: lt_entityset TYPE TABLE OF zcl_z_snote_deimplemen_mpc=>ts_notedeimplementation.
  et_entityset = lt_entityset.
ENDMETHOD.
ENDCLASS.
