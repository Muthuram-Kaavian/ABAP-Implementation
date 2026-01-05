class ZCL_Z_SNOTE_DEIMPLE_01_DPC_EXT definition
  public
  inheriting from ZCL_Z_SNOTE_DEIMPLE_01_DPC
  create public .

public section.
protected section.

  methods Z_C_DEIMPLEMEN01_GET_ENTITY
    redefinition .
  methods Z_C_DEIMPLEMENTC_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_SNOTE_DEIMPLE_01_DPC_EXT IMPLEMENTATION.


METHOD z_c_deimplemen01_create_entity.
    DATA: ls_input        TYPE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementchecktype,
          ls_e070         TYPE e070,
          lv_task         TYPE trkorr,
          lv_parent       TYPE trkorr,
          lv_transport_scwb TYPE scwb_request_id,
          lv_rc           TYPE i,
          lt_api_msg      TYPE scwb_api_t_msg,
          ls_api_msg      TYPE scwb_api_msg,
          lt_notes        TYPE STANDARD TABLE OF scwb_api_notenumber,
          ls_note         TYPE scwb_api_notenumber,
          ls_esmsg        TYPE scwb_api_msg,
          ls_req          TYPE trwbo_request,
          lv_buf          TYPE string,
          lv_text         TYPE string,
          lt_e070         TYPE TABLE OF e070,
          lv_min_time     TYPE t,
          lv_status       TYPE string,
          lv_message      TYPE string.

    " Get input data from the request
    io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

    " Step 1: Validate and resolve input
    SELECT SINGLE * FROM e070 INTO ls_e070 WHERE trkorr = ls_input-transport.
    IF sy-subrc <> 0.
      er_entity = VALUE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementresulttype(
        step        = 'VALIDATE_INPUT'
        status      = 'ERROR'
        message     = |Transport/Task { ls_input-transport } does not exist|
        transport   = ls_input-transport
        notenumber  = ls_input-notenumber
      ).
      RETURN.
    ENDIF.

    " Resolve task/parent
    IF ls_e070-strkorr IS INITIAL.
      CLEAR lv_task.
      lv_parent = ls_e070-trkorr.
    ELSE.
      lv_task   = ls_e070-trkorr.
      lv_parent = ls_e070-strkorr.
    ENDIF.

    " Step 2: Unlock both task and parent
    " Unlock task if it exists
    IF lv_task IS NOT INITIAL.
      ls_req-h-trkorr = lv_task.
      CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
        CHANGING
          cs_request = ls_req.
    ENDIF.

    " Unlock parent
    ls_req-h-trkorr = lv_parent.
    CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
      CHANGING
        cs_request = ls_req.

    COMMIT WORK AND WAIT.

    " Step 3: Deimplement note
    lv_transport_scwb = lv_parent.

    " Prepare note number for FM call
    ls_note-numm = ls_input-notenumber.
    APPEND ls_note TO lt_notes.

    " Call deimplementation FM
    CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
      EXPORTING
        iv_reqid = lv_transport_scwb
      IMPORTING
        ev_rc    = lv_rc
        es_msg   = ls_esmsg
        et_msg   = lt_api_msg
      TABLES
        it_notes = lt_notes
      EXCEPTIONS
        OTHERS   = 1.

    IF ls_esmsg IS NOT INITIAL.
      APPEND ls_esmsg TO lt_api_msg.
    ENDIF.

    " If SNOTE creates new transport (RC=99), detect and use it
    IF lv_rc = 99.
      " Detect new SNOTE transport
      lv_min_time = sy-uzeit - 120.
      IF lv_min_time < 0.
        lv_min_time = 000000.
      ENDIF.

      SELECT * FROM e070
        INTO TABLE lt_e070
        WHERE as4user    = sy-uname
          AND trfunction = 'K'
          AND as4date    = sy-datum
          AND as4time    >= lv_min_time.

      IF sy-subrc = 0.
        SORT lt_e070 BY as4date DESCENDING as4time DESCENDING.
        READ TABLE lt_e070 INDEX 1 INTO ls_e070.
        IF sy-subrc = 0.
          lv_parent = ls_e070-trkorr.
          lv_transport_scwb = lv_parent.

          " Call deimplementation FM again with new transport
          CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
            EXPORTING
              iv_reqid = lv_transport_scwb
            IMPORTING
              ev_rc    = lv_rc
              es_msg   = ls_esmsg
              et_msg   = lt_api_msg
            TABLES
              it_notes = lt_notes
            EXCEPTIONS
              OTHERS   = 1.
        ENDIF.
      ENDIF.
    ENDIF.

    " Process messages and set status
    CLEAR: lv_buf.
    LOOP AT lt_api_msg INTO ls_api_msg.
      CLEAR lv_text.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = ls_api_msg-msgid
          msgnr               = ls_api_msg-msgno
          msgv1               = ls_api_msg-msgv1
          msgv2               = ls_api_msg-msgv2
          msgv3               = ls_api_msg-msgv3
          msgv4               = ls_api_msg-msgv4
        IMPORTING
          message_text_output = lv_text
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc = 0.
        IF lv_buf IS INITIAL.
          lv_buf = lv_text.
        ELSE.
          CONCATENATE lv_buf ';' lv_text INTO lv_buf.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Determine status and message
    CLEAR: lv_status, lv_message.

    IF lines( lt_api_msg ) > 0.
      READ TABLE lt_api_msg INTO ls_api_msg INDEX 1.
      CASE ls_api_msg-msgty.
        WHEN 'E'.
          lv_status = 'ERROR'.
        WHEN 'W'.
          lv_status = 'WARNING'.
        WHEN 'S'.
          lv_status = 'SUCCESS'.
        WHEN OTHERS.
          lv_status = 'INFO'.
      ENDCASE.
      lv_message = lv_buf.
    ELSE.
      IF lv_rc = 0.
        lv_status = 'SUCCESS'.
        lv_message = 'Note deimplemented successfully'.
      ELSE.
        lv_status = 'ERROR'.
        lv_message = |API RC { lv_rc }|.
      ENDIF.
    ENDIF.

    " Return the result
    er_entity = VALUE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementresulttype(
      step        = 'NOTE_DEIMPLEMENT'
      status      = lv_status
      message     = lv_message
      transport   = lv_parent
      notenumber  = ls_input-notenumber
    ).
ENDMETHOD.


METHOD z_c_deimplemen01_get_entity.
    " Minimal GET_ENTITY - returns empty success response
    CLEAR er_entity.
ENDMETHOD.


METHOD z_c_deimplementc_create_entity.
    DATA: ls_input        TYPE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementchecktype,
          ls_e070         TYPE e070,
          lv_task         TYPE trkorr,
          lv_parent       TYPE trkorr,
          lv_transport_scwb TYPE scwb_request_id,
          lv_rc           TYPE i,
          lt_api_msg      TYPE scwb_api_t_msg,
          ls_api_msg      TYPE scwb_api_msg,
          lt_notes        TYPE STANDARD TABLE OF scwb_api_notenumber,
          ls_note         TYPE scwb_api_notenumber,
          ls_esmsg        TYPE scwb_api_msg,
          ls_req          TYPE trwbo_request,
          lv_buf          TYPE string,
          lv_text         TYPE string,
          lt_e070         TYPE TABLE OF e070,
          lv_min_time     TYPE t,
          lv_status       TYPE string,
          lv_message      TYPE string.

    " Get input data from the request
    io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

    " Step 1: Validate and resolve input
    SELECT SINGLE * FROM e070 INTO ls_e070 WHERE trkorr = ls_input-transport.
    IF sy-subrc <> 0.
      er_entity = VALUE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementresulttype(
        step        = 'VALIDATE_INPUT'
        status      = 'ERROR'
        message     = |Transport/Task { ls_input-transport } does not exist|
        transport   = ls_input-transport
        notenumber  = ls_input-notenumber
      ).
      RETURN.
    ENDIF.

    " Resolve task/parent
    IF ls_e070-strkorr IS INITIAL.
      CLEAR lv_task.
      lv_parent = ls_e070-trkorr.
    ELSE.
      lv_task   = ls_e070-trkorr.
      lv_parent = ls_e070-strkorr.
    ENDIF.

    " Step 2: Unlock both task and parent
    " Unlock task if it exists
    IF lv_task IS NOT INITIAL.
      ls_req-h-trkorr = lv_task.
      CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
        CHANGING
          cs_request = ls_req.
    ENDIF.

    " Unlock parent
    ls_req-h-trkorr = lv_parent.
    CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
      CHANGING
        cs_request = ls_req.

    COMMIT WORK AND WAIT.

    " Step 3: Deimplement note
    lv_transport_scwb = lv_parent.

    " Prepare note number for FM call
    ls_note-numm = ls_input-notenumber.
    APPEND ls_note TO lt_notes.

    " Call deimplementation FM
    CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
      EXPORTING
        iv_reqid = lv_transport_scwb
      IMPORTING
        ev_rc    = lv_rc
        es_msg   = ls_esmsg
        et_msg   = lt_api_msg
      TABLES
        it_notes = lt_notes
      EXCEPTIONS
        OTHERS   = 1.

    IF ls_esmsg IS NOT INITIAL.
      APPEND ls_esmsg TO lt_api_msg.
    ENDIF.

    " If SNOTE creates new transport (RC=99), detect and use it
    IF lv_rc = 99.
      " Detect new SNOTE transport
      lv_min_time = sy-uzeit - 120.
      IF lv_min_time < 0.
        lv_min_time = 000000.
      ENDIF.

      SELECT * FROM e070
        INTO TABLE lt_e070
        WHERE as4user    = sy-uname
          AND trfunction = 'K'
          AND as4date    = sy-datum
          AND as4time    >= lv_min_time.

      IF sy-subrc = 0.
        SORT lt_e070 BY as4date DESCENDING as4time DESCENDING.
        READ TABLE lt_e070 INDEX 1 INTO ls_e070.
        IF sy-subrc = 0.
          lv_parent = ls_e070-trkorr.
          lv_transport_scwb = lv_parent.

          " Call deimplementation FM again with new transport
          CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
            EXPORTING
              iv_reqid = lv_transport_scwb
            IMPORTING
              ev_rc    = lv_rc
              es_msg   = ls_esmsg
              et_msg   = lt_api_msg
            TABLES
              it_notes = lt_notes
            EXCEPTIONS
              OTHERS   = 1.
        ENDIF.
      ENDIF.
    ENDIF.

    " Process messages and set status
    CLEAR: lv_buf.
    LOOP AT lt_api_msg INTO ls_api_msg.
      CLEAR lv_text.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = ls_api_msg-msgid
          msgnr               = ls_api_msg-msgno
          msgv1               = ls_api_msg-msgv1
          msgv2               = ls_api_msg-msgv2
          msgv3               = ls_api_msg-msgv3
          msgv4               = ls_api_msg-msgv4
        IMPORTING
          message_text_output = lv_text
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc = 0.
        IF lv_buf IS INITIAL.
          lv_buf = lv_text.
        ELSE.
          CONCATENATE lv_buf ';' lv_text INTO lv_buf.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Determine status and message
    CLEAR: lv_status, lv_message.

    IF lines( lt_api_msg ) > 0.
      READ TABLE lt_api_msg INTO ls_api_msg INDEX 1.
      CASE ls_api_msg-msgty.
        WHEN 'E'.
          lv_status = 'ERROR'.
        WHEN 'W'.
          lv_status = 'WARNING'.
        WHEN 'S'.
          lv_status = 'SUCCESS'.
        WHEN OTHERS.
          lv_status = 'INFO'.
      ENDCASE.
      lv_message = lv_buf.
    ELSE.
      IF lv_rc = 0.
        lv_status = 'SUCCESS'.
        lv_message = 'Note deimplemented successfully'.
      ELSE.
        lv_status = 'ERROR'.
        lv_message = |API RC { lv_rc }|.
      ENDIF.
    ENDIF.

    " Return the result
    er_entity = VALUE zcl_z_snote_deimple_01_mpc=>ts_z_c_deimplementresulttype(
      step        = 'NOTE_DEIMPLEMENT'
      status      = lv_status
      message     = lv_message
      transport   = lv_parent
      notenumber  = ls_input-notenumber
    ).
ENDMETHOD.
ENDCLASS.
