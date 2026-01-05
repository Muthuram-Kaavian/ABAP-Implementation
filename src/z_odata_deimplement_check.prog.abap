
REPORT z_odata_deimplement_check.
*==================== Types & Data ====================*
TYPES: BEGIN OF ty_result,
         step         TYPE string,
         status       TYPE string,
         message      TYPE string,
         transport    TYPE trkorr,
         note_number  TYPE cwbntnumm,
         rc           TYPE i,
       END OF ty_result.
DATA: lt_results        TYPE STANDARD TABLE OF ty_result,
      ls_result         TYPE ty_result.
" Inputs / derived
DATA: lv_note_number    TYPE cwbntnumm,
      gv_input_trkorr   TYPE trkorr,
      gv_task           TYPE trkorr,
      gv_parent         TYPE trkorr,
      lv_transport_scwb TYPE scwb_request_id.
" API scratch
DATA: lv_rc       TYPE i,
      lt_api_msg  TYPE scwb_api_t_msg,
      ls_api_msg  TYPE scwb_api_msg.
*==================== Selection Screen ====================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_note  TYPE cwbntnumm OBLIGATORY,
            p_trans TYPE trkorr    OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_debug AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.
*==================== Main ====================*
START-OF-SELECTION.
  PERFORM show_program_header.
  PERFORM validate_and_resolve_input.
  PERFORM unlock_both_task_and_parent.     " Only cleanup unlocks
  PERFORM deimplement_note.                " Let SNOTE handle transport
  PERFORM display_results.
*==================== UI ======================*
FORM show_program_header.
  WRITE: / 'deimplement sap note'.
  WRITE: / 'Minimal version - unlock only, no release steps'.
  SKIP 1.
ENDFORM.
*==================== Input resolve ===========*
FORM validate_and_resolve_input.
  DATA ls_e070 TYPE e070.
  lv_note_number  = p_note.
  gv_input_trkorr = p_trans.
  SELECT SINGLE * FROM e070 INTO ls_e070 WHERE trkorr = gv_input_trkorr.
  IF sy-subrc <> 0.
    ls_result-step      = 'VALIDATE_INPUT'.
    ls_result-status    = 'ERROR'.
    ls_result-message   = |Transport/Task { gv_input_trkorr } does not exist|.
    ls_result-transport = gv_input_trkorr.
    APPEND ls_result TO lt_results.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF ls_e070-strkorr IS INITIAL.
    CLEAR gv_task.
    gv_parent = ls_e070-trkorr.
  ELSE.
    gv_task   = ls_e070-trkorr.
    gv_parent = ls_e070-strkorr.
  ENDIF.
  lv_transport_scwb = gv_parent.
  WRITE: / 'SNOTE Deimplementation – Minimal Approach'.
  ULINE.
  SKIP.
  WRITE: / 'Input Parameters:'.
  WRITE: / '  Note     :', lv_note_number.
  WRITE: / '  Entered  :', gv_input_trkorr.
  IF gv_task IS NOT INITIAL.
    WRITE: / '  Task     :', gv_task.
  ENDIF.
  WRITE: / '  Parent   :', gv_parent.
  WRITE: / '  Strategy : Unlock only, no release steps'.
  SKIP.
ENDFORM.
*==================== Unlock both task and parent ===========*
*==================== Unlock (task only) ===========*
FORM unlock_both_task_and_parent.
  DATA ls_req TYPE trwbo_request.

  " ✅ Unlock TASK only (if provided)
  IF gv_task IS NOT INITIAL.
    CLEAR ls_result.
    ls_result-step      = 'UNLOCK_TASK'.
    ls_result-transport = gv_task.

    CLEAR ls_req.
    ls_req-h-trkorr = gv_task.
    CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
      CHANGING
        cs_request = ls_req.

    IF sy-subrc = 0.
      ls_result-status  = 'SUCCESS'.
      ls_result-message = 'Task unlocked'.
    ELSE.
      ls_result-status  = 'WARNING'.
      ls_result-message = |Task unlock SY-SUBRC { sy-subrc }|.
    ENDIF.
    APPEND ls_result TO lt_results.
    WRITE: / '✓ Task unlocked:', gv_task.
  ELSE.
    " No task provided (parent-only input). Do nothing here.
    CLEAR ls_result.
    ls_result-step      = 'UNLOCK_TASK'.
    ls_result-status    = 'INFO'.
    ls_result-message   = 'No task to unlock (input was a parent). Skipped.'.
    ls_result-transport = gv_parent.
    APPEND ls_result TO lt_results.
  ENDIF.

  " ❌ Do NOT unlock the parent; keep its objects/visibility in SE09.
  " COMMIT only if you actually changed something
  IF gv_task IS NOT INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.

*==================== Minimal Deimplement ==============*
FORM deimplement_note.
  CLEAR: ls_result, lt_api_msg, lv_rc.
  ls_result-step        = 'NOTE_DEIMPLEMENT'.
  ls_result-note_number = lv_note_number.
  WRITE: / 'Starting deimplementation - SNOTE will create its own transport...'.
  " Try with original transport (will trigger SNOTE to create new one)
  ls_result-transport = gv_parent.
  lv_transport_scwb = gv_parent.
  PERFORM call_deimpl_fm USING lv_note_number lv_transport_scwb CHANGING lv_rc lt_api_msg.
  " If SNOTE creates new transport (RC=99), detect and use it
  IF lv_rc = 99.
    WRITE: / 'SNOTE creating new transport, detecting...'.
    PERFORM detect_new_snote_parent CHANGING gv_parent.
    IF gv_parent IS NOT INITIAL.
      ls_result-transport = gv_parent.
      lv_transport_scwb = gv_parent.
      WRITE: / 'Using SNOTE-created transport:', gv_parent.
      PERFORM call_deimpl_fm USING lv_note_number lv_transport_scwb CHANGING lv_rc lt_api_msg.
    ENDIF.
  ENDIF.
  ls_result-rc = lv_rc.
  PERFORM process_deimpl_msgs USING lt_api_msg lv_rc
                              CHANGING ls_result-status ls_result-message.
  APPEND ls_result TO lt_results.
ENDFORM.
*==================== Detect newly created SNOTE parent =========*
FORM detect_new_snote_parent CHANGING cv_parent TYPE trkorr.
  DATA lt_e070 TYPE TABLE OF e070.
  DATA lv_min_time TYPE t.
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
    READ TABLE lt_e070 INDEX 1 INTO DATA(ls_e070).
    IF sy-subrc = 0.
      cv_parent = ls_e070-trkorr.
    ENDIF.
  ENDIF.
ENDFORM.
*==================== FM wrapper ===============*
FORM call_deimpl_fm
  USING    iv_note_number TYPE cwbntnumm
           iv_transport   TYPE scwb_request_id
  CHANGING cv_rc          TYPE i
           ct_api_msg     TYPE scwb_api_t_msg.
  DATA: lt_notes TYPE STANDARD TABLE OF scwb_api_notenumber,
        ls_note  TYPE scwb_api_notenumber,
        ls_esmsg TYPE scwb_api_msg.
  CLEAR ct_api_msg.
  ls_note-numm = iv_note_number.
  APPEND ls_note TO lt_notes.
  WRITE: / 'Calling SCWB_API_NOTES_DEIMPLEMENT ...'.
  WRITE: / '  Transport passed to API:', iv_transport.
  CALL FUNCTION 'SCWB_API_NOTES_DEIMPLEMENT'
    EXPORTING
      iv_reqid = iv_transport
    IMPORTING
      ev_rc    = cv_rc
      es_msg   = ls_esmsg
      et_msg   = ct_api_msg
    TABLES
      it_notes = lt_notes
    EXCEPTIONS
      OTHERS   = 1.
  IF p_debug = abap_true.
    WRITE: / '  SY-SUBRC:', sy-subrc, '  EV_RC:', cv_rc.
    IF ls_esmsg IS NOT INITIAL.
      APPEND ls_esmsg TO ct_api_msg.
    ENDIF.
    IF lines( ct_api_msg ) > 0.
      PERFORM display_det_msgs USING ct_api_msg.
    ENDIF.
  ENDIF.
  SKIP.
ENDFORM.
*==================== Msg helpers ==============*
FORM display_det_msgs USING it_api_msg TYPE scwb_api_t_msg.
  DATA ls_msg TYPE scwb_api_msg.
  WRITE: / 'Detailed messages:'.
  LOOP AT it_api_msg INTO ls_msg.
    PERFORM display_msg_text USING ls_msg.
  ENDLOOP.
ENDFORM.
FORM display_msg_text USING is_msg TYPE scwb_api_msg.
  DATA lv_text TYPE string.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid = is_msg-msgid
      msgnr = is_msg-msgno
      msgv1 = is_msg-msgv1
      msgv2 = is_msg-msgv2
      msgv3 = is_msg-msgv3
      msgv4 = is_msg-msgv4
    IMPORTING
      message_text_output = lv_text
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc = 0.
    WRITE: / is_msg-msgty, is_msg-msgid, is_msg-msgno, ':', lv_text.
  ELSE.
    WRITE: / is_msg-msgty, is_msg-msgid, is_msg-msgno, is_msg-msgv1, is_msg-msgv2.
  ENDIF.
ENDFORM.
FORM process_deimpl_msgs
  USING    it_api_msg TYPE scwb_api_t_msg
           iv_rc      TYPE i
  CHANGING cv_status  TYPE string
           cv_message TYPE string.
  DATA: lv_buf TYPE string, ls_msg TYPE scwb_api_msg.
  LOOP AT it_api_msg INTO ls_msg.
    DATA lv_text TYPE string.
    CLEAR lv_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING msgid = ls_msg-msgid msgnr = ls_msg-msgno
                msgv1 = ls_msg-msgv1 msgv2 = ls_msg-msgv2
                msgv3 = ls_msg-msgv3 msgv4 = ls_msg-msgv4
      IMPORTING message_text_output = lv_text
      EXCEPTIONS OTHERS = 1.
    IF lv_buf IS INITIAL.
      lv_buf = lv_text.
    ELSE.
      CONCATENATE lv_buf ';' lv_text INTO lv_buf SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  IF lines( it_api_msg ) > 0.
    READ TABLE it_api_msg INTO ls_msg INDEX 1.
    CASE ls_msg-msgty.
      WHEN 'E'.
        cv_status = 'ERROR'.
      WHEN 'W'.
        cv_status = 'WARNING'.
      WHEN 'S'.
        cv_status = 'SUCCESS'.
      WHEN OTHERS.
        cv_status = 'INFO'.
    ENDCASE.
    cv_message = lv_buf.
    RETURN.
  ENDIF.
  IF iv_rc = 0.
    cv_status = 'SUCCESS'.
    cv_message = 'Note deimplemented successfully'.
  ELSE.
    cv_status = 'ERROR'.
    cv_message = |API RC { iv_rc }|.
  ENDIF.
ENDFORM.
*==================== Results ==================*
FORM display_results.
  DATA lv_ok  TYPE i.
  DATA lv_err TYPE i.
  SKIP 1.
  WRITE: / 'FINAL RESULTS'.
  ULINE.
  LOOP AT lt_results INTO ls_result.
    IF ls_result-status = 'SUCCESS'.
      lv_ok  = lv_ok + 1.
    ELSEIF ls_result-status = 'ERROR'.
      lv_err = lv_err + 1.
    ENDIF.
  ENDLOOP.
  WRITE: / 'Success:', lv_ok, ' Errors:', lv_err.
  SKIP.
  LOOP AT lt_results INTO ls_result.
    ULINE.
    WRITE: / 'Step     :', ls_result-step.
    WRITE: / 'Transport:', ls_result-transport.
    IF ls_result-note_number IS NOT INITIAL.
      WRITE: / 'Note     :', ls_result-note_number.
    ENDIF.
    WRITE: / 'Status   :', ls_result-status.
    IF ls_result-rc IS NOT INITIAL.
      WRITE: / 'RC       :', ls_result-rc.
    ENDIF.
    WRITE: / 'Message  :', ls_result-message.
  ENDLOOP.
  SKIP.
  IF lv_err = 0.
    WRITE: / '✓ Completed OK'.
  ELSE.
    WRITE: / '✗ Completed with errors'.
  ENDIF.
ENDFORM.
