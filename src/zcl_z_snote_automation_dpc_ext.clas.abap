class ZCL_Z_SNOTE_AUTOMATION_DPC_EXT definition
  public
  inheriting from ZCL_Z_SNOTE_AUTOMATION_DPC
  create public .

public section.
protected section.

  methods SNOTEREQUESTS_CREATE_ENTITY
    redefinition .
  methods SNOTEREQUESTS_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_SNOTE_AUTOMATION_DPC_EXT IMPLEMENTATION.


DEFINE assign_msg_fields.
"=== Macro to assign MSG* fields from any log row ===
  ASSIGN COMPONENT 'MSGTY' OF STRUCTURE &1 TO <ty>.
  ASSIGN COMPONENT 'MSGID'  OF STRUCTURE &1 TO <id>.
  ASSIGN COMPONENT 'MSGNO'  OF STRUCTURE &1 TO <no>.
  ASSIGN COMPONENT 'MSGV1'  OF STRUCTURE &1 TO <v1>.
  ASSIGN COMPONENT 'MSGV2'  OF STRUCTURE &1 TO <v2>.
  ASSIGN COMPONENT 'MSGV3'  OF STRUCTURE &1 TO <v3>.
  ASSIGN COMPONENT 'MSGV4'  OF STRUCTURE &1 TO <v4>.
END-OF-DEFINITION.

METHOD snoterequests_create_entity.

  "-------------------- Declarations --------------------
  DATA ls_request       TYPE zcl_z_snote_automation_mpc=>ts_snoterequest. " Incoming OData payload
  DATA lv_note_number   TYPE cwbntnumm.                                   " Normalized Note number (string without spaces)
  DATA lv_trkorr        TYPE trkorr.                                      " Transport request number
  DATA lv_caller_id     TYPE cwbcaller_id.
  DATA lv_msg_text      TYPE string.
  DATA lv_failed        TYPE abap_bool.
  DATA lv_nl            TYPE string.

  " NEW: overall return code derived from logs
  DATA lv_return_code   TYPE i VALUE 0.                                   " 0 = OK, 4 = warning, 8 = error
  DATA lv_msg_severity  TYPE i.                                           " per-message severity during loops

  " SNOTE interface objects (match your FM)
  DATA ls_sched         TYPE scwn_dark_s_schedule_info.                   " Schedule/flags to run SNOTE without dialogs
  DATA lt_note_headers  TYPE cwbnt_t_head.                                " Table of notes for batch implementation
  DATA ls_note_header   TYPE cwbnthead.                                   " Single note header row

  DATA lt_note_logs     TYPE scwn_dark_t_note_log_message.                " Logs at note level
  DATA lt_object_logs   TYPE scwn_dark_t_log_message.                     " Logs at object level
  DATA lt_generic_logs  TYPE scwn_dark_t_gen_log_message.                 " Misc/general logs
  DATA lt_udo_logs      TYPE scwn_dark_t_udo_message.                     " UDO logs

  " E070 (transport)
  DATA ls_e070          TYPE e070.                                        " Transport header (for validations)

  FIELD-SYMBOLS: <fs_any>    TYPE any,
                 <fs_trkorr> TYPE any.

  " For dynamic schedule flags
  DATA lo_sched_desc TYPE REF TO cl_abap_structdescr.                     " Runtime structure descriptor
  DATA ls_hdr_comp   TYPE abap_compdescr.                                 " Each component of schedule struct

  " For log loops (explicit FS — no inline)
  FIELD-SYMBOLS: <row1> TYPE any.
  FIELD-SYMBOLS: <row2> TYPE any.
  FIELD-SYMBOLS: <row3> TYPE any.
  FIELD-SYMBOLS: <row4> TYPE any.
  FIELD-SYMBOLS: <ty>   TYPE any.                                         " Message type (S/I/W/E/A/X)
  FIELD-SYMBOLS: <id>   TYPE any.                                         " Message class
  FIELD-SYMBOLS: <no>   TYPE any.                                         " Message number
  FIELD-SYMBOLS: <v1>   TYPE any.                                         " Message variable 1
  FIELD-SYMBOLS: <v2>   TYPE any.
  FIELD-SYMBOLS: <v3>   TYPE any.
  FIELD-SYMBOLS: <v4>   TYPE any.

  " Additional flags for message shaping
  DATA lv_already_impl TYPE abap_bool VALUE abap_false.                   " Detected "already implemented" info message

  TRY.

      "----------- Read payload & validate -----------
      io_data_provider->read_entry_data( IMPORTING es_data = ls_request ). " Read OData POST body into ls_request

      " Require BOTH: Note & Transport (fix #1)
      IF ls_request-notenumber IS INITIAL OR ls_request-transport IS INITIAL.
        ls_request-status      = 'Error'.
        ls_request-message     = 'Note number and transport are required.'.
        ls_request-returncode  = 8.                                       " Validation error → treat as error
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      "----------- Normalize & audit ----------------
      lv_note_number = ls_request-notenumber.                             " Copy & normalize Note number
      CONDENSE lv_note_number NO-GAPS.

      " Always use transport from request; normalize
      lv_trkorr = ls_request-transport.                                   " Copy & normalize TR
      TRANSLATE lv_trkorr TO UPPER CASE.
      CONDENSE   lv_trkorr NO-GAPS.

      ls_request-createdby = sy-uname.                                    " Audit fields
      GET TIME STAMP FIELD ls_request-createdat.

      IF ls_request-taskid IS INITIAL.
        TRY.
            ls_request-taskid = cl_system_uuid=>create_uuid_x16_static( ). " Generate task id if not provided
          CATCH cx_uuid_error.
            CONCATENATE sy-uname sy-datum sy-uzeit INTO ls_request-taskid. " Fallback unique-ish id
        ENDTRY.
      ENDIF.

      " caller id (avoid CONV)
      CLEAR lv_caller_id.
      IF ls_request-taskid IS NOT INITIAL.
        lv_caller_id = ls_request-taskid.                                 " Prefer task id as caller id
      ELSE.
        lv_caller_id = sy-uname.                                          " Fallback to user
      ENDIF.

      "----------- Validate: already implemented? -----
      DATA ls_bcwbn_note  TYPE bcwbn_note.                                " Structure name differs by release
      DATA lv_implemented TYPE bcwbn_implemented.

      CLEAR ls_bcwbn_note.

      " Put the note number into the correct component (name differs by release)
      ASSIGN COMPONENT 'NOTE_NUMBER' OF STRUCTURE ls_bcwbn_note TO <fs_any>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT 'NUMM' OF STRUCTURE ls_bcwbn_note TO <fs_any>.
      ENDIF.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT 'NOTE' OF STRUCTURE ls_bcwbn_note TO <fs_any>.
      ENDIF.
      IF sy-subrc = 0 AND <fs_any> IS ASSIGNED.
        <fs_any> = lv_note_number.
      ENDIF.

      CALL FUNCTION 'SCWB_NOTE_CHECK_IMPLEMENTED'                          " Check if Note already implemented
        IMPORTING
          ev_implemented = lv_implemented
        CHANGING
          cs_note        = ls_bcwbn_note
        EXCEPTIONS
          not_found                   = 1
          inconsistent_note_data      = 2
          inconsistent_delivery_data  = 3
          undefined_component_state   = 4
          OTHERS                      = 5.

      IF sy-subrc = 0 AND lv_implemented = abap_true.
        ls_request-status      = 'Already Implemented'.
        ls_request-message     = 'This SNOTE number is already implemented.'.
        ls_request-transport   = lv_trkorr.                                " Echo given TR in response
        ls_request-returncode  = 0.                                       " Already implemented → OK
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      "----------- Validate TR via E070 -------------
      SELECT SINGLE *                                                      " Fetch transport header
        FROM e070
        INTO @ls_e070
        WHERE trkorr = @lv_trkorr.

      IF sy-subrc <> 0 OR ls_e070-trkorr IS INITIAL.
        ls_request-status      = 'Implementation Error'.
        CONCATENATE 'Transport "' lv_trkorr '" could not be found. Please verify the request number.'
               INTO ls_request-message.
        ls_request-transport   = lv_trkorr.
        ls_request-returncode  = 8.
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      " Enhanced validation: check if transport is locked or released
      IF ls_e070-trstatus = 'R' OR ls_e070-trstatus = 'L'.                " R/L = Released/Locked → cannot use
        ls_request-status      = 'Implementation Failed'.
        CONCATENATE 'Transport "' lv_trkorr '" is locked or released, cannot be used.'
               INTO ls_request-message.
        ls_request-transport   = lv_trkorr.
        ls_request-notenumber  = lv_note_number.
        ls_request-returncode  = 8.
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      " must be Workbench (K)
      IF ls_e070-trfunction <> 'K'.                                       " Only Workbench requests allowed
        ls_request-status      = 'Implementation Error'.
        CONCATENATE 'Transport "' lv_trkorr '" is not a Workbench request.'
               INTO ls_request-message.
        ls_request-transport   = lv_trkorr.
        ls_request-returncode  = 8.
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      "----------- Build IT_NOTE_HEADERS ------------
      CLEAR lt_note_headers.
      CLEAR ls_note_header.
      ls_note_header-numm = lv_note_number.                               " Add the note to implement
      APPEND ls_note_header TO lt_note_headers.

      "----------- Make schedule silent -------------
      CLEAR ls_sched.
      lo_sched_desc ?= cl_abap_typedescr=>describe_by_data( ls_sched ).   " Reflect over schedule structure

      LOOP AT lo_sched_desc->components INTO ls_hdr_comp.
        IF ls_hdr_comp-name CS 'DIALOG'
        OR ls_hdr_comp-name CS 'POPUP'
        OR ls_hdr_comp-name CS 'CONFIRM'
        OR ls_hdr_comp-name CS 'YES'
        OR ls_hdr_comp-name CS 'OK'
        OR ls_hdr_comp-name CS 'AUTO'
        OR ls_hdr_comp-name CS 'PREREQ'
        OR ls_hdr_comp-name CS 'PREREQUIS'
        OR ls_hdr_comp-name CS 'FORCE'
        OR ls_hdr_comp-name CS 'CONTINUE'
        OR ls_hdr_comp-name CS 'ACCEPT'
        OR ls_hdr_comp-name CS 'SILENT'
        OR ls_hdr_comp-name CS 'DOWNLOAD'
        OR ls_hdr_comp-name CS 'IMPLEMENT'
        OR ls_hdr_comp-name CS 'APPLY'
        OR ls_hdr_comp-name CS 'UPDATE'.
          ASSIGN COMPONENT ls_hdr_comp-name OF STRUCTURE ls_sched TO <fs_any>.
          IF sy-subrc = 0.
            " best-effort set to abap_true without CONV
            <fs_any> = abap_true.                                         " Force silent/auto behavior when possible
          ENDIF.
        ENDIF.
      ENDLOOP.

      "----------- Implement via DARK FM ------------
      CLEAR lt_note_logs.
      CLEAR lt_object_logs.
      CLEAR lt_generic_logs.
      CLEAR lt_udo_logs.
      CLEAR lv_msg_text.
      lv_failed       = abap_false.
      lv_nl           = cl_abap_char_utilities=>newline.
      lv_return_code  = 0.                                                " reset before reading logs

      CALL FUNCTION 'SCWN_DARK_API_IMPL_MULTI_NOTES'                      " Main SNOTE dark API call
        EXPORTING
          iv_def_trans     = lv_trkorr                                   " Use provided TR
          iv_caller_id     = lv_caller_id                                " For audit/trace
          is_schedule_info = ls_sched                                    " Silent flags
        IMPORTING
          et_note_logs     = lt_note_logs
          et_object_logs   = lt_object_logs
          et_generic_logs  = lt_generic_logs
          et_udo_logs      = lt_udo_logs
        TABLES
          it_note_headers  = lt_note_headers
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc <> 0.                                                   " Non-zero SUBRC → build readable message
        IF sy-msgid IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid = sy-msgid
              msgnr = sy-msgno
              msgv1 = sy-msgv1
              msgv2 = sy-msgv2
              msgv3 = sy-msgv3
              msgv4 = sy-msgv4
            IMPORTING
              message_text_output = lv_msg_text
            EXCEPTIONS
              OTHERS = 1.
        ENDIF.

        IF lv_msg_text IS INITIAL.
          lv_msg_text = 'SNOTE dark API returned non-zero subrc'.
        ENDIF.

        ls_request-status      = 'Implementation Error'.
        ls_request-message     = lv_msg_text.
        ls_request-transport   = lv_trkorr.
        ls_request-returncode  = 8.                                       " Dark API call itself failed
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      "----------- Interpret all exported logs ------
      " Helper: local routine to update lv_return_code based on <ty>
      " (in ABAP OO you'd normally move this to a private method; here inline):

      " NOTE logs
      LOOP AT lt_note_logs ASSIGNING <row1>.
        assign_msg_fields <row1>.
        IF <ty> IS ASSIGNED AND <id> IS ASSIGNED AND <no> IS ASSIGNED.

          " Derive per-message severity from MSGTY
          lv_msg_severity = 0.
          CASE <ty>.
            WHEN 'E' OR 'A' OR 'X'. lv_msg_severity = 8.
            WHEN 'W'.               lv_msg_severity = 4.
            WHEN OTHERS.            lv_msg_severity = 0.                  " S/I etc.
          ENDCASE.

          " Track overall highest severity
          IF lv_msg_severity > lv_return_code.
            lv_return_code = lv_msg_severity.
          ENDIF.

          " Detect hard errors
          IF <ty> = 'E' OR <ty> = 'A' OR <ty> = 'X'.
            lv_failed = abap_true.
          ENDIF.

          " Detect "already implemented"
          IF <id> = 'SCWN_DARK' AND <no> = '071' AND <ty> = 'I'.
            lv_already_impl = abap_true.
          ENDIF.

          " Build message buffer (first line or append)
          IF lv_msg_text IS INITIAL.
            CONCATENATE '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text SEPARATED BY space.
          ELSE.
            CONCATENATE lv_msg_text lv_nl '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " OBJECT logs
      LOOP AT lt_object_logs ASSIGNING <row2>.
        assign_msg_fields <row2>.
        IF <ty> IS ASSIGNED AND <id> IS ASSIGNED AND <no> IS ASSIGNED.

          lv_msg_severity = 0.
          CASE <ty>.
            WHEN 'E' OR 'A' OR 'X'. lv_msg_severity = 8.
            WHEN 'W'.               lv_msg_severity = 4.
            WHEN OTHERS.            lv_msg_severity = 0.
          ENDCASE.

          IF lv_msg_severity > lv_return_code.
            lv_return_code = lv_msg_severity.
          ENDIF.

          IF <ty> = 'E' OR <ty> = 'A' OR <ty> = 'X'.
            lv_failed = abap_true.
          ENDIF.

          IF lv_msg_text IS INITIAL.
            CONCATENATE '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text SEPARATED BY space.
          ELSE.
            CONCATENATE lv_msg_text lv_nl '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " GENERIC logs
      LOOP AT lt_generic_logs ASSIGNING <row3>.
        assign_msg_fields <row3>.
        IF <ty> IS ASSIGNED AND <id> IS ASSIGNED AND <no> IS ASSIGNED.

          lv_msg_severity = 0.
          CASE <ty>.
            WHEN 'E' OR 'A' OR 'X'. lv_msg_severity = 8.
            WHEN 'W'.               lv_msg_severity = 4.
            WHEN OTHERS.            lv_msg_severity = 0.
          ENDCASE.

          IF lv_msg_severity > lv_return_code.
            lv_return_code = lv_msg_severity.
          ENDIF.

          IF <ty> = 'E' OR <ty> = 'A' OR <ty> = 'X'.
            lv_failed = abap_true.
          ENDIF.

          IF lv_msg_text IS INITIAL.
            CONCATENATE '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text SEPARATED BY space.
          ELSE.
            CONCATENATE lv_msg_text lv_nl '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " UDO logs
      LOOP AT lt_udo_logs ASSIGNING <row4>.
        assign_msg_fields <row4>.
        IF <ty> IS ASSIGNED AND <id> IS ASSIGNED AND <no> IS ASSIGNED.

          lv_msg_severity = 0.
          CASE <ty>.
            WHEN 'E' OR 'A' OR 'X'. lv_msg_severity = 8.
            WHEN 'W'.               lv_msg_severity = 4.
            WHEN OTHERS.            lv_msg_severity = 0.
          ENDCASE.

          IF lv_msg_severity > lv_return_code.
            lv_return_code = lv_msg_severity.
          ENDIF.

          IF <ty> = 'E' OR <ty> = 'A' OR <ty> = 'X'.
            lv_failed = abap_true.
          ENDIF.

          IF lv_msg_text IS INITIAL.
            CONCATENATE '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text SEPARATED BY space.
          ELSE.
            CONCATENATE lv_msg_text lv_nl '[' <ty> ']' <id> '-' <no> <v1> <v2> <v3> <v4>
                   INTO lv_msg_text.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " If the API said "already implemented" in logs, return that explicitly
      IF lv_already_impl = abap_true.
        ls_request-status      = 'Already Implemented'.
        ls_request-message     = 'This SNOTE number is already implemented.'.
        ls_request-transport   = lv_trkorr.                                " Include transport in response
        ls_request-returncode  = 0.                                       " treat as success
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      " If any error message → failed
      IF lv_failed = abap_true.
        ls_request-status = 'Implementation Failed'.

        CLEAR ls_request-message.
        CONCATENATE
          'SAP Note'        lv_note_number
          'implementation failed in the provided transport'
          lv_trkorr
          '- please use the correct, modifiable Workbench transport request for this Note number.'
          INTO ls_request-message SEPARATED BY space.

        ls_request-transport   = lv_trkorr.
        ls_request-returncode  = lv_return_code.                          " will be 8 in case of error
        er_entity              = ls_request.
        RETURN.
      ENDIF.

      "----------- Success --------------------------
      COMMIT WORK AND WAIT.                                                " Persist changes synchronously
      ls_request-status = 'Implementation Completed'.

      CONCATENATE 'SAP Note' lv_note_number 'successfully implemented in transport' lv_trkorr
        INTO ls_request-message SEPARATED BY space.

      ls_request-transport   = lv_trkorr.
      ls_request-returncode  = lv_return_code.                            " 0 = clean, 4 = with warnings
      er_entity              = ls_request.
      RETURN.

    CATCH cx_root INTO DATA(lx_any).                                       " Final safety net for unexpected exceptions
      ls_request-status      = 'Implementation Error'.
      ls_request-message     = lx_any->get_text( ).
      IF ls_request-message IS INITIAL.
        ls_request-message = 'Unhandled backend error while implementing SNOTE (see /IWFND/ERROR_LOG).'.
      ENDIF.
      ls_request-transport   = lv_trkorr.
      ls_request-returncode  = 8.
      er_entity              = ls_request.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD snoterequests_get_entityset.

  " This method is required for CSRF token but can return empty result
  DATA: lt_requests TYPE TABLE OF zcl_z_snote_automation_mpc=>ts_snoterequest.

  " Return empty entity set for CSRF token requests
  et_entityset = lt_requests.

ENDMETHOD.
ENDCLASS.
