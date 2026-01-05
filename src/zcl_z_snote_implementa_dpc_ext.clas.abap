class ZCL_Z_SNOTE_IMPLEMENTA_DPC_EXT definition
  public
  inheriting from ZCL_Z_SNOTE_IMPLEMENTA_DPC
  create public .

public section.
protected section.

  methods SNOTEREQUESTSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_SNOTE_IMPLEMENTA_DPC_EXT IMPLEMENTATION.


METHOD snoterequestset_create_entity.

  "=== Macro to assign MSG* fields from any log row ===
  DEFINE assign_msg_fields.
    ASSIGN COMPONENT 'MSGTY' OF STRUCTURE &1 TO <ty>.
    ASSIGN COMPONENT 'MSGID' OF STRUCTURE &1 TO <id>.
    ASSIGN COMPONENT 'MSGNO' OF STRUCTURE &1 TO <no>.
    ASSIGN COMPONENT 'MSGV1' OF STRUCTURE &1 TO <v1>.
    ASSIGN COMPONENT 'MSGV2' OF STRUCTURE &1 TO <v2>.
    ASSIGN COMPONENT 'MSGV3' OF STRUCTURE &1 TO <v3>.
    ASSIGN COMPONENT 'MSGV4' OF STRUCTURE &1 TO <v4>.
  END-OF-DEFINITION.

  "=== Macro to finalize one note result into batch ===
  DEFINE finalize_row.
    APPEND ls_result TO lt_batch.
    IF lv_overall_rc < ls_result-returncode.
      lv_overall_rc = ls_result-returncode.
    ENDIF.
    IF ls_result-returncode >= 8.
      lv_any_failed = abap_true.
    ELSEIF ls_result-returncode = 4.
      lv_any_warning = abap_true.
    ENDIF.
  END-OF-DEFINITION.

  "-------------------- Declarations --------------------
  DATA: ls_request      TYPE zcl_z_snote_implementa_mpc=>ts_snoterequest,
        lt_notes_raw    TYPE STANDARD TABLE OF string,
        lv_note_raw     TYPE string,
        lv_transport_in TYPE string,
        ls_result       TYPE zcl_z_snote_implementa_mpc=>ts_snoterequest,
        lt_batch        TYPE zcl_z_snote_implementa_mpc=>tt_snoterequest.

  DATA: lv_any_failed   TYPE abap_bool VALUE abap_false,
        lv_any_warning  TYPE abap_bool VALUE abap_false,
        lv_overall_rc   TYPE i VALUE 0.

  DATA: lv_note_number  TYPE cwbntnumm,
        lv_note_num10   TYPE cwbntnumm,
        lv_note_disp    TYPE string,
        lv_trkorr       TYPE trkorr,
        lv_caller_id    TYPE cwbcaller_id,
        lv_msg_text     TYPE string,
        lv_failed       TYPE abap_bool.

  DATA: lv_msg_all      TYPE string,
        lv_line         TYPE string,
        lv_rc_all       TYPE string,
        lv_rc_str       TYPE string,
        lv_return_code  TYPE i,
        lv_msg_severity TYPE i.

  DATA: ls_sched        TYPE scwn_dark_s_schedule_info,
        lt_note_headers TYPE cwbnt_t_head,
        ls_note_header  TYPE cwbnthead,
        lt_note_logs    TYPE scwn_dark_t_note_log_message,
        lt_object_logs  TYPE scwn_dark_t_log_message,
        lt_generic_logs TYPE scwn_dark_t_gen_log_message,
        lt_udo_logs     TYPE scwn_dark_t_udo_message.

  DATA: ls_e070         TYPE e070,
        lv_already_impl TYPE abap_bool.

  DATA: lv_note_objname1 TYPE e071-obj_name,
        lv_note_objname2 TYPE e071-obj_name.

  DATA: lo_sched_desc   TYPE REF TO cl_abap_structdescr,
        ls_hdr_comp     TYPE abap_compdescr.

  FIELD-SYMBOLS: <fs_any> TYPE any,
                 <row1>   TYPE any,
                 <row2>   TYPE any,
                 <row3>   TYPE any,
                 <row4>   TYPE any,
                 <ty>     TYPE any,
                 <id>     TYPE any,
                 <no>     TYPE any,
                 <v1>     TYPE any,
                 <v2>     TYPE any,
                 <v3>     TYPE any,
                 <v4>     TYPE any.

  DATA: ls_note_details TYPE bcwbn_note,
        lv_impl_status  TYPE cwbprstat.

  CONSTANTS: gc_prstat_obsolete    TYPE cwbprstat VALUE 'O',
             gc_prstat_implemented TYPE cwbprstat VALUE 'I',
             gc_prstat_completed   TYPE cwbprstat VALUE 'E',
             gc_prstat_cannot_impl TYPE cwbprstat VALUE '-',
             gc_prstat_manual      TYPE cwbprstat VALUE 'U'.

  "--- Request + tasks list (range) ---
  DATA: lt_tasks  TYPE STANDARD TABLE OF trkorr,
        lv_task   TYPE trkorr,
        lr_my_trs TYPE RANGE OF trkorr,
        ls_r      LIKE LINE OF lr_my_trs.

  "--- Types for another transport detection (no JOIN) ---
  TYPES: BEGIN OF ty_tr,
           trkorr TYPE trkorr,
         END OF ty_tr.

  TYPES: BEGIN OF ty_e070,
           trkorr  TYPE trkorr,
           strkorr TYPE trkorr,
           as4date TYPE as4date,
           as4time TYPE as4time,
         END OF ty_e070.

  DATA: lv_same_tr    TYPE trkorr,
        lt_note_trs   TYPE STANDARD TABLE OF ty_tr,
        lt_e070cand   TYPE STANDARD TABLE OF ty_e070,
        ls_e070cand   TYPE ty_e070,
        lv_other_tr   TYPE trkorr,
        lv_other_disp TYPE trkorr.

  "---- NEW: For downloadable XLS (HTML->Base64) ----
  CONSTANTS: gc_threshold TYPE i VALUE 5.

  DATA: lv_note_count TYPE i,
        lv_crlf       TYPE string,
        lv_html       TYPE string,
        lv_xstr       TYPE xstring,
        lv_b64        TYPE string,
        lv_msg_esc    TYPE string,
        lv_note_esc   TYPE string.

  DATA: lx_any TYPE REF TO cx_root.

  TRY.

      "----------- Read payload ------------
      io_data_provider->read_entry_data( IMPORTING es_data = ls_request ).

      IF ls_request-notenumber IS INITIAL OR ls_request-transport IS INITIAL.
        ls_request-status     = 'Error'.
        ls_request-message    = 'NoteNumber and Transport are required (NoteNumber can be comma-separated; Transport must be single).'.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      "----------- Normalize single Transport ----------
      lv_transport_in = ls_request-transport.
      CONDENSE lv_transport_in NO-GAPS.
      TRANSLATE lv_transport_in TO UPPER CASE.

      IF lv_transport_in CS ','.
        ls_request-status     = 'Error'.
        ls_request-message    = 'Transport must be a single request. Provide one modifiable Workbench transport.'.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      lv_trkorr = lv_transport_in.

      "----------- Split NoteNumbers (CSV) -------------
      SPLIT ls_request-notenumber AT ',' INTO TABLE lt_notes_raw.
      LOOP AT lt_notes_raw INTO lv_note_raw.
        CONDENSE lv_note_raw NO-GAPS.
        MODIFY lt_notes_raw FROM lv_note_raw INDEX sy-tabix.
      ENDLOOP.

      IF lt_notes_raw IS INITIAL.
        ls_request-status     = 'Error'.
        ls_request-message    = 'No valid NoteNumber provided.'.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      "----------- Batch audit ----------
      ls_request-createdby = sy-uname.
      GET TIME STAMP FIELD ls_request-createdat.

      IF ls_request-taskid IS INITIAL.
        TRY.
            ls_request-taskid = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            CONCATENATE sy-uname sy-datum sy-uzeit INTO ls_request-taskid.
        ENDTRY.
      ENDIF.

      "----------- Validate TR via E070 (ONCE) ----------
      CLEAR ls_e070.
      SELECT SINGLE * FROM e070 INTO ls_e070 WHERE trkorr = lv_trkorr.

      IF sy-subrc <> 0 OR ls_e070-trkorr IS INITIAL.
        ls_request-status     = 'Batch Failed'.
        ls_request-message    = |Transport "{ lv_trkorr }" could not be found. Please verify the transport request number.|.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      IF ls_e070-trstatus = 'R' OR ls_e070-trstatus = 'L'.
        ls_request-status     = 'Batch Failed'.
        ls_request-message    = |Transport "{ lv_trkorr }" is locked or released, cannot be used.|.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      IF ls_e070-trfunction <> 'K'.
        ls_request-status     = 'Batch Failed'.
        ls_request-message    = |Transport "{ lv_trkorr }" is not a Workbench request.|.
        ls_request-returncode = '8'.
        er_entity = ls_request.
        RETURN.
      ENDIF.

      "----------- Build my TR range (request + tasks) ----------
      CLEAR: lt_tasks, lr_my_trs.

      ls_r-sign = 'I'.
      ls_r-option = 'EQ'.
      ls_r-low = lv_trkorr.
      APPEND ls_r TO lr_my_trs.

      SELECT trkorr FROM e070 INTO TABLE lt_tasks WHERE strkorr = lv_trkorr.
      LOOP AT lt_tasks INTO lv_task.
        CLEAR ls_r.
        ls_r-sign = 'I'.
        ls_r-option = 'EQ'.
        ls_r-low = lv_task.
        APPEND ls_r TO lr_my_trs.
      ENDLOOP.

      "----------- Loop notes -----------
      LOOP AT lt_notes_raw INTO lv_note_raw.

        CLEAR: ls_result, lv_note_number, lv_note_num10, lv_note_disp, lv_caller_id,
               lv_msg_text, lv_failed, lv_return_code, lv_msg_severity,
               ls_sched, lt_note_headers, ls_note_header,
               lt_note_logs, lt_object_logs, lt_generic_logs, lt_udo_logs,
               lv_already_impl, ls_note_details, lv_impl_status,
               lv_note_objname1, lv_note_objname2,
               lv_same_tr, lt_note_trs, lt_e070cand, ls_e070cand, lv_other_tr, lv_other_disp.

        ls_result-createdby = ls_request-createdby.
        ls_result-createdat = ls_request-createdat.

        TRY.
            ls_result-taskid = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            CONCATENATE sy-uname sy-datum sy-uzeit INTO ls_result-taskid.
        ENDTRY.

        ls_result-notenumber = lv_note_raw.
        ls_result-transport  = lv_trkorr.

        " normalize note (technical + display)
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = lv_note_raw
          IMPORTING output = lv_note_num10.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING input  = lv_note_num10
          IMPORTING output = lv_note_disp.

        lv_note_number = lv_note_num10.

        "Always prepare NOTE objnames for TR lookup (even if impl fails)
        lv_note_objname1 = lv_note_num10.
        lv_note_objname2 = lv_note_raw.

        lv_caller_id = ls_result-taskid.
        IF lv_caller_id IS INITIAL.
          lv_caller_id = sy-uname.
        ENDIF.

        "----------- Note status check -----------
        CLEAR: ls_note_details, lv_impl_status.
        ls_note_details-key-numm = lv_note_number.
        ls_note_details-langu    = sy-langu.

        CALL FUNCTION 'SCWB_NOTE_READ'
          EXPORTING
            iv_read_short_text          = abap_true
            iv_read_corr_instructions   = abap_false
            iv_read_customer_attributes = abap_false
          CHANGING
            cs_note                     = ls_note_details
          EXCEPTIONS
            OTHERS                      = 1.

        IF sy-subrc = 0.

          CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
            EXPORTING  is_note   = ls_note_details
            IMPORTING  ev_status = lv_impl_status
            EXCEPTIONS note_not_found             = 1
                       inconsistent_delivery_data = 2
                       undefined_component_state  = 3
                       incomplete_note_data       = 4
                       OTHERS                     = 5.

          IF sy-subrc = 0.
            CASE lv_impl_status.

              WHEN gc_prstat_implemented OR gc_prstat_completed.

                "A) same request OR its tasks?
                CLEAR lv_same_tr.
                SELECT SINGLE trkorr
                  FROM e071
                  INTO lv_same_tr
                  WHERE trkorr IN lr_my_trs
                    AND pgmid  = 'R3TR'
                    AND object = 'NOTE'
                    AND ( obj_name = lv_note_objname1 OR obj_name = lv_note_objname2 ).

                IF sy-subrc = 0 AND lv_same_tr IS NOT INITIAL.
                  ls_result-status     = 'Already Implemented'.
                  ls_result-message    = |The SNOTE Number { lv_note_disp } is already implemented in the provided transport.|.
                  ls_result-returncode = 0.
                  finalize_row.
                  CONTINUE.
                ENDIF.

                "B) another transport? (exclude request+tasks)
                CLEAR: lv_other_tr, lv_other_disp, lt_note_trs, lt_e070cand, ls_e070cand.

                SELECT trkorr
                  FROM e071
                  INTO TABLE lt_note_trs
                  WHERE pgmid  = 'R3TR'
                    AND object = 'NOTE'
                    AND ( obj_name = lv_note_objname1 OR obj_name = lv_note_objname2 ).

                IF lt_note_trs IS NOT INITIAL.
                  SELECT trkorr strkorr as4date as4time
                    FROM e070
                    INTO TABLE lt_e070cand
                    FOR ALL ENTRIES IN lt_note_trs
                    WHERE trkorr = lt_note_trs-trkorr.

                  SORT lt_e070cand BY as4date DESCENDING as4time DESCENDING.

                  LOOP AT lt_e070cand INTO ls_e070cand.
                    IF ls_e070cand-trkorr IN lr_my_trs OR ls_e070cand-strkorr = lv_trkorr.
                      CONTINUE.
                    ENDIF.

                    lv_other_tr = ls_e070cand-trkorr.
                    IF ls_e070cand-strkorr IS NOT INITIAL.
                      lv_other_disp = ls_e070cand-strkorr.
                    ELSE.
                      lv_other_disp = ls_e070cand-trkorr.
                    ENDIF.
                    EXIT.
                  ENDLOOP.
                ENDIF.

                IF lv_other_tr IS NOT INITIAL.
                  ls_result-status     = 'Already Implemented'.
                  ls_result-message    = |The SNOTE Number { lv_note_disp } is already implemented in another transport { lv_other_disp }. please provide the correct transport details|.
                  ls_result-returncode = 0.
                  finalize_row.
                  CONTINUE.
                ELSE.
                  ls_result-status     = 'Already Implemented'.
                  ls_result-message    = |The SNOTE Number { lv_note_disp } is already implemented but transport could not be determined. Please verify in SNOTE/SE03|.
                  ls_result-returncode = 0.
                  finalize_row.
                  CONTINUE.
                ENDIF.

              WHEN gc_prstat_cannot_impl OR gc_prstat_manual.
                ls_result-status     = 'Cannot be Implemented'.
                ls_result-message    = |The SNOTE number { lv_note_disp } is in "Cannot be implemented" status in the system, so manual intervention is required.|.
                ls_result-returncode = 8.
                finalize_row.
                CONTINUE.

              WHEN gc_prstat_obsolete.
                ls_result-status     = 'Obsolete Note'.
                ls_result-message    = |SNOTE { lv_note_disp } is obsolete and cannot be implemented|.
                ls_result-returncode = 8.
                finalize_row.
                CONTINUE.

              WHEN OTHERS.
                "continue to implement

            ENDCASE.
          ENDIF.
        ENDIF.

        "----------- Build IT_NOTE_HEADERS ------------
        CLEAR lt_note_headers.
        CLEAR ls_note_header.
        ls_note_header-numm = lv_note_number.
        APPEND ls_note_header TO lt_note_headers.

        "----------- Make schedule silent -------------
        CLEAR ls_sched.
        lo_sched_desc ?= cl_abap_typedescr=>describe_by_data( ls_sched ).
        IF lo_sched_desc IS BOUND.
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
                <fs_any> = abap_true.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        "----------- Implement via DARK FM ------------
        CLEAR: lt_note_logs, lt_object_logs, lt_generic_logs, lt_udo_logs, lv_msg_text.
        lv_failed        = abap_false.
        lv_return_code   = 0.
        lv_already_impl  = abap_false.

        CALL FUNCTION 'SCWN_DARK_API_IMPL_MULTI_NOTES'
          EXPORTING
            iv_def_trans     = lv_trkorr
            iv_caller_id     = lv_caller_id
            is_schedule_info = ls_sched
          IMPORTING
            et_note_logs     = lt_note_logs
            et_object_logs   = lt_object_logs
            et_generic_logs  = lt_generic_logs
            et_udo_logs      = lt_udo_logs
          TABLES
            it_note_headers  = lt_note_headers
          EXCEPTIONS
            OTHERS           = 1.

        IF sy-subrc <> 0.
          IF sy-msgid IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid = sy-msgid msgnr = sy-msgno
                msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4
              IMPORTING
                message_text_output = lv_msg_text
              EXCEPTIONS OTHERS = 1.
          ENDIF.

          IF lv_msg_text IS INITIAL.
            lv_msg_text = 'SNOTE dark API returned non-zero subrc'.
          ENDIF.

          ls_result-status     = 'Implementation Error'.
          ls_result-message    = lv_msg_text.
          ls_result-returncode = 8.
          finalize_row.
          CONTINUE.
        ENDIF.

        "----------- Interpret logs (same logic) -----------
        lv_failed        = abap_false.
        lv_return_code   = 0.
        lv_already_impl  = abap_false.

        LOOP AT lt_note_logs ASSIGNING <row1>.
          assign_msg_fields <row1>.
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
            IF <id> = 'SCWN_DARK' AND <no> = '071' AND <ty> = 'I'.
              lv_already_impl = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.

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
          ENDIF.
        ENDLOOP.

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
          ENDIF.
        ENDLOOP.

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
          ENDIF.
        ENDLOOP.

        IF lv_already_impl = abap_true.
          ls_result-status     = 'Already Implemented'.
          ls_result-message    = |SNOTE { lv_note_disp } already implemented (DARK). Please verify in SNOTE/SE03|.
          ls_result-returncode = 0.
          finalize_row.
          CONTINUE.
        ENDIF.

        IF lv_failed = abap_true.
          ls_result-status = 'Implementation Failed'.

          "If we don't already know other TR, try to find from DB now
          IF lv_other_disp IS INITIAL.
            CLEAR: lv_other_tr, lt_note_trs, lt_e070cand, ls_e070cand.
            SELECT trkorr
              FROM e071
              INTO TABLE lt_note_trs
              WHERE pgmid  = 'R3TR'
                AND object = 'NOTE'
                AND ( obj_name = lv_note_objname1 OR obj_name = lv_note_objname2 ).

            IF lt_note_trs IS NOT INITIAL.
              SELECT trkorr strkorr as4date as4time
                FROM e070
                INTO TABLE lt_e070cand
                FOR ALL ENTRIES IN lt_note_trs
                WHERE trkorr = lt_note_trs-trkorr.

              SORT lt_e070cand BY as4date DESCENDING as4time DESCENDING.
              LOOP AT lt_e070cand INTO ls_e070cand.
                "Skip current request + its tasks
                IF ls_e070cand-trkorr IN lr_my_trs OR ls_e070cand-strkorr = lv_trkorr.
                  CONTINUE.
                ENDIF.

                lv_other_tr = ls_e070cand-trkorr.

                "If it's a task, show parent request; else show itself
                IF ls_e070cand-strkorr IS NOT INITIAL.
                  lv_other_disp = ls_e070cand-strkorr.
                ELSE.
                  lv_other_disp = ls_e070cand-trkorr.
                ENDIF.
                EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.

          IF lv_other_disp IS NOT INITIAL.
            CONCATENATE
              'SAP Note' lv_note_disp
              'implementation failed in the provided transport'
              lv_trkorr
              '- This SNOTE is available in transport'
              lv_other_disp
              '. Please use the correct modifiable Workbench transport request for implementing this Note.'
              INTO ls_result-message SEPARATED BY space.
          ELSE.
            CONCATENATE
              'SAP Note' lv_note_disp
              'implementation failed in the provided transport'
              lv_trkorr
              '. Please use the correct modifiable Workbench transport request for implementing this Note.'
              INTO ls_result-message SEPARATED BY space.
          ENDIF.

          ls_result-returncode = 8.
          finalize_row.
          CONTINUE.
        ENDIF.

        COMMIT WORK AND WAIT.

        ls_result-status = 'Implementation Completed'.
        CONCATENATE 'SAP Note' lv_note_disp
                    'successfully implemented in transport'
                    lv_trkorr
               INTO ls_result-message SEPARATED BY space.

        IF lv_return_code = 4.
          ls_result-returncode = 4.
        ELSE.
          ls_result-returncode = 0.
        ENDIF.

        finalize_row.

      ENDLOOP.

      "----------- Final overall response -------------
      IF lv_any_failed = abap_true.
        ls_request-status = 'Batch Failed'.
      ELSEIF lv_any_warning = abap_true OR lv_overall_rc = 4.
        ls_request-status = 'Batch Completed With Warnings'.
      ELSE.
        ls_request-status = 'Batch Completed'.
      ENDIF.

      lv_note_count = lines( lt_notes_raw ).

      "=========================================================
      " If more than threshold => return downloadable XLS buffer
      "=========================================================
      IF lv_note_count > gc_threshold.

        lv_crlf = cl_abap_char_utilities=>cr_lf.

        lv_html =
          |<html><head><meta charset="UTF-8"></head><body>| && lv_crlf &&
          |<table border="1" cellspacing="0" cellpadding="5" width="100%">| && lv_crlf &&
          |<tr>| && lv_crlf &&
          |<th>SapNoteNumber</th>| && lv_crlf &&
          |<th>Transport</th>| && lv_crlf &&
          |<th>ReturnCode</th>| && lv_crlf &&
          |<th>Status</th>| && lv_crlf &&
          |<th>Message</th>| && lv_crlf &&
          |</tr>| && lv_crlf.

        LOOP AT lt_batch INTO ls_result.

          lv_msg_esc = ls_result-message.
          REPLACE ALL OCCURRENCES OF '&' IN lv_msg_esc WITH '&amp;'.
          REPLACE ALL OCCURRENCES OF '<' IN lv_msg_esc WITH '&lt;'.
          REPLACE ALL OCCURRENCES OF '>' IN lv_msg_esc WITH '&gt;'.

          lv_note_esc = ls_result-notenumber.
          REPLACE ALL OCCURRENCES OF '&' IN lv_note_esc WITH '&amp;'.
          REPLACE ALL OCCURRENCES OF '<' IN lv_note_esc WITH '&lt;'.
          REPLACE ALL OCCURRENCES OF '>' IN lv_note_esc WITH '&gt;'.

          lv_html = lv_html && |<tr>| && lv_crlf &&
            |<td>{ lv_note_esc }</td>| && lv_crlf &&
            |<td>{ ls_result-transport }</td>| && lv_crlf &&
            |<td>{ ls_result-returncode }</td>| && lv_crlf &&
            |<td>{ ls_result-status }</td>| && lv_crlf &&
            |<td>{ lv_msg_esc }</td>| && lv_crlf &&
            |</tr>| && lv_crlf.

        ENDLOOP.

        lv_html = lv_html && |</table>| && lv_crlf && |</body></html>|.

        lv_xstr = cl_abap_codepage=>convert_to( source = lv_html ).
        lv_b64  = cl_http_utility=>encode_x_base64( lv_xstr ).

        "Set file fields dynamically (compile-safe)
        FIELD-SYMBOLS: <fc> TYPE any, <fn> TYPE any, <ft> TYPE any.

        ASSIGN COMPONENT 'FILECONTENT' OF STRUCTURE ls_request TO <fc>.
        IF sy-subrc <> 0.
          ASSIGN COMPONENT 'FileContent' OF STRUCTURE ls_request TO <fc>.
        ENDIF.

        ASSIGN COMPONENT 'FILENAME' OF STRUCTURE ls_request TO <fn>.
        IF sy-subrc <> 0.
          ASSIGN COMPONENT 'FileName' OF STRUCTURE ls_request TO <fn>.
        ENDIF.

        ASSIGN COMPONENT 'FILETYPE' OF STRUCTURE ls_request TO <ft>.
        IF sy-subrc <> 0.
          ASSIGN COMPONENT 'FileType' OF STRUCTURE ls_request TO <ft>.
        ENDIF.

        IF <fc> IS ASSIGNED.
          <fc> = lv_b64.
        ENDIF.
        IF <fn> IS ASSIGNED.
          <fn> = |SNOTE_Result_{ sy-datum }_{ sy-uzeit }.xls|.
        ENDIF.
        IF <ft> IS ASSIGNED.
          <ft> = 'XLS'.
        ENDIF.

        ls_request-message    = |{ lv_note_count } notes processed. Download the XLS for per-note details.|.
        ls_request-returncode = |{ lv_overall_rc }|.

        er_entity = ls_request.
        RETURN.

      ENDIF.

      "=========================================================
      " Else (<= threshold) => keep your current comma message
      "=========================================================
      CLEAR: lv_msg_all, lv_rc_all.

      LOOP AT lt_batch INTO ls_result.
        lv_line = ls_result-message.
        IF lv_msg_all IS INITIAL.
          CONCATENATE '{' lv_line INTO lv_msg_all SEPARATED BY space.
        ELSE.
          CONCATENATE lv_msg_all ',' lv_line INTO lv_msg_all SEPARATED BY space.
        ENDIF.

        lv_rc_str = |{ ls_result-returncode }|.
        IF lv_rc_all IS INITIAL.
          lv_rc_all = lv_rc_str.
        ELSE.
          CONCATENATE lv_rc_all ',' lv_rc_str INTO lv_rc_all.
        ENDIF.
      ENDLOOP.

      IF lv_msg_all IS NOT INITIAL.
        CONCATENATE lv_msg_all '}' INTO lv_msg_all SEPARATED BY space.
      ENDIF.

      ls_request-message    = lv_msg_all.
      ls_request-returncode = lv_rc_all.
      er_entity = ls_request.
      RETURN.

    CATCH cx_root INTO lx_any.
      ls_request-status  = 'Implementation Error'.
      ls_request-message = lx_any->get_text( ).
      IF ls_request-message IS INITIAL.
        ls_request-message = 'Unhandled backend error while implementing SNOTE batch (see /IWFND/ERROR_LOG).'.
      ENDIF.
      ls_request-returncode = '8'.
      er_entity = ls_request.
      RETURN.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
