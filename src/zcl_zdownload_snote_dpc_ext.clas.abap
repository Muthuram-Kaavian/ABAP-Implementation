class ZCL_ZDOWNLOAD_SNOTE_DPC_EXT definition
  public
  inheriting from ZCL_ZDOWNLOAD_SNOTE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZDOWNLOAD_SNOTE_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  CASE iv_action_name.

    WHEN 'DownloadNotes'.

      DATA: lt_note_keys_lg TYPE bcwbn_note_keys_lg,
            ls_note_key_lg  TYPE cwbntkeylg,
            ls_parameter    TYPE /iwbep/s_mgw_name_value_pair,
            lt_status       TYPE STANDARD TABLE OF zcl_zdownload_snote_mpc=>ts_notestatus,
            ls_status       LIKE LINE OF lt_status,
            lv_call_subrc   TYPE sy-subrc.

      "Request parsing
      DATA: lt_raw_tokens   TYPE STANDARD TABLE OF string,
            lt_note_list    TYPE STANDARD TABLE OF string,
            lt_note_list_u  TYPE STANDARD TABLE OF string,
            lv_raw          TYPE string,
            lv_token        TYPE string.

      "HTML read buffers
      DATA: lv_html       TYPE string,
            lv_html_x     TYPE xstring,
            ls_key_lg     TYPE cwbntkeylg,
            lv_num_norm   TYPE cwbntdata-numm,
            lv_try_lang1  TYPE cwbntdata-langu,
            lv_try_lang2  TYPE cwbntdata-langu.

      "ZIP
      DATA: lo_zip        TYPE REF TO cl_abap_zip,
            lv_zip_x      TYPE xstring,
            lv_fname      TYPE string.

      "Final aggregated response (single row)
      DATA: lt_out        TYPE STANDARD TABLE OF zcl_zdownload_snote_mpc=>ts_notestatus,
            ls_out        TYPE zcl_zdownload_snote_mpc=>ts_notestatus,
            lv_all_note   TYPE string,
            lv_all_msg    TYPE string,
            lv_all_ver    TYPE string,
            lv_all_stext  TYPE string,
            lv_sep        TYPE string VALUE ','.

      DATA: lv_count TYPE i.

      "Single filename
      DATA: lv_single_fname TYPE string.

      "Range table inside class (NO RANGES statement)
      DATA: lr_numm TYPE RANGE OF cwbntdata-numm.
      DATA: ls_r    LIKE LINE OF lr_numm.

      "All cwbntdata in memory (one DB call)
      DATA: lt_all_data TYPE STANDARD TABLE OF cwbntdata,
            ls_all      TYPE cwbntdata,
            ls_best     TYPE cwbntdata.

      "========================================================
      " NEW: Threshold + Excel (XLS) buffer for > 5 notes
      "========================================================
      CONSTANTS: gc_threshold TYPE i VALUE 5.

      DATA: lv_crlf     TYPE string,
            lv_xls_html TYPE string,
            lv_xstr     TYPE xstring,
            lv_b64      TYPE string,
            lv_esc      TYPE string.

      "------------------------------------------------------------
      "1) REQUEST PARSING (multi params + comma separated)
      "------------------------------------------------------------
      CLEAR lt_note_list.

      LOOP AT it_parameter INTO ls_parameter WHERE name = 'NoteNumber'.
        lv_raw = ls_parameter-value.
        REPLACE ALL OCCURRENCES OF '''' IN lv_raw WITH ''.
        CONDENSE lv_raw NO-GAPS.

        CLEAR lt_raw_tokens.
        SPLIT lv_raw AT ',' INTO TABLE lt_raw_tokens.

        LOOP AT lt_raw_tokens INTO lv_token.
          CONDENSE lv_token NO-GAPS.
          IF lv_token IS INITIAL.
            CONTINUE.
          ENDIF.
          APPEND lv_token TO lt_note_list.
        ENDLOOP.
      ENDLOOP.

      "Remove duplicates keep order
      CLEAR lt_note_list_u.
      LOOP AT lt_note_list INTO lv_token.
        READ TABLE lt_note_list_u WITH KEY table_line = lv_token TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND lv_token TO lt_note_list_u.
        ENDIF.
      ENDLOOP.
      lt_note_list = lt_note_list_u.

      "------------------------------------------------------------
      "2) Build keys + status
      "------------------------------------------------------------
      CLEAR: lt_note_keys_lg, lt_status.

      LOOP AT lt_note_list INTO lv_token.
        CLEAR ls_note_key_lg.
        ls_note_key_lg-numm  = lv_token.
        ls_note_key_lg-langu = sy-langu.
        APPEND ls_note_key_lg TO lt_note_keys_lg.

        CLEAR ls_status.
        ls_status-notenumber = lv_token.
        APPEND ls_status TO lt_status.
      ENDLOOP.

      IF lt_note_keys_lg IS INITIAL.
        CLEAR ls_out.
        ls_out-notenumber = 'N/A'.
        ls_out-shorttext  = 'Error'.
        ls_out-version    = 'N/A'.
        ls_out-message    = 'No SAP Notes provided in request'.

        ls_out-filetype   = 'N/A'.
        ls_out-filename   = 'N/A'.

        APPEND ls_out TO lt_out.

        copy_data_to_ref(
          EXPORTING is_data = lt_out
          CHANGING  cr_data = er_data
        ).
        RETURN.
      ENDIF.

      DESCRIBE TABLE lt_status LINES lv_count.

      "------------------------------------------------------------
      "3) Read note BEFORE download
      "------------------------------------------------------------
      DATA: ls_note_before TYPE bcwbn_note.

      LOOP AT lt_status INTO ls_status.
        CLEAR: ls_note_before, lv_call_subrc.

        ls_note_before-key-numm = ls_status-notenumber.
        ls_note_before-langu    = sy-langu.

        TRY.
            CALL FUNCTION 'SCWB_NOTE_READ'
              EXPORTING
                iv_read_short_text = abap_true
              CHANGING
                cs_note            = ls_note_before
              EXCEPTIONS
                OTHERS             = 1.

            lv_call_subrc = sy-subrc.
            IF lv_call_subrc = 0 AND ls_note_before IS NOT INITIAL.
              ls_status-version   = ls_note_before-key-versno.
              ls_status-shorttext = ls_note_before-stext.
            ENDIF.

          CATCH cx_root INTO DATA(lx_error1).
            ls_status-message = lx_error1->get_text( ).
        ENDTRY.

        MODIFY lt_status FROM ls_status.
      ENDLOOP.

      "------------------------------------------------------------
      "4) Download notes (bulk)
      "------------------------------------------------------------
      CLEAR lv_call_subrc.

      TRY.
          CALL FUNCTION 'SCWN_NOTES_DOWNLOAD'
            EXPORTING
              it_note_keys_lg = lt_note_keys_lg
            EXCEPTIONS
              error           = 1
              rfc_error       = 2
              OTHERS          = 3.
          lv_call_subrc = sy-subrc.
        CATCH cx_root INTO DATA(lx_download_error).
          lv_call_subrc = 8.
      ENDTRY.

      "------------------------------------------------------------
      "5) OPTIMIZATION: Fetch all CWBNTDATA in ONE DB call
      "------------------------------------------------------------
      CLEAR lr_numm.

      LOOP AT lt_status INTO ls_status.
        lv_num_norm = ls_status-notenumber.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING  input  = lv_num_norm
          IMPORTING  output = lv_num_norm.

        CLEAR ls_r.
        ls_r-sign   = 'I'.
        ls_r-option = 'EQ'.
        ls_r-low    = lv_num_norm.
        APPEND ls_r TO lr_numm.
      ENDLOOP.

      SORT lr_numm BY low.
      DELETE ADJACENT DUPLICATES FROM lr_numm COMPARING low.

      CLEAR lt_all_data.
      IF lr_numm[] IS NOT INITIAL.
        SELECT * FROM cwbntdata
          INTO TABLE lt_all_data
          WHERE numm IN lr_numm.
      ENDIF.

      "Init ZIP only if >1 note (UNCHANGED logic behavior)
      IF lv_count > 1.
        CREATE OBJECT lo_zip.
      ENDIF.

      "------------------------------------------------------------
      "6) After download: read details + impl status + HTML
      "------------------------------------------------------------
      DATA: ls_note_details TYPE bcwbn_note,
            lv_impl_status TYPE cwbprstat,
            lv_status_text TYPE string.

      CONSTANTS: gc_prstat_not_implemented TYPE cwbprstat VALUE 'N',
                 gc_prstat_obsolete        TYPE cwbprstat VALUE 'O',
                 gc_prstat_implemented     TYPE cwbprstat VALUE 'I',
                 gc_prstat_partial         TYPE cwbprstat VALUE 'P',
                 gc_prstat_completed       TYPE cwbprstat VALUE 'E',
                 gc_prstat_cannot_impl     TYPE cwbprstat VALUE '-'.

      LOOP AT lt_status INTO ls_status.

        CLEAR: ls_note_details, lv_impl_status, lv_status_text,
               lv_html, lv_html_x, ls_best.

        ls_note_details-key-numm = ls_status-notenumber.
        ls_note_details-langu    = sy-langu.

        TRY.
            CALL FUNCTION 'SCWB_NOTE_READ'
              EXPORTING
                iv_read_short_text          = abap_true
                iv_read_corr_instructions   = abap_true
                iv_read_customer_attributes = abap_true
              CHANGING
                cs_note                     = ls_note_details
              EXCEPTIONS
                OTHERS                      = 1.

            IF sy-subrc = 0 AND ls_note_details IS NOT INITIAL.
              ls_status-shorttext = ls_note_details-stext.
              ls_status-version   = ls_note_details-key-versno.
            ELSE.
              ls_status-shorttext = 'Note not found'.
              ls_status-version   = 'N/A'.
            ENDIF.

          CATCH cx_root INTO DATA(lx_read_error2).
            ls_status-message = lx_read_error2->get_text( ).
            MODIFY lt_status FROM ls_status.
            CONTINUE.
        ENDTRY.

        TRY.
            CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
              EXPORTING
                is_note    = ls_note_details
              IMPORTING
                ev_status  = lv_impl_status
              EXCEPTIONS
                OTHERS     = 5.

            IF sy-subrc = 0.
              CASE lv_impl_status.
                WHEN gc_prstat_obsolete. " = 'O'
                  lv_status_text = 'Obsolete - Note is no longer valid (cannot be implemented)'.

                WHEN 'V'.
                  lv_status_text = 'Obsolete version implemented - A newer version is available and can be implemented'.
                WHEN gc_prstat_not_implemented OR 'C'.
                  lv_status_text = 'Can be Implemented - Ready for implementation'.
                WHEN gc_prstat_implemented.
                  lv_status_text = 'Implemented - Already applied to system'.
                WHEN gc_prstat_partial.
                  lv_status_text = 'Partially Implemented - Some corrections applied'.
                WHEN gc_prstat_completed.
                  lv_status_text = 'Completely Implemented - All corrections applied'.
                WHEN gc_prstat_cannot_impl OR 'U'.
                  lv_status_text = 'Cannot be Implemented - Manual intervention required'.
                WHEN OTHERS.
                  lv_status_text = lv_impl_status.
              ENDCASE.
              ls_status-message = lv_status_text.
            ELSE.
              ls_status-message = 'Status check failed'.
            ENDIF.

          CATCH cx_root INTO DATA(lx_status_error2).
            ls_status-message = lx_status_error2->get_text( ).
        ENDTRY.

        lv_num_norm = ls_status-notenumber.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING  input  = lv_num_norm
          IMPORTING  output = lv_num_norm.

        lv_try_lang1 = sy-langu.
        IF lv_try_lang1 IS INITIAL.
          lv_try_lang1 = 'E'.
        ENDIF.
        lv_try_lang2 = 'E'.

        CLEAR ls_best.

        LOOP AT lt_all_data INTO ls_all.
          IF ls_all-numm = lv_num_norm AND ls_all-langu = lv_try_lang1.
            IF ls_best-numm IS INITIAL OR ls_all-versno > ls_best-versno.
              ls_best = ls_all.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF ls_best-numm IS INITIAL.
          LOOP AT lt_all_data INTO ls_all.
            IF ls_all-numm = lv_num_norm AND ls_all-langu = lv_try_lang2.
              IF ls_best-numm IS INITIAL OR ls_all-versno > ls_best-versno.
                ls_best = ls_all.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF ls_best-numm IS INITIAL.
          LOOP AT lt_all_data INTO ls_all.
            IF ls_all-numm = lv_num_norm.
              IF ls_best-numm IS INITIAL OR ls_all-versno > ls_best-versno.
                ls_best = ls_all.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF ls_best-numm IS NOT INITIAL.
          CLEAR ls_key_lg.
          ls_key_lg-numm   = ls_best-numm.
          ls_key_lg-versno = ls_best-versno.
          ls_key_lg-langu  = ls_best-langu.

          CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
            EXPORTING  is_note_key_lg = ls_key_lg
            IMPORTING  html_text      = lv_html
            EXCEPTIONS OTHERS         = 1.

          IF sy-subrc = 0 AND lv_html IS NOT INITIAL.
            CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
              EXPORTING  text   = lv_html
              IMPORTING  buffer = lv_html_x
              EXCEPTIONS OTHERS = 1.
          ENDIF.
        ENDIF.

        IF lv_count = 1.
          ls_status-htmlcontent = lv_html_x.
        ELSE.
          IF lo_zip IS BOUND AND lv_html_x IS NOT INITIAL.
            CLEAR lv_fname.
            lv_fname = ls_status-notenumber.
            CONCATENATE lv_fname '.html' INTO lv_fname.
            lo_zip->add( name = lv_fname content = lv_html_x ).
          ENDIF.
        ENDIF.

        MODIFY lt_status FROM ls_status.

      ENDLOOP.

      IF lv_count > 1 AND lo_zip IS BOUND.
        lv_zip_x = lo_zip->save( ).
      ENDIF.

      "------------------------------------------------------------
      "7) Build aggregated strings (UNCHANGED)
      "------------------------------------------------------------
      CLEAR: ls_out, lv_all_note, lv_all_msg, lv_all_ver, lv_all_stext.

      LOOP AT lt_status INTO ls_status.
        IF lv_all_note IS INITIAL.
          lv_all_note  = ls_status-notenumber.
          lv_all_msg   = ls_status-message.
          lv_all_ver   = ls_status-version.
          lv_all_stext = ls_status-shorttext.
        ELSE.
          CONCATENATE lv_all_note  lv_sep ls_status-notenumber INTO lv_all_note.
          CONCATENATE lv_all_msg   lv_sep ls_status-message    INTO lv_all_msg.
          CONCATENATE lv_all_ver   lv_sep ls_status-version    INTO lv_all_ver.
          CONCATENATE lv_all_stext lv_sep ls_status-shorttext  INTO lv_all_stext.
        ENDIF.
      ENDLOOP.

      "------------------------------------------------------------
      "8) NEW: If > 5 notes => create XLS (Base64) with 4 fields
      "    (HTML/ZIP creation remains unchanged above)
      "------------------------------------------------------------
      CLEAR: lv_b64, lv_xls_html, lv_xstr.

      IF lv_count > gc_threshold.

        lv_crlf = cl_abap_char_utilities=>cr_lf.

        lv_xls_html =
          |<html><head><meta charset="UTF-8"></head><body>| && lv_crlf &&
          |<table border="1" cellspacing="0" cellpadding="5" width="100%">| && lv_crlf &&
          |<tr>| && lv_crlf &&
          |<th>NoteNumber</th>| && lv_crlf &&
          |<th>ShortText</th>| && lv_crlf &&
          |<th>Version</th>| && lv_crlf &&
          |<th>Message</th>| && lv_crlf &&
          |</tr>| && lv_crlf.

        LOOP AT lt_status INTO ls_status.

          "Escape ShortText
          lv_esc = ls_status-shorttext.
          REPLACE ALL OCCURRENCES OF '&' IN lv_esc WITH '&amp;'.
          REPLACE ALL OCCURRENCES OF '<' IN lv_esc WITH '&lt;'.
          REPLACE ALL OCCURRENCES OF '>' IN lv_esc WITH '&gt;'.
          DATA(lv_stext_esc) = lv_esc.

          "Escape Message
          lv_esc = ls_status-message.
          REPLACE ALL OCCURRENCES OF '&' IN lv_esc WITH '&amp;'.
          REPLACE ALL OCCURRENCES OF '<' IN lv_esc WITH '&lt;'.
          REPLACE ALL OCCURRENCES OF '>' IN lv_esc WITH '&gt;'.
          DATA(lv_msg_esc) = lv_esc.

          lv_xls_html = lv_xls_html &&
            |<tr>| && lv_crlf &&
            |<td>{ ls_status-notenumber }</td>| && lv_crlf &&
            |<td>{ lv_stext_esc }</td>| && lv_crlf &&
            |<td>{ ls_status-version }</td>| && lv_crlf &&
            |<td>{ lv_msg_esc }</td>| && lv_crlf &&
            |</tr>| && lv_crlf.

        ENDLOOP.

        lv_xls_html = lv_xls_html && |</table>| && lv_crlf && |</body></html>|.

        lv_xstr = cl_abap_codepage=>convert_to( source = lv_xls_html ).
        lv_b64  = cl_http_utility=>encode_x_base64( lv_xstr ).

      ENDIF.

      "------------------------------------------------------------
      "9) Final ls_out build
      "   - <=5: fields as-is (comma-separated), file stays HTML/ZIP
      "   - >5 : fields moved to XLS in BUFFER (Base64)
      "          HTML/ZIP still returned as before
      "------------------------------------------------------------
      IF lv_count <= gc_threshold.

        "As-is
        ls_out-notenumber = lv_all_note.
        ls_out-message    = lv_all_msg.
        ls_out-version    = lv_all_ver.
        ls_out-shorttext  = lv_all_stext.

      ELSE.

        "Details in Excel (buffer), keep entity fields short
        ls_out-notenumber = |{ lv_count } notes|.
        ls_out-shorttext  = |Details in XLS|.
        ls_out-version    = ''.
        ls_out-message    = |Download XLS for per-note Notenumber/ShortText/Version/Message.|.

        "Use model fields: name/type/buffer for the XLS download
        "name = filename, type = filetype, buffer = Base64 content
        ls_out-name   = |SNOTE_Details_{ sy-datum }_{ sy-uzeit }.xls|.
        ls_out-type   = 'XLS'.
        ls_out-buffer = lv_b64.

      ENDIF.

      "Keep your existing HTML/ZIP logic unchanged
      IF lv_count = 1.
        READ TABLE lt_status INTO ls_status INDEX 1.
        IF sy-subrc = 0.
          ls_out-htmlcontent = ls_status-htmlcontent.
        ENDIF.

        ls_out-filetype = 'HTML'.
        lv_single_fname = lv_all_note.
        CONCATENATE lv_single_fname '.html' INTO lv_single_fname.
        ls_out-filename = lv_single_fname.

      ELSE.
        ls_out-htmlcontent = lv_zip_x.
        ls_out-filetype    = 'ZIP'.
        ls_out-filename    = 'snotes.zip'.
      ENDIF.

      APPEND ls_out TO lt_out.

      copy_data_to_ref(
        EXPORTING is_data = lt_out
        CHANGING  cr_data = er_data
      ).

    WHEN OTHERS.
      "other actions
  ENDCASE.

ENDMETHOD.
ENDCLASS.
