class ZCL_ZSNOTE_PDF_ENTI_01_DPC_EXT definition
  public
  inheriting from ZCL_ZSNOTE_PDF_ENTI_01_DPC
  create public .

public section.
protected section.

  methods SNOTERESPONSESET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSNOTE_PDF_ENTI_01_DPC_EXT IMPLEMENTATION.


METHOD snoteresponseset_create_entity.

  "======== Declarations ========
  DATA: ls_req      TYPE zcl_zsnote_pdf_enti_01_mpc=>ts_z_snote_res,
        lt_numbers  TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
        lv_item     TYPE string,
        lv_num_in   TYPE c LENGTH 10,
        lv_num      TYPE c LENGTH 10,
        lv_html     TYPE string,
        lv_html_x   TYPE xstring,
        lv_msg      TYPE string,
        lv_added    TYPE i,
        lv_failed   TYPE i.

  DATA: ls_key      TYPE cwbntkeylg,
        lv_name     TYPE string,
        lv_final    TYPE string,
        lv_added_c  TYPE c LENGTH 10,
        lv_failed_c TYPE c LENGTH 10.

  DATA: lo_zip      TYPE REF TO cl_abap_zip,
        lv_zip_x    TYPE xstring.

  " From CWBNTDATA (note data with language)
  DATA: lt_data      TYPE STANDARD TABLE OF cwbntdata,
        ls_data      TYPE cwbntdata,
        ls_data_best TYPE cwbntdata.

  DATA: lv_try_lang1 TYPE cwbntdata-langu,
        lv_try_lang2 TYPE cwbntdata-langu.

  " For on-demand download of missing notes
  DATA: lt_note_keys_lg TYPE bcwbn_note_keys_lg,
        ls_note_key_lg  TYPE cwbntkeylg,
        lv_call_subrc   TYPE sy-subrc.

  "======== 1) Read request ========
  io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

  IF ls_req-notenumber IS INITIAL.
    er_entity = VALUE #(
      notenumber  = ls_req-notenumber
      version     = ''
      language    = ''
      status      = 'E'
      message     = 'Notenumber list required (comma-separated)'
      htmlcontent = VALUE xstring( )
    ).
    RETURN.
  ENDIF.

  "======== 2) Split comma-separated ========
  SPLIT ls_req-notenumber AT ',' INTO TABLE lt_numbers.

  CREATE OBJECT lo_zip.
  CLEAR: lv_added, lv_failed, lv_msg.

  "======== 3) Loop over each note number ========
  LOOP AT lt_numbers INTO lv_item.

    " cleanup input (remove spaces, uppercase)
    TRANSLATE lv_item TO UPPER CASE.
    CONDENSE lv_item NO-GAPS.
    IF lv_item IS INITIAL.
      CONTINUE.
    ENDIF.

    " Keep original (for filename)
    lv_num_in = lv_item.

    " normalize to NUMM (10 chars, leading zeros)
    lv_num = lv_item.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_num
      IMPORTING
        output = lv_num.

    "---- 3.1 Try to get note data from local system (CWBNTDATA) ----
    CLEAR: lt_data, ls_data, ls_data_best.
    SELECT *
      FROM cwbntdata
      INTO TABLE @lt_data
      WHERE numm = @lv_num.

    "---- 3.1.a If not found locally, try to download note into system ----
    IF sy-subrc <> 0 OR lt_data IS INITIAL.

      CLEAR: lt_note_keys_lg, ls_note_key_lg, lv_call_subrc.

      ls_note_key_lg-numm  = lv_num.
      ls_note_key_lg-langu = sy-langu.
      APPEND ls_note_key_lg TO lt_note_keys_lg.

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

      ENDTRY.

      " After download, try again to read from CWBNTDATA
      IF lv_call_subrc = 0.
        CLEAR: lt_data, ls_data, ls_data_best.
        SELECT *
          FROM cwbntdata
          INTO TABLE @lt_data
          WHERE numm = @lv_num.
      ENDIF.

    ENDIF.

    " If still no data -> final fail for this note
    IF lt_data IS INITIAL.
      ADD 1 TO lv_failed.
      CONCATENATE lv_msg '[' lv_num_in ': no note data found (local + download)]'
             INTO lv_msg SEPARATED BY space.
      CONTINUE.
    ENDIF.

    " Preferred languages
    lv_try_lang1 = sy-langu.
    IF lv_try_lang1 IS INITIAL.
      lv_try_lang1 = 'E'.
    ENDIF.
    lv_try_lang2 = 'E'.

    "---- 3.1.1 Try logon language, highest version ----
    LOOP AT lt_data INTO ls_data WHERE langu = lv_try_lang1.
      IF ls_data_best-numm IS INITIAL OR ls_data-versno > ls_data_best-versno.
        ls_data_best = ls_data.
      ENDIF.
    ENDLOOP.

    "---- 3.1.2 If not found, try English, highest version ----
    IF ls_data_best-numm IS INITIAL.
      LOOP AT lt_data INTO ls_data WHERE langu = lv_try_lang2.
        IF ls_data_best-numm IS INITIAL OR ls_data-versno > ls_data_best-versno.
          ls_data_best = ls_data.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "---- 3.1.3 If still not found, take any language, highest version ----
    IF ls_data_best-numm IS INITIAL.
      LOOP AT lt_data INTO ls_data.
        IF ls_data_best-numm IS INITIAL OR ls_data-versno > ls_data_best-versno.
          ls_data_best = ls_data.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ls_data_best-numm IS INITIAL.
      ADD 1 TO lv_failed.
      CONCATENATE lv_msg '[' lv_num_in ': no suitable version/language]'
             INTO lv_msg SEPARATED BY space.
      CONTINUE.
    ENDIF.

    "---- 3.2 Read HTML for that (note,version,language) ----
    CLEAR: lv_html, ls_key.

    ls_key-numm   = ls_data_best-numm.
    ls_key-versno = ls_data_best-versno.
    ls_key-langu  = ls_data_best-langu.

    CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
      EXPORTING
        is_note_key_lg = ls_key
      IMPORTING
        html_text      = lv_html
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0 OR lv_html IS INITIAL.
      ADD 1 TO lv_failed.
      CONCATENATE lv_msg '[' lv_num_in ': no HTML found]'
             INTO lv_msg SEPARATED BY space.
      CONTINUE.
    ENDIF.

    "---- 3.3 String -> XSTRING ----
    CLEAR lv_html_x.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_html
      IMPORTING
        buffer = lv_html_x
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0 OR lv_html_x IS INITIAL.
      ADD 1 TO lv_failed.
      CONCATENATE lv_msg '[' lv_num_in ': convert to xstring failed]'
             INTO lv_msg SEPARATED BY space.
      CONTINUE.
    ENDIF.

    "---- 3.4 Add file to ZIP as <original>.html ----
    CLEAR lv_name.
    CONCATENATE lv_num_in '.html' INTO lv_name.

    lo_zip->add(
      name    = lv_name
      content = lv_html_x
    ).

    ADD 1 TO lv_added.

  ENDLOOP.

  "======== 4) Finalize response ========
  IF lv_added = 0.
    IF lv_msg IS INITIAL.
      lv_msg = 'No HTML files created'.
    ENDIF.
    er_entity = VALUE #(
      notenumber  = ls_req-notenumber
      version     = ''
      language    = ''
      status      = 'E'
      message     = lv_msg
      htmlcontent = VALUE xstring( )
    ).
    RETURN.
  ENDIF.

  " Save ZIP
  lv_zip_x = lo_zip->save( ).

  CLEAR: lv_added_c, lv_failed_c.
  WRITE lv_added  TO lv_added_c.
  WRITE lv_failed TO lv_failed_c.

  CONCATENATE 'ZIP ready. Files:' lv_added_c INTO lv_final SEPARATED BY space.
  IF lv_failed > 0.
    CONCATENATE lv_final ', Failed:' lv_failed_c INTO lv_final SEPARATED BY space.
  ENDIF.
  IF lv_msg IS NOT INITIAL.
    CONCATENATE lv_final lv_msg INTO lv_final SEPARATED BY space.
  ENDIF.

  er_entity = VALUE #(
    notenumber  = ls_req-notenumber
    version     = ''
    language    = ''
    status      = 'S'
    message     = lv_final
    htmlcontent = lv_zip_x
  ).

ENDMETHOD.
ENDCLASS.
