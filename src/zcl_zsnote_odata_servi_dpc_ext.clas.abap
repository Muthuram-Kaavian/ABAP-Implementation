class ZCL_ZSNOTE_ODATA_SERVI_DPC_EXT definition
  public
  inheriting from ZCL_ZSNOTE_ODATA_SERVI_DPC
  create public .

public section.
protected section.

  methods NOTEPDFSET_GET_ENTITY
    redefinition .
  methods NOTEPDFSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSNOTE_ODATA_SERVI_DPC_EXT IMPLEMENTATION.


METHOD notepdfset_get_entity.

  " 1. Hardcoded inputs (same as your report)
  DATA(lv_note_number) = '3164537'.
  DATA(lv_version)     = '0002'.
  DATA(lv_language)    = 'E'.

  " 2. Call SCWB_NOTE_READ_HTML_TEXT function
  DATA: ls_note_key TYPE cwbntkeylg,
        lv_html     TYPE string.

  " Prepare key (same as your report)
  ls_note_key-numm    = lv_note_number.
  ls_note_key-versno  = lv_version.
  ls_note_key-langu   = lv_language.

  " Call function (same as your report)
  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_note_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  " 3. Check if HTML received
  IF sy-subrc <> 0 OR lv_html IS INITIAL.
    " If error, return empty
    CLEAR er_entity.
    RETURN.
  ENDIF.

  " 4. Convert HTML to XSTRING (same as your report)
  DATA: lv_pdf_buffer TYPE xstring.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_html
    IMPORTING
      buffer = lv_pdf_buffer
    EXCEPTIONS
      OTHERS = 1.

  " 5. Return response
  er_entity-notenumber   = lv_note_number.
  er_entity-version      = lv_version.
  er_entity-language     = lv_language.
  er_entity-pdfcontent   = lv_pdf_buffer.   " PDF buffer
  " er_entity-htmlcontent  = lv_html.       " If you want HTML also

ENDMETHOD.


METHOD notepdfset_get_entityset.

  " Hardcoded values (your example)
DATA(lv_note_number) = '3164537'.
DATA(lv_version) = '0002'.
DATA(lv_language) = 'E'.
  " HTML from SCWB
  DATA: ls_note_key TYPE cwbntkeylg,
        lv_html     TYPE string.

  ls_note_key-numm   = lv_note_number.
  ls_note_key-versno = lv_version.
  ls_note_key-langu  = lv_language.

  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_note_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  " Output table
  DATA lt_entityset TYPE TABLE OF zcl_zsnote_odata_servi_mpc=>ts_notepd001.

  " XSTRING buffer for HTML (not PDF)
  DATA lv_html_xstr TYPE xstring.

  IF sy-subrc = 0 AND lv_html IS NOT INITIAL.

    " Convert HTML → XSTRING
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = lv_html
        encoding = 'UTF-8'
      IMPORTING
        buffer   = lv_html_xstr.

    " Add response row
    APPEND VALUE zcl_zsnote_odata_servi_mpc=>ts_notepd001(
      notenumber = lv_note_number
      version    = lv_version
      language   = lv_language
      htmlcontent = lv_html
      pdfcontent  = lv_html_xstr
    ) TO lt_entityset.

  ENDIF.

  et_entityset = lt_entityset.

ENDMETHOD.
ENDCLASS.
