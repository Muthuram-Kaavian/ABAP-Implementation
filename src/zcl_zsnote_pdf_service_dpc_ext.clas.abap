class ZCL_ZSNOTE_PDF_SERVICE_DPC_EXT definition
  public
  inheriting from ZCL_ZSNOTE_PDF_SERVICE_DPC
  create public .

public section.
protected section.

  methods NOTEPDFSET_GET_ENTITY
    redefinition .
  methods NOTEPDFSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSNOTE_PDF_SERVICE_DPC_EXT IMPLEMENTATION.


METHOD notepdfset_get_entity.

  DATA:
    lv_note_number TYPE c LENGTH 8,
    lv_version     TYPE c LENGTH 4,
    lv_language    TYPE c LENGTH 1,
    lv_html        TYPE string.

  " 1. Hardcoded values (same as entityset)
  lv_note_number = '3164537'.
  lv_version     = '0002'.
  lv_language    = 'E'.

  " 2. Validate
  IF lv_note_number IS INITIAL OR lv_version IS INITIAL OR lv_language IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'NoteNumber, Version, Language venum'.
  ENDIF.

  " 3. Prepare key for SCWB function
  DATA: ls_note_key TYPE cwbntkeylg.
  ls_note_key-numm    = lv_note_number.
  ls_note_key-versno  = lv_version.
  ls_note_key-langu   = lv_language.

  " 4. Call SCWB function to get HTML
  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_note_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0 OR lv_html IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'Note HTML content kedaikala'.
  ENDIF.

  " 5. Convert HTML to XSTRING (PDF buffer)
  DATA: lv_pdf_buffer TYPE xstring.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_html
    IMPORTING
      buffer = lv_pdf_buffer
    EXCEPTIONS
      OTHERS = 1.

  " 6. Fill response entity
  er_entity-notenumber   = lv_note_number.
  er_entity-version      = lv_version.
  er_entity-language     = lv_language.
  er_entity-pdfcontent   = lv_pdf_buffer.

ENDMETHOD.


METHOD notepdfset_get_entityset.

  DATA:
    ls_entity      TYPE zcl_zsnote_pdf_service_mpc=>ts_notepdf,
    lv_note_number TYPE c LENGTH 8,
    lv_version     TYPE c LENGTH 4,
    lv_language    TYPE c LENGTH 1,
    lv_html        TYPE string,
    lv_pdf_buffer  TYPE xstring.

  " 1. Hardcoded values (same as get_entity)
  lv_note_number = '3164537'.
  lv_version     = '0002'.
  lv_language    = 'E'.

  " 2. Validate
  IF lv_note_number IS INITIAL OR lv_version IS INITIAL OR lv_language IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'NoteNumber, Version, Language venum'.
  ENDIF.

  " 3. Prepare key for SCWB function
  DATA ls_note_key TYPE cwbntkeylg.
  ls_note_key-numm    = lv_note_number.
  ls_note_key-versno  = lv_version.
  ls_note_key-langu   = lv_language.

  " 4. Call SCWB function to get HTML
  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_note_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0 OR lv_html IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'Note HTML content kedaikala'.
  ENDIF.

  " 5. Convert HTML to XSTRING (PDF buffer)
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_html
    IMPORTING
      buffer = lv_pdf_buffer
    EXCEPTIONS
      OTHERS = 1.

  " 6. Create entity for entityset
  ls_entity-notenumber = lv_note_number.
  ls_entity-version    = lv_version.
  ls_entity-language   = lv_language.
  ls_entity-pdfcontent = lv_pdf_buffer.

  " 7. Add to response entityset
  APPEND ls_entity TO et_entityset.

ENDMETHOD.
ENDCLASS.
