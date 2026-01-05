class ZCL_ZSNOTE_PDF_ENTITY_DPC_EXT definition
  public
  inheriting from ZCL_ZSNOTE_PDF_ENTITY_DPC
  create public .

public section.
protected section.

  methods NOTEPDFSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSNOTE_PDF_ENTITY_DPC_EXT IMPLEMENTATION.


METHOD NOTEPDFSET_CREATE_ENTITY.
*----------------------------------------------------------------------
* OData method to convert SNOTE HTML to PDF
*----------------------------------------------------------------------
  DATA: ls_request  TYPE zcl_zsnote_pdf_entity_mpc=>ts_zsnote_pdf_type,
        ls_response TYPE zcl_zsnote_pdf_entity_mpc=>ts_zsnote_pdf_type,
        ls_key      TYPE cwbntkeylg,
        lv_html     TYPE string,
        lv_pdf      TYPE xstring.

* 1) Get request data
  io_data_provider->read_entry_data( IMPORTING es_data = ls_request ).

* 2) Validate
  IF ls_request-notenumber IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message = 'NoteNumber is mandatory'.
  ENDIF.

* 3) Get HTML from SNOTE
  ls_key-numm   = ls_request-notenumber.
  ls_key-versno = COND #( WHEN ls_request-version IS INITIAL THEN '0002' ELSE ls_request-version ).
  ls_key-langu  = COND #( WHEN ls_request-language IS INITIAL THEN 'E' ELSE ls_request-language ).

  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0 OR lv_html IS INITIAL.
    ls_response-message = 'HTML not available for given note'.
    er_entity = ls_response.
    RETURN.
  ENDIF.

* 4) Convert HTML to XSTRING directly (No file operations)
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_html
    IMPORTING
      buffer = lv_pdf.

* 5) Prepare response
  ls_response-notenumber = ls_request-notenumber.
  ls_response-version    = ls_key-versno.
  ls_response-language   = ls_key-langu.
  ls_response-pdfcontent = lv_pdf.
  ls_response-message    = 'HTML content converted to PDF format'.

* 6) Return response
  er_entity = ls_response.

ENDMETHOD.
ENDCLASS.
