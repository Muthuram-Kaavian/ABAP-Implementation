class ZCL_ZNOTE_PDF_01_DPC_EXT definition
  public
  inheriting from ZCL_ZNOTE_PDF_01_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZNOTE_PDF_01_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_function_import.

  "---- Declarations
  DATA lv_html     TYPE string.
  DATA lv_pdf      TYPE xstring.
  DATA lv_html_x   TYPE xstring.
  DATA ls_key      TYPE cwbntkeylg.
  DATA ls_out      TYPE zcl_znote_pdf_mpc=>ts_notepdf.

  DATA lo_http     TYPE REF TO if_http_client.
  DATA lo_req      TYPE REF TO if_http_request.
  DATA lo_res      TYPE REF TO if_http_response.

  DATA lv_status   TYPE i.
  DATA lv_reason   TYPE string.

  DATA ls_param    TYPE /iwbep/s_mgw_name_value_pair.

  DATA lv_numm     TYPE string.
  DATA lv_versno   TYPE string.
  DATA lv_langu    TYPE string.

  DATA lv_body     TYPE string.
  DATA lv_work     TYPE string.
  DATA lv_crlf     TYPE string.
  DATA lv_pdf_ok   TYPE c LENGTH 1.

  FIELD-SYMBOLS <comp> TYPE any.

  "---- Only handle our function import name
  IF iv_operation_name <> 'NoteToPdf'.
    RETURN.
  ENDIF.

  "---- Read parameters from it_parameter (Function Import passes here)
  CLEAR ls_param.
  READ TABLE it_parameter INTO ls_param WITH KEY name = 'Numm'.
  IF sy-subrc = 0. lv_numm   = ls_param-value. ENDIF.

  CLEAR ls_param.
  READ TABLE it_parameter INTO ls_param WITH KEY name = 'Versno'.
  IF sy-subrc = 0. lv_versno = ls_param-value. ENDIF.

  CLEAR ls_param.
  READ TABLE it_parameter INTO ls_param WITH KEY name = 'Langu'.
  IF sy-subrc = 0. lv_langu  = ls_param-value. ENDIF.

  "---- Read HTML
  CLEAR ls_key.
  ls_key-numm   = lv_numm.
  ls_key-versno = lv_versno.
  ls_key-langu  = lv_langu.

  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  "---- Try convert HTML -> PDF (SM59 dest Z_HTML2PDF)
  CLEAR lv_pdf.
  lv_pdf_ok = space.

  IF lv_html IS NOT INITIAL.
    lv_work = lv_html.
    REPLACE ALL OCCURRENCES OF '\' IN lv_work WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN lv_work WITH '\"'.
    lv_crlf = cl_abap_char_utilities=>cr_lf.
    REPLACE ALL OCCURRENCES OF lv_crlf IN lv_work WITH '\n'.
    CONCATENATE '{ "html": "' lv_work '" }' INTO lv_body.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING destination = 'Z_HTML2PDF'
      IMPORTING client      = lo_http.

    IF lo_http IS BOUND.
      lo_req = lo_http->request.
      lo_res = lo_http->response.

      CALL METHOD lo_req->set_method
        EXPORTING method = if_http_request=>co_request_method_post.

      CALL METHOD lo_req->set_content_type
        EXPORTING content_type = 'application/json; charset=utf-8'.

      CALL METHOD lo_req->set_cdata
        EXPORTING data = lv_body.

      CALL METHOD lo_http->send.
      CALL METHOD lo_http->receive.

      CALL METHOD lo_res->get_status
        IMPORTING code = lv_status reason = lv_reason.

      IF lv_status = 200.
        CALL METHOD lo_res->get_data RECEIVING data = lv_pdf.
        IF lv_pdf IS NOT INITIAL.
          lv_pdf_ok = 'X'.
        ENDIF.
      ENDIF.

      CALL METHOD lo_http->close.
    ENDIF.
  ENDIF.

  "---- Fallback: if no PDF, return HTML as UTF-8 bytes (no error)
  IF lv_pdf_ok IS INITIAL AND lv_html IS NOT INITIAL.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING text = lv_html encoding = 'UTF-8'
      IMPORTING buffer = lv_html_x
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc = 0 AND lv_html_x IS NOT INITIAL.
      lv_pdf = lv_html_x.
    ENDIF.
  ENDIF.

  "---- Fill output entity (NUMM, VERSNO, LANGU, PDF)
  ASSIGN COMPONENT 'NUMM'   OF STRUCTURE ls_out TO <comp>. IF sy-subrc = 0. <comp> = lv_numm.   ENDIF.
  ASSIGN COMPONENT 'VERSNO' OF STRUCTURE ls_out TO <comp>. IF sy-subrc = 0. <comp> = lv_versno. ENDIF.
  ASSIGN COMPONENT 'LANGU'  OF STRUCTURE ls_out TO <comp>. IF sy-subrc = 0. <comp> = lv_langu.  ENDIF.
  ASSIGN COMPONENT 'PDF'    OF STRUCTURE ls_out TO <comp>. IF sy-subrc = 0. <comp> = lv_pdf.    ENDIF.

  copy_data_to_ref(
    EXPORTING is_data = ls_out
    CHANGING  cr_data = er_data ).

ENDMETHOD.
ENDCLASS.
