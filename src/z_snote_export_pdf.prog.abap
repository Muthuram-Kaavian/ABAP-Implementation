REPORT z_snote_export_pdf_noinput2.

*--- Hardcoded values
CONSTANTS: c_numm  TYPE c LENGTH 8 VALUE '3164537',
           c_vers  TYPE c LENGTH 4 VALUE '0002',
           c_langu TYPE c LENGTH 1 VALUE 'E',
           c_path  TYPE string     VALUE 'C:\Temp\'.

*--- FM requires this exact DDIC type:
DATA: ls_key      TYPE cwbntkeylg,     " expects NUMM / VERSNO / LANGU (in your system)
      lv_html     TYPE string,
      lt_lines    TYPE STANDARD TABLE OF string,
      lv_htmlfile TYPE string,
      lv_pdffile  TYPE string,
      lv_err      TYPE string.

* helper source with the right field names to MOVE-CORRESPONDING
TYPES: BEGIN OF ty_key_src,
         numm   TYPE c LENGTH 8,
         versno TYPE c LENGTH 4,
         langu  TYPE c LENGTH 1,
       END OF ty_key_src.
DATA: ls_src TYPE ty_key_src.

* OLE for Word
DATA: go_word TYPE ole2_object,
      go_docs TYPE ole2_object,
      go_doc  TYPE ole2_object.

START-OF-SELECTION.

* 1) Fill helper and move into the DDIC key
  ls_src-numm   = c_numm.
  ls_src-versno = c_vers.
  ls_src-langu  = c_langu.
  MOVE-CORRESPONDING ls_src TO ls_key.

* filenames
  CONCATENATE c_path 'note_' ls_src-numm '_' ls_src-versno '.html' INTO lv_htmlfile.
  CONCATENATE c_path 'note_' ls_src-numm '_' ls_src-versno '.pdf'  INTO lv_pdffile.

* 2) Read HTML – ONLY this FM
  CALL FUNCTION 'SCWB_NOTE_READ_HTML_TEXT'
    EXPORTING
      is_note_key_lg = ls_key
    IMPORTING
      html_text      = lv_html
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0 OR lv_html IS INITIAL.
    MESSAGE 'HTML not available. In SNOTE, download the note (HTML) and retry.' TYPE 'E'.
  ENDIF.

* 3) Save HTML (UTF-8)
  SPLIT lv_html AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
  IF lt_lines IS INITIAL.
    APPEND lv_html TO lt_lines.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename = lv_htmlfile
      filetype = 'ASC'
      codepage = '4110'    " UTF-8 (if not supported on your GUI, omit CODEPAGE)
    CHANGING
      data_tab = lt_lines.

  IF sy-subrc <> 0.
    MESSAGE 'Failed to save HTML file.' TYPE 'E'.
  ENDIF.

* 4) Convert HTML -> PDF via Word OLE (Windows client)
  PERFORM convert_with_word USING lv_htmlfile lv_pdffile CHANGING lv_err.
  IF lv_err IS NOT INITIAL.
    MESSAGE lv_err TYPE 'E'.
  ENDIF.

* 5) Open the PDF
  PERFORM open_file USING lv_pdffile.

*----------------------------------------------------------------------
* FORMS
*----------------------------------------------------------------------
FORM convert_with_word USING    iv_htmlfile TYPE string
                                iv_pdffile  TYPE string
                        CHANGING cv_msg     TYPE string.

  DATA: lv_fmt TYPE i.
  lv_fmt = 17.  " wdFormatPDF

  TRY.
      CREATE OBJECT go_word 'Word.Application'.
      SET PROPERTY OF go_word 'Visible' = 0.

      CALL METHOD OF go_word 'Documents' = go_docs.
      CALL METHOD OF go_docs 'Open' = go_doc
        EXPORTING
          #1 = iv_htmlfile.

      CALL METHOD OF go_doc 'SaveAs2'
        EXPORTING
          #1 = iv_pdffile
          #2 = lv_fmt.

      CALL METHOD OF go_doc 'Close'.
      CALL METHOD OF go_word 'Quit'.

    CATCH cx_root INTO DATA(lx).
      cv_msg = lx->get_text( ).
  ENDTRY.
ENDFORM.

FORM open_file USING iv_file TYPE string.
  CALL METHOD cl_gui_frontend_services=>execute
    EXPORTING
      document  = iv_file
      operation = 'OPEN'.
ENDFORM.
