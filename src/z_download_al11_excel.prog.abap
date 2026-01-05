
REPORT z_download_multiple_files_as_zip.

PARAMETERS: p_file1 TYPE rlgrap-filename,
            p_file2 TYPE rlgrap-filename,
            p_file3 TYPE rlgrap-filename,
            p_file4 TYPE rlgrap-filename,
            p_file5 TYPE rlgrap-filename.

DATA: lt_files      TYPE TABLE OF string,
      lv_zip_data   TYPE xstring,
      lt_zip_data   TYPE TABLE OF solix,
      lv_filename   TYPE string,
      lv_path       TYPE string,
      lv_fullpath   TYPE string,
      lo_zip        TYPE REF TO cl_abap_zip,
      lv_file_data  TYPE xstring,
      lv_file_name  TYPE string,
      lv_index      TYPE i,
      lv_file_count TYPE i.

START-OF-SELECTION.

  " Collect all file paths that are not empty
  IF p_file1 IS NOT INITIAL.
    APPEND p_file1 TO lt_files.
  ENDIF.
  IF p_file2 IS NOT INITIAL.
    APPEND p_file2 TO lt_files.
  ENDIF.
  IF p_file3 IS NOT INITIAL.
    APPEND p_file3 TO lt_files.
  ENDIF.
  IF p_file4 IS NOT INITIAL.
    APPEND p_file4 TO lt_files.
  ENDIF.
  IF p_file5 IS NOT INITIAL.
    APPEND p_file5 TO lt_files.
  ENDIF.

  IF lt_files IS INITIAL.
    MESSAGE 'No files specified for download!' TYPE 'E'.
    RETURN.
  ENDIF.

  " Create ZIP object
  CREATE OBJECT lo_zip.

  " Read each file and add to ZIP
  LOOP AT lt_files INTO lv_file_name.
    " Check if file exists
    OPEN DATASET lv_file_name FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE |File { lv_file_name } does not exist on the server!| TYPE 'W'.
      CONTINUE.
    ENDIF.

    " Read file content
    READ DATASET lv_file_name INTO lv_file_data.
    CLOSE DATASET lv_file_name.

    IF sy-subrc <> 0 OR lv_file_data IS INITIAL.
      MESSAGE |Error reading file { lv_file_name } or file is empty!| TYPE 'W'.
      CONTINUE.
    ENDIF.

    " Extract just the filename from the full path for ZIP entry
    lv_index = find( val = lv_file_name sub = '/' occ = -1 ).
    IF lv_index > 0.
      lv_filename = lv_file_name+lv_index.
    ELSE.
      lv_filename = lv_file_name.
    ENDIF.

    " Add file to ZIP
    lo_zip->add( name    = lv_filename
                 content = lv_file_data ).

    " Count successful file additions
    lv_file_count = lv_file_count + 1.
  ENDLOOP.

  " Check if any files were successfully added to ZIP
  IF lv_file_count = 0.
    MESSAGE 'No valid files to download!' TYPE 'E'.
    RETURN.
  ENDIF.

  " Get the ZIP content as xstring
  lv_zip_data = lo_zip->save( ).

  " Convert xstring to binary table
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_zip_data
    TABLES
      binary_tab = lt_zip_data.

  " Open dialog to let user choose where to save the ZIP file
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension = 'ZIP'
      default_file_name = 'downloaded_files'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath.

  " Check if user cancelled the dialog
  IF lv_fullpath IS INITIAL.
    MESSAGE 'Download cancelled by user' TYPE 'S'.
    RETURN.
  ENDIF.

  " Download the ZIP file to local PC
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename = lv_fullpath
      filetype = 'BIN'
    CHANGING
      data_tab = lt_zip_data.

  IF sy-subrc = 0.
    MESSAGE |{ lv_file_count } files downloaded successfully as ZIP!| TYPE 'S'.
  ELSE.
    MESSAGE 'Error downloading ZIP file!' TYPE 'E'.
  ENDIF.
