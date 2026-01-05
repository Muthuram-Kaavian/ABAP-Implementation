class ZCL_Z_DOWNLOAD_FROM_01_DPC_EXT definition
  public
  inheriting from ZCL_Z_DOWNLOAD_FROM_01_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.

  methods READ_FILE_FROM_SERVER
    importing
      value(IV_FILEPATH) type STRING
    returning
      value(RT_CONTENT) type STRING_TABLE .
ENDCLASS.



CLASS ZCL_Z_DOWNLOAD_FROM_01_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.

  DATA: ls_response_entity  TYPE zcl_z_download_from_01_mpc=>ts_filedownload,
        lt_files            TYPE TABLE OF string,
        lt_parts            TYPE TABLE OF string,
        lv_filename         TYPE string,
        lv_file_count       TYPE i,
        lv_count            TYPE i,
        lo_zip              TYPE REF TO cl_abap_zip,
        lv_zip_data         TYPE xstring,
        lv_filepath         TYPE string,
        lv_temp_path        TYPE string,
        lv_excel_str        TYPE string,
        lv_xls_content      TYPE xstring,
        lt_excel_data       TYPE string_table,
        lt_failed_paths     TYPE TABLE OF string,
        lv_total_count      TYPE i,
        lv_failed_paths_str TYPE string.

  TYPES: BEGIN OF ty_successful_file,
           name    TYPE string,
           content TYPE xstring,
         END OF ty_successful_file.
  DATA: lt_successful_files TYPE TABLE OF ty_successful_file.

  FIELD-SYMBOLS: <fs_key_tab> TYPE /iwbep/s_mgw_name_value_pair.

  "---------------------------------------------------------------
  " 1. Read and validate the 'FilePath' key from the request URL
  "---------------------------------------------------------------
  READ TABLE it_key_tab WITH KEY name = 'FilePath' ASSIGNING <fs_key_tab>.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'FilePath key is required'.
  ENDIF.

  IF <fs_key_tab>-value IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'No file path provided.'.
  ENDIF.

  "---------------------------------------------------------------
  " 2. Decode URL and sanitize filepath
  "---------------------------------------------------------------
  lv_filepath = cl_http_utility=>unescape_url( <fs_key_tab>-value ).
  CONDENSE lv_filepath NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN lv_filepath WITH ''.

  "---------------------------------------------------------------
  " 3. Handle multiple files (comma separated)
  "---------------------------------------------------------------
  IF lv_filepath CS ','.
    SPLIT lv_filepath AT ',' INTO TABLE lt_files.
    lv_total_count = lines( lt_files ).

    "--- Step 3.1: Loop through all paths to see which succeed and which fail
    LOOP AT lt_files INTO lv_temp_path.
      CONDENSE lv_temp_path.

      lt_excel_data = me->read_file_from_server( iv_filepath = lv_temp_path ).

      IF lt_excel_data IS INITIAL.
        " File failed, so add its path to the failed list
        APPEND lv_temp_path TO lt_failed_paths.
        CONTINUE.
      ENDIF.

      " File succeeded, so convert and store its data temporarily
      CONCATENATE LINES OF lt_excel_data INTO lv_excel_str SEPARATED BY cl_abap_char_utilities=>cr_lf.
      lv_xls_content = cl_bcs_convert=>string_to_xstring( iv_string = lv_excel_str ).

      CLEAR: lt_parts, lv_filename, lv_count.
      SPLIT lv_temp_path AT '/' INTO TABLE lt_parts.
      DESCRIBE TABLE lt_parts LINES lv_count.
      READ TABLE lt_parts INDEX lv_count INTO lv_filename.

      IF lv_filename IS NOT INITIAL.
        APPEND VALUE #( name = lv_filename content = lv_xls_content ) TO lt_successful_files.
      ENDIF.
    ENDLOOP.

    "--- Step 3.2: Set counts for response
    ls_response_entity-totalcount   = lv_total_count.
    ls_response_entity-successcount = lines( lt_successful_files ).
    ls_response_entity-failedcount  = lines( lt_failed_paths ).

    "--- NEW: Concatenate failed paths into a single string
    IF lt_failed_paths IS NOT INITIAL.
      CONCATENATE LINES OF lt_failed_paths INTO lv_failed_paths_str SEPARATED BY '; '.
      ls_response_entity-failedpaths = lv_failed_paths_str.
    ENDIF.

    "--- Step 3.3: Set status message
    ls_response_entity-status = |Processed { lv_total_count } file(s): { ls_response_entity-successcount } successful, { ls_response_entity-failedcount } failed.|.

    "--- Step 3.4: Decide what to return: nothing, a single file, or a ZIP
    CASE ls_response_entity-successcount.
      WHEN 0. " No valid files found
        ls_response_entity-filename    = ''.
        ls_response_entity-filecontent = ''.

      WHEN 1. " Exactly ONE successful file, download directly
        READ TABLE lt_successful_files INDEX 1 INTO DATA(ls_single_file).
        ls_response_entity-filename    = ls_single_file-name.
        ls_response_entity-filecontent = ls_single_file-content.

      WHEN OTHERS. " TWO OR MORE successful files, create a ZIP
        CREATE OBJECT lo_zip.
        LOOP AT lt_successful_files INTO DATA(ls_success_file).
          lo_zip->add( name = ls_success_file-name content = ls_success_file-content ).
        ENDLOOP.

        lv_zip_data = lo_zip->save( ).
        ls_response_entity-filename    = |backup_files_{ sy-datum }_{ sy-uzeit }.zip|.
        ls_response_entity-filecontent = lv_zip_data.
    ENDCASE.

  ELSE.
    "---------------------------------------------------------------
    " 4. Handle a single file download
    "---------------------------------------------------------------
    lt_excel_data = me->read_file_from_server( iv_filepath = lv_filepath ).

    IF lt_excel_data IS INITIAL.
      ls_response_entity-totalcount   = 1.
      ls_response_entity-successcount = 0.
      ls_response_entity-failedcount  = 1.
      ls_response_entity-failedpaths  = lv_filepath. " Single path as string
      ls_response_entity-status       = 'File does not exist or is empty on the server'.
      ls_response_entity-filename     = ''.
      ls_response_entity-filecontent  = ''.
    ELSE.
      CONCATENATE LINES OF lt_excel_data INTO lv_excel_str SEPARATED BY cl_abap_char_utilities=>cr_lf.
      lv_xls_content = cl_bcs_convert=>string_to_xstring( iv_string = lv_excel_str ).

      CLEAR: lt_parts, lv_filename, lv_count.
      SPLIT lv_filepath AT '/' INTO TABLE lt_parts.
      DESCRIBE TABLE lt_parts LINES lv_count.
      IF lv_count > 0.
        READ TABLE lt_parts INDEX lv_count INTO lv_filename.
      ELSE.
        lv_filename = 'downloaded_file.xls'. " Fallback name
      ENDIF.

      ls_response_entity-totalcount   = 1.
      ls_response_entity-successcount = 1.
      ls_response_entity-failedcount  = 0.
      ls_response_entity-failedpaths  = ''.
      ls_response_entity-filepath = <fs_key_tab>-value.
      ls_response_entity-filename     = lv_filename.
      ls_response_entity-filecontent  = lv_xls_content.
      ls_response_entity-status       = 'File downloaded successfully'.
    ENDIF.
  ENDIF.

  "---------------------------------------------------------------
  " 5. Return the final entity
  "---------------------------------------------------------------
  copy_data_to_ref(
    EXPORTING is_data = ls_response_entity
    CHANGING  cr_data = er_entity ).

ENDMETHOD.


METHOD read_file_from_server.
  DATA: lv_text_line TYPE string.

  " Attempt to open the specified file on the application server
  OPEN DATASET iv_filepath FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    " If successful, read the file line by line
    DO.
      READ DATASET iv_filepath INTO lv_text_line.
      IF sy-subrc <> 0.
        " Exit loop when the end of the file is reached
        EXIT.
      ENDIF.
      APPEND lv_text_line TO rt_content.
    ENDDO.
    CLOSE DATASET iv_filepath.
  ENDIF.
  " The method returns the table of text lines (rt_content).
  " If the file could not be opened or was empty, the table will be initial.
ENDMETHOD.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
**  EXPORTING
**    iv_action_name          =
**    it_parameter            =
**    io_tech_request_context =
**  IMPORTING
**    er_data                 =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  endmethod.
ENDCLASS.
