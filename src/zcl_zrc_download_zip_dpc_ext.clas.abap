class ZCL_ZRC_DOWNLOAD_ZIP_DPC_EXT definition
  public
  inheriting from ZCL_ZRC_DOWNLOAD_ZIP_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZRC_DOWNLOAD_ZIP_DPC_EXT IMPLEMENTATION.


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


METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    DATA: ls_stream TYPE ty_s_media_resource,
          lt_data   TYPE TABLE OF srtm_datax,
          ls_data   TYPE srtm_datax,
          lv_hex2   TYPE x LENGTH 2.

    " -----------------------------------------------------------------
    " STRATEGY: SEARCH BY FILE SIGNATURE (MAGIC NUMBER)
    " We fetch the recent records and look for the specific ZIP file signature.
    " ZIP files always start with Hex '504B' (ASCII 'PK').
    " This guarantees we ignore the XML files and get the ZIP.
    " -----------------------------------------------------------------

    " 1. Fetch recent 50 records (The job usually creates 10-20 records)
    SELECT *
      FROM srtm_datax
      INTO TABLE lt_data
      UP TO 50 ROWS
      ORDER BY ddate DESCENDING dtime DESCENDING.

    " 2. Loop to find the entry that is actually a ZIP file
    LOOP AT lt_data INTO ls_data.

      " Safety check: Data must be at least 4 bytes to be a file
      IF xstrlen( ls_data-xtext ) < 4.
        CONTINUE.
      ENDIF.

      " Check first 2 bytes for ZIP signature 'PK' (Hex 504B)
      lv_hex2 = ls_data-xtext(2).

      IF lv_hex2 = '504B'.
        " Found it! This is the ZIP file.
        ls_stream-value = ls_data-xtext.
        EXIT. " Stop searching, we have the file.
      ENDIF.

    ENDLOOP.

    " -----------------------------------------------------------------
    " 3. Error Handling
    " -----------------------------------------------------------------
    IF ls_stream-value IS INITIAL.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
         EXPORTING
           textid  = /iwbep/cx_mgw_busi_exception=>business_error
           message = 'No ZIP file found. Please ensure the Job Z_READINESS_CHECK has finished successfully.'.
    ENDIF.

    " -----------------------------------------------------------------
    " 4. Prepare Download
    " -----------------------------------------------------------------
    ls_stream-mime_type = 'application/zip'.

    DATA: lv_filename TYPE string.
    CONCATENATE 'RC_Analysis_' ls_data-ddate '_' ls_data-dtime '.zip' INTO lv_filename.

    DATA: ls_lheader TYPE ihttpnvp.
    ls_lheader-name  = 'Content-Disposition'.
    ls_lheader-value = |attachment; filename="{ lv_filename }"|.
    set_header( ls_lheader ).

    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).

  ENDMETHOD.
ENDCLASS.
