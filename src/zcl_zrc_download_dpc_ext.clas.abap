class ZCL_ZRC_DOWNLOAD_DPC_EXT definition
  public
  inheriting from ZCL_ZRC_DOWNLOAD_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZRC_DOWNLOAD_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

  DATA: ls_request   TYPE zcl_zrc_download_mpc=>ts_zrc_request,   "INPUT: CLIENTID,TARGETVERSION
        ls_response  TYPE zcl_zrc_download_mpc=>ts_zrc_response,  "OUTPUT: ZIPCONTENT,FILENAME,...
        lv_error_msg TYPE bapi_msg.

  DATA(lo_msg_cont) = mo_context->get_message_container( ).

  DATA: lv_client     TYPE symandt,
        lv_client_str TYPE string,
        lv_filename   TYPE string,
        lv_fullpath   TYPE string,
        lv_zip        TYPE xstring,
        lv_size       TYPE i.

  " buffer for reading file
  DATA: lv_chunk TYPE xstring,
        lv_raw   TYPE x255.

  CONSTANTS lc_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.

  " TODO: *** ADAPT THIS DIRECTORY TO YOUR SYSTEM ***
  CONSTANTS gc_dir TYPE string VALUE '/usr/sap/S4H/SYS'.   "example folder on app server

  CASE iv_entity_name.

    WHEN 'ZRC_RESPONSE'.

      "----------------------------------------------------
      " 1. Read input (CLIENTID,TARGETVERSION)
      "----------------------------------------------------
      io_data_provider->read_entry_data(
        IMPORTING
          es_data = ls_request ).

      "---------------- client validation -----------------
      IF ls_request-clientid IS INITIAL.
        lv_error_msg = 'Client ID is required (e.g., 100, 200)'.
        lo_msg_cont->add_message_text_only(
          EXPORTING iv_msg_type = 'E'
                    iv_msg_text = lv_error_msg ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_msg_cont.
      ENDIF.

      lv_client_str = ls_request-clientid.
      CONDENSE lv_client_str NO-GAPS.

      IF lv_client_str IS INITIAL OR lv_client_str CN '0123456789'.
        lv_error_msg = |Invalid Client ID: { ls_request-clientid }. Use numeric like 100, 200|.
        lo_msg_cont->add_message_text_only(
          EXPORTING iv_msg_type = 'E'
                    iv_msg_text = lv_error_msg ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_msg_cont.
      ENDIF.

      lv_client = lv_client_str.

      "---------------- target version validation ---------
      IF ls_request-targetversion IS INITIAL.
        lv_error_msg = 'Target version is required.'.
        lo_msg_cont->add_message_text_only(
          EXPORTING iv_msg_type = 'E'
                    iv_msg_text = lv_error_msg ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_msg_cont.
      ENDIF.

      "----------------------------------------------------
      " 2. Determine file name of ORIGINAL RC ZIP
      "    *** ADAPT PATTERN TO YOUR SYSTEM ***
      "----------------------------------------------------
      "Example pattern similar to SAP: S4RCAnalysisData<SID>_<CLIENT>_<DATE>.zip
      lv_filename = |S4RCAnalysisData{ sy-sysid }_{ lv_client }_{ sy-datum }.zip|.

      "Full path on app server
      lv_fullpath = gc_dir && '/' && lv_filename.

      "----------------------------------------------------
      " 3. Read ZIP from application server (binary)
      "----------------------------------------------------
      OPEN DATASET lv_fullpath FOR INPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        lv_error_msg = |Original Readiness Check ZIP not found: { lv_fullpath }.|.
        lo_msg_cont->add_message_text_only(
          EXPORTING iv_msg_type = 'E'
                    iv_msg_text = lv_error_msg ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_msg_cont.
      ENDIF.

      CLEAR lv_zip.

      WHILE sy-subrc = 0.
        READ DATASET lv_fullpath INTO lv_raw.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        lv_chunk = lv_raw.
        CONCATENATE lv_zip lv_chunk INTO lv_zip IN BYTE MODE.
      ENDWHILE.

      CLOSE DATASET lv_fullpath.

      IF lv_zip IS INITIAL.
        lv_error_msg = |ZIP file { lv_fullpath } is empty.|..
        lo_msg_cont->add_message_text_only(
          EXPORTING iv_msg_type = 'E'
                    iv_msg_text = lv_error_msg ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_msg_cont.
      ENDIF.

      lv_size = xstrlen( lv_zip ).

      "----------------------------------------------------
      " 4. Fill response structure with ORIGINAL ZIP
      "----------------------------------------------------
      CLEAR ls_response.

      ls_response-zipcontent = lv_zip.
      ls_response-filesize   = lv_size.
      ls_response-filename   = lv_filename.
      ls_response-status     = 'S'.
      ls_response-message    =
        |Original Readiness Check ZIP read from { lv_fullpath }.|.

      "----------------------------------------------------
      " 5. Return entity
      "----------------------------------------------------
      copy_data_to_ref(
        EXPORTING is_data = ls_response
        CHANGING  cr_data = er_entity ).

    WHEN 'ZRC_REQUEST'.

      lv_error_msg = 'Please POST to ZRC_RESPONSESet, not ZRC_REQUESTSet.'.
      lo_msg_cont->add_message_text_only(
        EXPORTING iv_msg_type = 'E'
                  iv_msg_text = lv_error_msg ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message_container = lo_msg_cont.

  ENDCASE.

ENDMETHOD.
ENDCLASS.
