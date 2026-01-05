class ZCL_Z_DOWNLOAD_RC_ZIP_DPC_EXT definition
  public
  inheriting from ZCL_Z_DOWNLOAD_RC_ZIP_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods ANALYSISFILESET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_DOWNLOAD_RC_ZIP_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    DATA: ls_stream TYPE ty_s_media_resource,
          lt_data   TYPE TABLE OF srtm_datax,
          ls_data   TYPE srtm_datax.

    " -----------------------------------------------------------------
    " 1. FETCH RECENT DATA
    " -----------------------------------------------------------------
    SELECT *
      FROM srtm_datax
      INTO TABLE lt_data
      UP TO 50 ROWS
      ORDER BY ddate DESCENDING dtime DESCENDING.

    " -----------------------------------------------------------------
    " 2. FIND THE LARGEST ZIP FILE (The Master ZIP)
    " We loop through ALL recent records and keep only the biggest ZIP.
    " This filters out small partial results.
    " -----------------------------------------------------------------
    DATA: lv_final_zip  TYPE xstring,
          lv_current_zip TYPE xstring,
          lv_hex2       TYPE x LENGTH 2,
          lv_max_size   TYPE i VALUE 0,
          lv_curr_size  TYPE i.

    " Structure to unwrap XML if found
    TYPES: BEGIN OF ty_xml_item,
             TARGET_STACK TYPE string,
             RESULT_XSTR  TYPE xstring,
           END OF ty_xml_item.
    DATA: lt_per_data TYPE STANDARD TABLE OF ty_xml_item,
          ls_item     LIKE LINE OF lt_per_data.

    LOOP AT lt_data INTO ls_data.
      CLEAR lv_current_zip.

      IF xstrlen( ls_data-xtext ) < 4. CONTINUE. ENDIF.
      lv_hex2 = ls_data-xtext(2).

      " CHECK A: Is it a Raw ZIP file? (Starts with 'PK' / Hex 504B)
      IF lv_hex2 = '504B'.
         lv_current_zip = ls_data-xtext.
      ENDIF.

      " CHECK B: Is it wrapped in XML? (Starts with '<?' / Hex 3C3F)
      IF lv_hex2 = '3C3F'.
         TRY.
             CALL TRANSFORMATION id
               SOURCE XML ls_data-xtext
               RESULT PER_DATA = lt_per_data.

             READ TABLE lt_per_data INTO ls_item INDEX 1.

             " Check if the unwrapped content is a ZIP ('PK')
             IF sy-subrc = 0 AND xstrlen( ls_item-result_xstr ) > 4.
                lv_hex2 = ls_item-result_xstr(2).
                IF lv_hex2 = '504B'.
                   lv_current_zip = ls_item-result_xstr.
                ENDIF.
             ENDIF.
         CATCH cx_root.
             " Ignore XML errors
         ENDTRY.
      ENDIF.

      " COMPARE SIZE: If this ZIP is bigger than the previous one, keep it.
      IF lv_current_zip IS NOT INITIAL.
         lv_curr_size = xstrlen( lv_current_zip ).

         IF lv_curr_size > lv_max_size.
            lv_max_size  = lv_curr_size.
            lv_final_zip = lv_current_zip.
            " Use the date of the largest file for the filename
            ls_data-ddate = ls_data-ddate.
            ls_data-dtime = ls_data-dtime.
         ENDIF.
      ENDIF.

    ENDLOOP.

    " -----------------------------------------------------------------
    " 3. ERROR HANDLING
    " -----------------------------------------------------------------
    IF lv_final_zip IS INITIAL.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
         EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                   message = 'No Readiness ZIP file found. Please run the Z_READINESS_CHECK job first.'.
    ENDIF.

    " -----------------------------------------------------------------
    " 4. SERVE THE ZIP FILE
    " -----------------------------------------------------------------
    ls_stream-value     = lv_final_zip.
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


METHOD analysisfileset_get_entity.

    " -----------------------------------------------------------------
    " 1. VARIABLES
    " -----------------------------------------------------------------
    DATA: lt_data        TYPE TABLE OF srtm_datax,
          ls_data        TYPE srtm_datax,
          lv_final_zip   TYPE xstring,
          lv_current_zip TYPE xstring,
          lv_hex2        TYPE x LENGTH 2,
          lv_max_size    TYPE i VALUE 0,
          lv_curr_size   TYPE i,
          lv_best_date   TYPE datum,
          lv_best_time   TYPE uzeit.

    " Structure to unwrap XML if found
    TYPES: BEGIN OF ty_xml_item,
             TARGET_STACK TYPE string,
             RESULT_XSTR  TYPE xstring,
           END OF ty_xml_item.
    DATA: lt_per_data TYPE STANDARD TABLE OF ty_xml_item,
          ls_item     LIKE LINE OF lt_per_data.

    " -----------------------------------------------------------------
    " 2. FETCH RECENT DATA (Vacuum Strategy)
    " -----------------------------------------------------------------
    SELECT *
      FROM srtm_datax
      INTO TABLE lt_data
      UP TO 50 ROWS
      ORDER BY ddate DESCENDING dtime DESCENDING.

    " -----------------------------------------------------------------
    " 3. FIND THE LARGEST ZIP FILE
    " -----------------------------------------------------------------
    LOOP AT lt_data INTO ls_data.
      CLEAR lv_current_zip.

      IF xstrlen( ls_data-xtext ) < 4. CONTINUE. ENDIF.
      lv_hex2 = ls_data-xtext(2).

      " CHECK A: Raw ZIP ('PK')
      IF lv_hex2 = '504B'.
         lv_current_zip = ls_data-xtext.
      ENDIF.

      " CHECK B: XML Wrapper ('<?')
      IF lv_hex2 = '3C3F'.
         TRY.
             CALL TRANSFORMATION id
               SOURCE XML ls_data-xtext
               RESULT PER_DATA = lt_per_data.

             READ TABLE lt_per_data INTO ls_item INDEX 1.
             IF sy-subrc = 0 AND xstrlen( ls_item-result_xstr ) > 4.
                lv_hex2 = ls_item-result_xstr(2).
                IF lv_hex2 = '504B'.
                   lv_current_zip = ls_item-result_xstr.
                ENDIF.
             ENDIF.
         CATCH cx_root.
         ENDTRY.
      ENDIF.

      " KEEP LARGEST
      IF lv_current_zip IS NOT INITIAL.
         lv_curr_size = xstrlen( lv_current_zip ).
         IF lv_curr_size > lv_max_size.
            lv_max_size  = lv_curr_size.
            lv_final_zip = lv_current_zip.
            lv_best_date = ls_data-ddate.
            lv_best_time = ls_data-dtime.
         ENDIF.
      ENDIF.
    ENDLOOP.

    " -----------------------------------------------------------------
    " 4. MAP TO ENTITY PROPERTIES
    " -----------------------------------------------------------------
    IF lv_final_zip IS INITIAL.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
         EXPORTING textid = /iwbep/cx_mgw_busi_exception=>business_error
                   message = 'No Readiness ZIP file found.'.
    ENDIF.

    " FIX: Use correct type /IWBEP/S_MGW_TECH_PAIR to match get_keys()
    DATA: ls_key_tab TYPE /iwbep/s_mgw_tech_pair.

    READ TABLE io_tech_request_context->get_keys( ) INTO ls_key_tab WITH KEY name = 'Filename'.

    IF sy-subrc = 0.
      er_entity-filename = ls_key_tab-value.
    ELSE.
      " Check case sensitivity backup
      READ TABLE io_tech_request_context->get_keys( ) INTO ls_key_tab WITH KEY name = 'FILENAME'.
      IF sy-subrc = 0.
         er_entity-filename = ls_key_tab-value.
      ELSE.
         " Fallback filename if key is missing/Latest
         CONCATENATE 'RC_Analysis_' lv_best_date '_' lv_best_time '.zip' INTO er_entity-filename.
      ENDIF.
    ENDIF.

    " Set the Content
    " NOTE: ABAP Type XSTRING automatically converts to Base64 in OData Edm.Binary
    er_entity-filecontent = lv_final_zip.

  ENDMETHOD.
ENDCLASS.
