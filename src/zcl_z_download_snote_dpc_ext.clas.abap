class ZCL_Z_DOWNLOAD_SNOTE_DPC_EXT definition
  public
  inheriting from ZCL_Z_DOWNLOAD_SNOTE_DPC
  create public .

public section.
protected section.

  methods SAPNOTESTATUSSET_GET_ENTITY
    redefinition .
  methods SAPNOTESTATUSSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_DOWNLOAD_SNOTE_DPC_EXT IMPLEMENTATION.


METHOD SAPNOTESTATUSSET_GET_ENTITY.

  "----------------------------------------------------------------------"
  " 1. DECLARATIONS
  "----------------------------------------------------------------------"
  DATA: ls_note_data          TYPE bcwbn_note,
        lv_note_number        TYPE cwbntkey-numm,
        lv_initial_version    TYPE cwbntkeyvs-versno,
        lv_impl_status        TYPE cwbprstat,
        lv_impl_status_text   TYPE string,
        ls_entity             TYPE zcl_z_download_snote_mpc=>ts_SAPNOTESTATUS.

  TRY.
      "----------------------------------------------------------------------"
      " 2. GET KEY
      "----------------------------------------------------------------------"
      lv_note_number = it_key_tab[ name = 'NoteNumber' ]-value.

      "----------------------------------------------------------------------"
      " 3. READ LOCAL NOTE (BEFORE DOWNLOAD)
      "----------------------------------------------------------------------"
      CALL FUNCTION 'SCWB_NOTE_READ'
        EXPORTING
          iv_read_attributes = abap_true
        CHANGING
          cs_note            = ls_note_data
        EXCEPTIONS
          OTHERS             = 1.

      " Store the version we found locally, if any
      lv_initial_version = ls_note_data-key-versno.

      "----------------------------------------------------------------------"
      " 4. ALWAYS ATTEMPT TO DOWNLOAD THE LATEST VERSION
      "----------------------------------------------------------------------"
      CALL FUNCTION 'SCWN_NOTES_DOWNLOAD'
        EXPORTING
          i_numm      = lv_note_number
        EXCEPTIONS
          rfc_error   = 1
          not_found   = 2
          OTHERS      = 3.

      IF sy-subrc <> 0.
        " If download fails, check if we have an initial version to proceed with
        IF lv_initial_version IS INITIAL.
           ls_entity-updatemessage = 'Error: Note not found locally and download failed.'.
           er_entity = ls_entity.
           RETURN.
        ENDIF.
      ENDIF.

      "----------------------------------------------------------------------"
      " 5. RE-READ NOTE DATA & COMPARE VERSIONS
      "----------------------------------------------------------------------"
      CLEAR ls_note_data.
      CALL FUNCTION 'SCWB_NOTE_READ'
        EXPORTING
          iv_read_attributes = abap_true
          iv_read_short_text = abap_true
        CHANGING
          cs_note            = ls_note_data
        EXCEPTIONS
          OTHERS             = 1.

      IF ls_note_data-key-versno > lv_initial_version.
        ls_entity-updatemessage = 'New version has been downloaded successfully.'.
      ELSEIF lv_initial_version IS NOT INITIAL.
        ls_entity-updatemessage = 'The latest version was already present in the system.'.
      ELSE.
        ls_entity-updatemessage = 'Note downloaded for the first time.'.
      ENDIF.

      "----------------------------------------------------------------------"
      " 6. GET IMPLEMENTATION STATUS
      "----------------------------------------------------------------------"
      CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
        EXPORTING
          is_note     = ls_note_data
        IMPORTING
          ev_status   = lv_impl_status
        EXCEPTIONS
          OTHERS      = 5.

      CASE lv_impl_status.
        WHEN 'A'. lv_impl_status_text = 'Completely Implemented'.
        WHEN 'B'. lv_impl_status_text = 'Partially Implemented'.
        WHEN 'C'. lv_impl_status_text = 'Not Relevant'.
        WHEN 'D'. lv_impl_status_text = 'Not Implemented'.
        WHEN 'E'. lv_impl_status_text = 'Obsolete'.
        WHEN OTHERS. lv_impl_status_text = 'Inconsistent or Unknown'.
      ENDCASE.

      "----------------------------------------------------------------------"
      " 7. PREPARE RESPONSE
      "----------------------------------------------------------------------"
      ls_entity-notenumber           = ls_note_data-key-numm.
      ls_entity-version              = ls_note_data-key-versno.
      ls_entity-shorttext            = ls_note_data-stext.
      ls_entity-implementationstatus = lv_impl_status_text.

      er_entity = ls_entity.

    CATCH cx_root INTO DATA(lx_root).

  ENDTRY.

ENDMETHOD.


METHOD SAPNOTESTATUSSET_GET_ENTITYSET.

  DATA: lt_note_numbers TYPE TABLE OF cwbntkey-numm,
        ls_note_data    TYPE bcwbn_note,
        ls_entity       TYPE zcl_z_download_snote_mpc=>ts_sapnotestatus,
        lv_note_number  TYPE cwbntkey-numm.

  " Handle filter by NoteNumber
  IF it_filter_select_options IS NOT INITIAL.
    LOOP AT it_filter_select_options INTO DATA(ls_filter).
      IF ls_filter-property = 'NOTENUMBER'.
        READ TABLE ls_filter-select_options INDEX 1 INTO DATA(ls_option).
        IF sy-subrc = 0.
          lv_note_number = ls_option-low.
          APPEND lv_note_number TO lt_note_numbers.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    " Get some sample notes or recent notes
    " For demo purposes, let's add some common SAP notes
    APPEND '3464863' TO lt_note_numbers.  " Your test note
    APPEND '2577840' TO lt_note_numbers.  " Common SAP note
    APPEND '2899822' TO lt_note_numbers.  " Another common note
  ENDIF.

  " Process each note number
  LOOP AT lt_note_numbers INTO lv_note_number.
    CLEAR: ls_note_data, ls_entity.

    " Try to read the note
    ls_note_data-key-numm = lv_note_number.

    CALL FUNCTION 'SCWB_NOTE_READ'
      EXPORTING
        iv_read_attributes = abap_true
        iv_read_short_text = abap_true
      CHANGING
        cs_note            = ls_note_data
      EXCEPTIONS
        OTHERS             = 1.

    IF sy-subrc = 0.
      " Note found - populate entity
      ls_entity-notenumber    = ls_note_data-key-numm.
      ls_entity-version       = ls_note_data-key-versno.
      ls_entity-shorttext     = ls_note_data-stext.
      ls_entity-updatemessage = 'Note available in system'.
    ELSE.
      " Note not found
      ls_entity-notenumber    = lv_note_number.
      ls_entity-version       = '0000'.
      ls_entity-shorttext     = 'Note not found locally'.
      ls_entity-updatemessage = 'Use single read to download'.
    ENDIF.

    ls_entity-implementationstatus = 'Check via single read'.
    APPEND ls_entity TO et_entityset.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
