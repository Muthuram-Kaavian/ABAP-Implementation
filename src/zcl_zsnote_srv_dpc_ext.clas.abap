class ZCL_ZSNOTE_SRV_DPC_EXT definition
  public
  inheriting from ZCL_ZSNOTE_SRV_DPC
  create public .

public section.
protected section.

  methods NOTESTATUSSET_GET_ENTITYSET
    redefinition .
  methods NOTESTATUSSET_GET_ENTITY
    redefinition .
private section.

  methods GET_IMPLEMENTATION_STATUS
    importing
      !IS_NOTE type BCWBN_NOTE
    exporting
      !EV_STATUS type CWBPRSTAT
      !EV_STATUS_TXT type STRING
    exceptions
      STATUS_ERROR .
  methods NOTE_DOWNLOAD
    importing
      !IV_NOTE_NUMBER type CWBNTKEY-NUMM
    exporting
      !EV_DOWNLOADED type ABAP_BOOL
      !EV_ALREADY_LATEST type ABAP_BOOL
      !EV_LOCAL_VERSION type CHAR4
      !EV_REMOTE_VERSION type CHAR4
      !ES_NOTE type BCWBN_NOTE
    exceptions
      DOWNLOAD_FAILED
      VERSION_CHECK_FAILED .
ENDCLASS.



CLASS ZCL_ZSNOTE_SRV_DPC_EXT IMPLEMENTATION.


METHOD get_implementation_status.
*---------------------------------------------------------------------*
* Get implementation status of the note
*---------------------------------------------------------------------*
  DATA: lv_status TYPE cwbprstat.

  " Include constants for status values
  CONSTANTS: gc_prstat_not_implemented TYPE cwbprstat VALUE 'N',
             gc_prstat_obsolete        TYPE cwbprstat VALUE 'O',
             gc_prstat_implemented     TYPE cwbprstat VALUE 'I',
             gc_prstat_can_implement   TYPE cwbprstat VALUE 'C',
             gc_prstat_partial         TYPE cwbprstat VALUE 'P'.

  " Read note with correction instructions
  DATA: ls_note TYPE bcwbn_note.
  ls_note = is_note.

  CALL FUNCTION 'SCWB_NOTE_READ'
    EXPORTING
      iv_read_short_text          = abap_true
      iv_read_corr_instructions   = abap_true
      iv_read_customer_attributes = abap_true
    CHANGING
      cs_note                     = ls_note
    EXCEPTIONS
      OTHERS                      = 0.

  " Get implementation status
  CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
    EXPORTING
      is_note                    = ls_note
    IMPORTING
      ev_status                  = lv_status
    EXCEPTIONS
      note_not_found             = 1
      inconsistent_delivery_data = 2
      undefined_component_state  = 3
      incomplete_note_data       = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    RAISE status_error.
  ENDIF.

  ev_status = lv_status.

  " Map status to descriptive text
  CASE lv_status.
    WHEN gc_prstat_obsolete OR 'V'.
      ev_status_txt = 'Obsolete - Note is no longer valid'.
    WHEN gc_prstat_not_implemented.
       ev_status_txt = 'Can be Implemented - Ready for implementation'.
    WHEN gc_prstat_implemented.
      ev_status_txt = 'Implemented - Already applied to system'.
    WHEN gc_prstat_partial.
      ev_status_txt = 'Partially Implemented - Some corrections applied'.
    WHEN 'U'.
      ev_status_txt = 'Cannot be Implemented - Manual changes required'.
    WHEN OTHERS.
      ev_status_txt = |Unknown Status: { lv_status }|.
  ENDCASE.

ENDMETHOD.


METHOD notestatusset_get_entity.
*---------------------------------------------------------------------*
* Get single note status
*---------------------------------------------------------------------*
  DATA: ls_entity       TYPE zcl_zsnote_srv_mpc=>ts_notestatus,
        ls_note         TYPE bcwbn_note,
        lv_note_number  TYPE cwbntkey-numm,
        lv_downloaded   TYPE abap_bool,
        lv_already_latest TYPE abap_bool,
        lv_local_version TYPE char4,
        lv_remote_version TYPE char4,
        lv_status       TYPE cwbprstat,
        lv_status_txt   TYPE string.

  " Read note number from key
  READ TABLE it_key_tab WITH KEY name = 'NoteNumber' INTO DATA(ls_key).
  IF sy-subrc = 0.
    lv_note_number = ls_key-value.
  ELSE.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'Note Number is required'.
  ENDIF.

  ls_entity-notenumber = lv_note_number.

  TRY.
      " Check version and download if necessary
      note_download(
        EXPORTING
          iv_note_number  = lv_note_number
        IMPORTING
          ev_downloaded     = lv_downloaded
          ev_already_latest = lv_already_latest
          ev_local_version  = lv_local_version
          ev_remote_version = lv_remote_version
          es_note           = ls_note
        EXCEPTIONS
          download_failed      = 1
          version_check_failed = 2 ).

      IF sy-subrc = 0.
        ls_entity-noteversion     = ls_note-key-versno.
        ls_entity-notetext        = ls_note-stext.
        ls_entity-downloaded       = lv_downloaded.
        ls_entity-alreadylatest   = lv_already_latest.
        ls_entity-localversion    = lv_local_version.
        ls_entity-remoteversion   = lv_remote_version.

        " Get implementation status
        get_implementation_status(
          EXPORTING
            is_note       = ls_note
          IMPORTING
            ev_status     = lv_status
            ev_status_txt = lv_status_txt
          EXCEPTIONS
            status_error = 1 ).

        IF sy-subrc = 0.
          ls_entity-implstatus      = lv_status.
          ls_entity-implstatustext = lv_status_txt.
        ELSE.
          ls_entity-implstatustext = 'Error getting status'.
        ENDIF.

      ELSE.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = 'Error checking note version'.
      ENDIF.

    CATCH cx_root INTO DATA(lx_error).

  ENDTRY.

  " Copy to export parameter
  er_entity = ls_entity.

ENDMETHOD.


METHOD notestatusset_get_entityset.
*---------------------------------------------------------------------*
* Get list of notes with their status
*---------------------------------------------------------------------*
  DATA: lt_note_numbers TYPE RANGE OF cwbntkey-numm,
        ls_note_number  LIKE LINE OF lt_note_numbers,
        ls_entity       TYPE zcl_zsnote_srv_mpc=>ts_notestatus,
        ls_note         TYPE bcwbn_note,
        lv_downloaded   TYPE abap_bool,
        lv_already_latest TYPE abap_bool,
        lv_local_version TYPE char4,
        lv_remote_version TYPE char4,
        lv_status       TYPE cwbprstat,
        lv_status_txt   TYPE string.

  " If no filter provided, show error
  IF lt_note_numbers IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = 'Please provide Note Number in filter'.
  ENDIF.

  " Process each note number
  LOOP AT lt_note_numbers INTO ls_note_number.
    CLEAR: ls_entity, ls_note, lv_downloaded, lv_already_latest,
           lv_local_version, lv_remote_version.

    ls_entity-notenumber = ls_note_number-low.

    TRY.
        " Check version and download if necessary
        note_download(
          EXPORTING
            iv_note_number    = ls_note_number-low
          IMPORTING
            ev_downloaded     = lv_downloaded
            ev_already_latest = lv_already_latest
            ev_local_version  = lv_local_version
            ev_remote_version = lv_remote_version
            es_note           = ls_note
          EXCEPTIONS
            download_failed      = 1
            version_check_failed = 2 ).

        IF sy-subrc = 0.
          " Populate entity with note info
          ls_entity-noteversion     = ls_note-key-versno.
          ls_entity-notetext        = ls_note-stext.
          ls_entity-downloaded       = lv_downloaded.
          ls_entity-alreadylatest   = lv_already_latest.
          ls_entity-localversion    = lv_local_version.
          ls_entity-remoteversion   = lv_remote_version.

          " Get implementation status
          get_implementation_status(
            EXPORTING
              is_note       = ls_note
            IMPORTING
              ev_status     = lv_status
              ev_status_txt = lv_status_txt
            EXCEPTIONS
              status_error = 1 ).

          IF sy-subrc = 0.
            ls_entity-implstatus      = lv_status.
            ls_entity-implstatustext = lv_status_txt.
          ELSE.
            ls_entity-implstatustext = 'Error getting status'.
          ENDIF.

        ELSE.
          ls_entity-implstatustext = 'Error checking note version'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        ls_entity-implstatustext = lx_error->get_text( ).
    ENDTRY.

    APPEND ls_entity TO et_entityset.
  ENDLOOP.

ENDMETHOD.


METHOD NOTE_DOWNLOAD.
*---------------------------------------------------------------------*
* Check if note needs download and download if necessary
*---------------------------------------------------------------------*
  DATA: ls_note           TYPE bcwbn_note,
        ls_note_before    TYPE bcwbn_note,
        lv_version_before TYPE char4,
        lv_version_after  TYPE char4,
        lv_exists         TYPE abap_bool,
        lt_notes          TYPE bcwbn_notes.

  CLEAR: ev_downloaded, ev_already_latest, es_note,
         ev_local_version, ev_remote_version.

  " Initialize note key
  ls_note-key-numm = iv_note_number.

  " STEP 1: Try to read note from local system
  CALL FUNCTION 'SCWB_NOTE_READ'
    EXPORTING
      iv_read_attributes          = abap_true
      iv_read_customer_attributes = abap_true
      iv_read_short_text          = abap_true
    CHANGING
      cs_note                     = ls_note
    EXCEPTIONS
      note_not_found              = 1
      OTHERS                      = 2.

  IF sy-subrc = 0.
    " Note exists locally
    lv_exists = abap_true.
    lv_version_before = ls_note-key-versno.
    ev_local_version = lv_version_before.
    ls_note_before = ls_note.
  ENDIF.

  " STEP 2: Attempt to download/update note
  " This function checks SAP Service and downloads if newer version exists
  CLEAR lt_notes.
  APPEND ls_note TO lt_notes.

  CALL FUNCTION 'SCWN_NOTE_DOWNLOAD'
    CHANGING
     IV_NOTE_NAME   = lt_notes
    EXCEPTIONS
      FAILURE           = 1
      RFC_ERROR         = 2
      NOTE_NOT_EXISTING = 3
      OTHERS            = 4.

  IF sy-subrc <> 0.
    " Download failed or was cancelled
    IF lv_exists = abap_true.
      " We have a local copy, use it
      ev_downloaded = abap_false.
      ev_already_latest = abap_true.
      es_note = ls_note_before.
      ev_remote_version = ev_local_version.
      RETURN.
    ELSE.
      RAISE download_failed.
    ENDIF.
  ENDIF.

  " STEP 3: Read the note again to get updated data
  CLEAR ls_note.
  ls_note-key-numm = iv_note_number.

  CALL FUNCTION 'SCWB_NOTE_READ'
    EXPORTING
      iv_read_attributes          = abap_true
      iv_read_customer_attributes = abap_true
      iv_read_short_text          = abap_true
      iv_read_corr_instructions   = abap_true
    CHANGING
      cs_note                     = ls_note
    EXCEPTIONS
      OTHERS                      = 1.

  IF sy-subrc <> 0.
    RAISE download_failed.
  ENDIF.

  lv_version_after = ls_note-key-versno.
  ev_remote_version = lv_version_after.
  es_note = ls_note.

  " STEP 4: Determine if download actually happened
  IF lv_exists = abap_true.
    " Note existed before
    IF lv_version_after > lv_version_before.
      " Version increased = new version was downloaded
      ev_downloaded = abap_true.
      ev_already_latest = abap_false.
    ELSE.
      " Version same = already had latest
      ev_downloaded = abap_false.
      ev_already_latest = abap_true.
    ENDIF.
  ELSE.
    " Note didn't exist before = was downloaded
    ev_downloaded = abap_true.
    ev_already_latest = abap_false.
    ev_local_version = '0000'.  " Didn't exist before
  ENDIF.

ENDMETHOD.
ENDCLASS.
