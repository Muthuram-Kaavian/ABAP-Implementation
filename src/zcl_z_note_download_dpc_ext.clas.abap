class ZCL_Z_NOTE_DOWNLOAD_DPC_EXT definition
  public
  inheriting from ZCL_Z_NOTE_DOWNLOAD_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_NOTE_DOWNLOAD_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

  " Check which function import was called by the URI
  CASE iv_action_name.
    WHEN 'DownloadNotes'. " This must match the name of your Function Import in SEGW

      DATA: lt_notes_to_download TYPE bcwbn_note_keys_lg,
            ls_note_to_download  TYPE cwbntkeylg,
            ls_parameter         TYPE /iwbep/s_mgw_name_value_pair,
            ls_note              TYPE bcwbn_note,
            lv_status            TYPE cwbprstat.

      " It's recommended to define this structure in your MPC (Model Provider Class)
      TYPES: BEGIN OF ty_note_status,
               notenumber TYPE cwbntnumm,
               message    TYPE string,
             END OF ty_note_status.

      DATA: lt_final_status TYPE STANDARD TABLE OF ty_note_status,
            ls_final_status TYPE ty_note_status.

      "--------------------------------------------------------------------
      " STEP 1: Pre-check all notes provided in the input
      "--------------------------------------------------------------------
      " Loop through all incoming note numbers. For each one, check if it's
      " already in the system and complete. If not, add it to a download list.
      LOOP AT it_parameter INTO ls_parameter WHERE name = 'NoteNumber'.
        CLEAR: ls_note, ls_final_status, lv_status.
        ls_final_status-notenumber = ls_parameter-value.
        ls_note-key-numm = ls_parameter-value.

        " Try to read the note's data from the local system
        CALL FUNCTION 'SCWB_NOTE_READ'
          EXPORTING
            iv_read_attributes          = abap_true
            iv_read_customer_attributes = abap_true
            iv_read_corr_instructions   = abap_true
          CHANGING
            cs_note                     = ls_note
          EXCEPTIONS
            OTHERS                      = 1.

        " If the note is NOT found (subrc<>0) or is incomplete, it needs to be downloaded.
        IF sy-subrc <> 0 OR ls_note-incomplete = abap_true.
          ls_note_to_download-numm  = ls_parameter-value.
          ls_note_to_download-langu = sy-langu.
          APPEND ls_note_to_download TO lt_notes_to_download.
        ELSE.
          " The note exists locally. Get its implementation status.
          CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS'
            EXPORTING
              is_note   = ls_note
            IMPORTING
              ev_status = lv_status
            EXCEPTIONS
              OTHERS    = 1.

          IF sy-subrc = 0.
            " Use hardcoded constants or define them in your class
           CASE lv_status.
             WHEN 'I' OR 'IN'.  " Implemented
               ls_final_status-message = |Note is already implemented.|.
             WHEN 'N' OR 'NI'.  " Not implemented
               ls_final_status-message = |Note is present and can be implemented.|.
             WHEN 'O' OR 'X' OR 'OB'.  " Obsolete or no valid instructions
               ls_final_status-message = |Note is obsolete or has no valid correction instructions.|.
             WHEN 'IP' OR 'PR'.  " In process
               ls_final_status-message = |Note implementation is in progress.|.
             WHEN OTHERS.
               ls_final_status-message = |Note status: { lv_status } - Please check manually.|.
           ENDCASE.
          ELSE.
            ls_final_status-message = |Could not determine implementation status for the note.|.
          ENDIF.
          APPEND ls_final_status TO lt_final_status.
        ENDIF.
      ENDLOOP.

      "--------------------------------------------------------------------
      " STEP 2: Download all necessary notes in a single batch
      "--------------------------------------------------------------------
      IF lt_notes_to_download IS NOT INITIAL.
        CALL FUNCTION 'SCWN_NOTES_DOWNLOAD'
          EXPORTING
            it_note_keys_lg = lt_notes_to_download
          EXCEPTIONS
            error           = 1
            rfc_error       = 2
            OTHERS          = 3.

        DATA(lv_download_subrc) = sy-subrc.

        "------------------------------------------------------------------
        " STEP 3: Check the status of the newly downloaded notes
        "------------------------------------------------------------------
        LOOP AT lt_notes_to_download INTO ls_note_to_download.
          CLEAR ls_final_status.
          ls_final_status-notenumber = ls_note_to_download-numm.

          IF lv_download_subrc = 0.
            ls_final_status-message = |Note downloaded successfully. It can now be implemented.|.
          ELSE.
            ls_final_status-message = |Error downloading Note. SY-SUBRC: { lv_download_subrc }.|.
          ENDIF.
          APPEND ls_final_status TO lt_final_status.
        ENDLOOP.
      ENDIF.

      "--------------------------------------------------------------------
      " STEP 4: Pass the final consolidated status table back to the OData framework
      "--------------------------------------------------------------------
      copy_data_to_ref(
        EXPORTING
          is_data = lt_final_status
        CHANGING
          cr_data = er_data
      ).

    WHEN OTHERS.
      " Handle other function imports or raise an exception
      super->/iwbep/if_mgw_appl_srv_runtime~execute_action(
        EXPORTING
          iv_action_name      = iv_action_name
          it_parameter        = it_parameter
          io_tech_request_context = io_tech_request_context
        IMPORTING
          er_data             = er_data ).
  ENDCASE.

ENDMETHOD.
ENDCLASS.
