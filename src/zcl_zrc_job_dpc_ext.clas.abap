class ZCL_ZRC_JOB_DPC_EXT definition
  public
  inheriting from ZCL_ZRC_JOB_DPC
  create public .

public section.
protected section.

  methods READINESSJOBSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZRC_JOB_DPC_EXT IMPLEMENTATION.


METHOD readinessjobset_create_entity.

  DATA: ls_input      TYPE zcl_zrc_job_mpc=>ts_readinessjob,
        lv_jobname    TYPE tbtcjob-jobname VALUE 'Z_READINESS_CHECK',
        lv_jobcount   TYPE tbtcjob-jobcount,
        lv_print      TYPE pri_params,
        lv_arc        TYPE arc_params.

  DATA: lo_msg_cont   TYPE REF TO /iwbep/if_message_container.
  DATA: lv_job_opened TYPE abap_bool VALUE abap_false.

  lo_msg_cont = mo_context->get_message_container( ).

  "1) Read request
  io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

  "2) Validate mandatory dropdown value (Target S/4HANA Version)
  IF ls_input-targetversion IS INITIAL.
    lo_msg_cont->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'TargetVersion is mandatory (maps to report parameter P_PRD_ST).' ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_msg_cont.
  ENDIF.

  "Default Output device
  IF ls_input-outputdevice IS INITIAL.
    ls_input-outputdevice = 'LP01'.
  ENDIF.

  "Default StartType
  IF ls_input-starttype IS INITIAL.
    ls_input-starttype = 'I'.
  ENDIF.

  "Only Immediate supported
  IF ls_input-starttype = 'D'.
    lo_msg_cont->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'StartType=D not supported because ScheduledStartDate/Time are not available in the service model. Use StartType=I.' ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_msg_cont.
  ENDIF.

  "3) Prepare spool
  CLEAR: lv_print, lv_arc.
  lv_print-pdest = ls_input-outputdevice.
  lv_print-primm = 'X'.
  lv_print-prrel = 'X'.

  "4) JOB_OPEN
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = lv_jobname
    IMPORTING
      jobcount = lv_jobcount
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    lo_msg_cont->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = |JOB_OPEN failed (SY-SUBRC={ sy-subrc })| ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING message_container = lo_msg_cont.
  ENDIF.

  lv_job_opened = abap_true.

  "5) SUBMIT inside TRY-CATCH to avoid leaving job open
  TRY.

      SUBMIT rc_collect_analysis_data
        WITH p_prd_st = ls_input-targetversion
        TO SAP-SPOOL
        SPOOL PARAMETERS lv_print
        WITHOUT SPOOL DYNPRO
        VIA JOB lv_jobname NUMBER lv_jobcount
        AND RETURN.

    CATCH cx_root INTO DATA(lx_any).

      "Best-effort cleanup: try to close the opened job without scheduling
      IF lv_job_opened = abap_true.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobname  = lv_jobname
            jobcount = lv_jobcount
          EXCEPTIONS
            OTHERS   = 1.
      ENDIF.

      lo_msg_cont->add_message_text_only(
        iv_msg_type = 'E'
        iv_msg_text = |SUBMIT failed: { lx_any->get_text( ) }| ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING message_container = lo_msg_cont.

  ENDTRY.

  "6) JOB_CLOSE - Immediate only
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = lv_jobname
      jobcount  = lv_jobcount
      strtimmed = abap_true
    EXCEPTIONS
      OTHERS    = 1.

  IF sy-subrc <> 0.
    lo_msg_cont->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = |JOB_CLOSE failed (SY-SUBRC={ sy-subrc })| ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING message_container = lo_msg_cont.
  ENDIF.

  "7) Response
  er_entity-targetsystem  = ls_input-targetsystem.
  er_entity-targetversion = ls_input-targetversion.
  er_entity-outputdevice  = ls_input-outputdevice.
  er_entity-starttype     = ls_input-starttype.
  er_entity-message       = |Job scheduled successfully for TargetVersion { ls_input-targetversion }|.

ENDMETHOD.
ENDCLASS.
