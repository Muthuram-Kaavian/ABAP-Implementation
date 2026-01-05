class ZCL_Z_TR_CREATE_DPC_EXT definition
  public
  inheriting from ZCL_Z_TR_CREATE_DPC
  create public .

public section.
protected section.

  methods TRS_CREATE_ENTITY
    redefinition .
  methods TRS_GET_ENTITY
    redefinition .
private section.

  methods RAISE_BUSINESS_EXCEPTION
    importing
      !IV_MESSAGE type STRING
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_Z_TR_CREATE_DPC_EXT IMPLEMENTATION.


METHOD raise_business_exception.
  DATA(lo_message_container) = mo_context->get_message_container( ).
  lo_message_container->add_message_text_only(
    iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
    iv_msg_text = CONV #( iv_message )  " Convert to appropriate type
  ).
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
    EXPORTING
      textid            = /iwbep/cx_mgw_busi_exception=>business_error
      message_container = lo_message_container.
ENDMETHOD.


METHOD trs_create_entity.
  " Signature:
  "   IMPORTING  io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider
  "   CHANGING   er_entity        TYPE zcl_z_tr_create_mpc=>ts_tr

  DATA: ls_request_data  TYPE zcl_z_tr_create_mpc=>ts_tr,   " request payload
        ls_response_data TYPE zcl_z_tr_create_mpc=>ts_tr,   " response payload
        ls_hdr           TYPE trwbo_request_header,
        lt_tsk           TYPE trwbo_request_headers,
        lv_target        TYPE tr_target,
        lv_text          TYPE as4text,                      " TR description C(60)
        lv_req           TYPE string.                       " work string for sanitizing

  " Read payload from OData request into local structure ls_request_data
  io_data_provider->read_entry_data( IMPORTING es_data = ls_request_data ).

  " ===================== Validate required 'Request' =====================
  IF ls_request_data-request IS INITIAL.
    raise_business_exception( 'Request (description) is required' ).
  ENDIF.

  " ===================== Determine & validate target system =====================
  IF ls_request_data-targetsystem IS INITIAL.
    lv_target = 'CL5'.
  ELSE.
    lv_target = to_upper( ls_request_data-targetsystem ).
  ENDIF.

  IF strlen( lv_target ) <> 3 OR lv_target NA 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.
    raise_business_exception( |Invalid TargetSystem '{ ls_request_data-targetsystem }' (expect 3-char SID).| ).
  ENDIF.

  " ===================== Sanitize description for IV_TEXT (AS4TEXT, C(60)) =====================
  lv_req = ls_request_data-request.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN lv_req WITH ' '.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN lv_req WITH ' '.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_req WITH ' '.
  SHIFT lv_req LEFT  DELETING LEADING space.
  SHIFT lv_req RIGHT DELETING TRAILING space.
  CONDENSE lv_req.
  IF strlen( lv_req ) > 60.
    lv_text = lv_req(60).
  ELSE.
    lv_text = lv_req.
  ENDIF.

  " ===================== Create Transport Request =====================
  CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
    EXPORTING
      " 'K' indicates the type of transport request:
      " K = Workbench Request (cross-client changes, e.g., programs, dictionary objects)
      iv_type           = 'K'
      iv_text           = lv_text
      iv_owner          = sy-uname
      iv_target         = lv_target
    IMPORTING
      es_request_header = ls_hdr
      et_task_headers   = lt_tsk
    EXCEPTIONS
      insert_failed     = 1
      enqueue_failed    = 2
      OTHERS            = 3.

  IF sy-subrc <> 0 OR ls_hdr-trkorr IS INITIAL.
    raise_business_exception( |TR creation failed (SUBRC={ sy-subrc }, Target={ lv_target }).| ).
  ENDIF.

  " ===================== Build response =====================
  ls_response_data-requestnumber = ls_hdr-trkorr.
  ls_response_data-request       = lv_text.
  ls_response_data-targetsystem  = lv_target.

  er_entity = ls_response_data.
ENDMETHOD.


METHOD trs_get_entity.

  " Local data: request key, TR number, headers, objects tables
  DATA: ls_key     TYPE /iwbep/s_mgw_name_value_pair,
        lv_trkorr  TYPE trkorr.

  DATA: ls_e070_main TYPE e070,
        lt_hdrs      TYPE trwbo_request_headers,
        ls_main_hdr  TYPE trwbo_request_header,
        lt_e071      TYPE STANDARD TABLE OF e071,
        lt_e071k     TYPE STANDARD TABLE OF e071k.

  " Read key from OData input: expects name = 'RequestNumber'
  READ TABLE it_key_tab INTO ls_key WITH KEY name = 'RequestNumber'.
  IF sy-subrc <> 0 OR ls_key-value IS INITIAL.
    " Validation: TR number is required
    raise_business_exception( 'Transport request number is required' ).
  ENDIF.
  lv_trkorr = ls_key-value.

  " Existence check via E070 (transport request main header)
  SELECT SINGLE * FROM e070 INTO @ls_e070_main WHERE trkorr = @lv_trkorr.

  " Added explicit validation: TR number must exist
  IF sy-subrc <> 0.
    raise_business_exception(
      |Transport Request "{ lv_trkorr }" not found. Please check and provide a valid transport request number.|
    ).
  ENDIF.

  IF sy-subrc <> 0.
    raise_business_exception( |Transport Request "{ lv_trkorr }" not found| ).
  ENDIF.

  " Function Module: TR_READ_REQUEST_WITH_TASKS
  CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
    EXPORTING
      iv_trkorr          = lv_trkorr
    IMPORTING
      et_request_headers = lt_hdrs
    EXCEPTIONS
      invalid_input      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    " Any FM failure bubbles up as business error
    raise_business_exception( |Reading TR "{ lv_trkorr }" failed (TR_READ_REQUEST_WITH_TASKS)| ).
  ENDIF.

  " Identify main header row (same TRKORR as input)
  READ TABLE lt_hdrs INTO ls_main_hdr WITH KEY trkorr = lv_trkorr.

  " Build Tasks JSON: child items (where STRKORR = main TR)
  TYPES: BEGIN OF ty_task_out,
           TaskNumber   TYPE trkorr,
           ParentTR     TYPE trkorr,
           TargetSystem TYPE tarsystem,
           Owner        TYPE as4user,
           CreatedOn    TYPE as4date,
           CreatedTime  TYPE as4time,
         END OF ty_task_out.

  DATA: lt_tasks_out TYPE STANDARD TABLE OF ty_task_out,
        ls_task_out  TYPE ty_task_out.

  LOOP AT lt_hdrs ASSIGNING FIELD-SYMBOL(<h>)
       WHERE strkorr = lv_trkorr AND trkorr <> lv_trkorr.
    CLEAR ls_task_out.
    ls_task_out-tasknumber   = <h>-trkorr.
    ls_task_out-parenttr     = <h>-strkorr.
    ls_task_out-targetsystem = <h>-tarsystem.
    ls_task_out-owner        = <h>-as4user.
    ls_task_out-createdon    = <h>-as4date.
    ls_task_out-createdtime  = <h>-as4time.
    APPEND ls_task_out TO lt_tasks_out.
  ENDLOOP.

  " Serialize tasks to JSON string for exposure in OData
  DATA(lv_tasks_json) = /ui2/cl_json=>serialize( data = lt_tasks_out ).

  " Objects of MAIN TR: E071/E071K
  SELECT * FROM e071  INTO TABLE @lt_e071  WHERE trkorr = @lv_trkorr.
  SELECT * FROM e071k INTO TABLE @lt_e071k WHERE trkorr = @lv_trkorr.

  " Serialize ALL objects (no filtering) into ObjectsJson
  TYPES: BEGIN OF ty_object_out,
           PgmId   TYPE e071-pgmid,
           Object  TYPE e071-object,
           ObjName TYPE e071-obj_name,
         END OF ty_object_out.

  DATA: lt_objects_out TYPE STANDARD TABLE OF ty_object_out WITH EMPTY KEY.

  FIELD-SYMBOLS: <s71o> TYPE e071.
  LOOP AT lt_e071 ASSIGNING <s71o>.
    APPEND VALUE ty_object_out(
      PgmId   = <s71o>-pgmid
      Object  = <s71o>-object
      ObjName = <s71o>-obj_name
    ) TO lt_objects_out.
  ENDLOOP.

  DATA(lv_objects_json) = /ui2/cl_json=>serialize( data = lt_objects_out ).

  " Fill OData entity (response fields)
  CLEAR er_entity.
  er_entity-requestnumber = lv_trkorr.
  er_entity-request       = ls_main_hdr-as4text.
  er_entity-targetsystem  = ls_e070_main-tarsystem.
  er_entity-owner         = ls_main_hdr-as4user.
  er_entity-category      = ls_e070_main-trfunction.
  er_entity-status        = ls_e070_main-trstatus.
  er_entity-released      = xsdbool( ls_e070_main-trstatus = 'R' ).
  er_entity-createdon     = |{ ls_main_hdr-as4date } { ls_main_hdr-as4time }|.

  " JSON strings pushed to string properties for client consumption
  er_entity-tasksjson   = lv_tasks_json.
  er_entity-objectsjson = lv_objects_json.

ENDMETHOD.
ENDCLASS.
