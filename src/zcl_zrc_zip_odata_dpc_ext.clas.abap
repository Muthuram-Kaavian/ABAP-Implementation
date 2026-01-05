class ZCL_ZRC_ZIP_ODATA_DPC_EXT definition
  public
  inheriting from ZCL_ZRC_ZIP_ODATA_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods RUNSET_CREATE_ENTITY
    redefinition .
  methods RUNSET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZRC_ZIP_ODATA_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

  DATA lv_entity TYPE string.
  DATA lv_runid  TYPE string.
  DATA lv_path   TYPE string.
  DATA lv_xall   TYPE xstring.
  DATA lv_xbuf   TYPE xstring.

  FIELD-SYMBOLS <ls_media> TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_media_resource.

  "Which entity is requesting $value?
  lv_entity = io_tech_request_context->get_entity_type_name( ).

  "Only handle Zip entity
  IF lv_entity <> 'Zip'.
    super->/iwbep/if_mgw_appl_srv_runtime~get_stream(
      EXPORTING
        iv_entity_name          = iv_entity_name
        iv_entity_set_name      = iv_entity_set_name
        iv_source_name          = iv_source_name
        it_key_tab              = it_key_tab
        io_tech_request_context = io_tech_request_context
      IMPORTING
        er_stream               = er_stream
        es_response_context     = es_response_context ).
    RETURN.
  ENDIF.

  "Read RunId from URL: /ZipSet('RUNID')/$value
  READ TABLE it_key_tab WITH KEY name = 'RunId' INTO DATA(ls_key).
  IF sy-subrc = 0.
    lv_runid = ls_key-value.
  ENDIF.

  IF lv_runid IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message = 'RunId missing'.
  ENDIF.

  "CHANGE THIS: folder where readiness check zip is available
  lv_path = |/usr/sap/<SID>/SYS/global/rc/{ lv_runid }.zip|.

  "Read ZIP file
  OPEN DATASET lv_path FOR INPUT IN BINARY MODE.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message = |ZIP not found: { lv_path }|.
  ENDIF.

  DO.
    READ DATASET lv_path INTO lv_xbuf MAXIMUM LENGTH 65535.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    lv_xall = lv_xall && lv_xbuf.
  ENDDO.

  CLOSE DATASET lv_path.

  "Return as media resource
  IF er_stream IS INITIAL.
    CREATE DATA er_stream TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_media_resource.
  ENDIF.

  ASSIGN er_stream->* TO <ls_media>.
  <ls_media>-mime_type = 'application/zip'.
  <ls_media>-value     = lv_xall.

ENDMETHOD.


METHOD runset_create_entity.

  DATA: ls_input   TYPE zcl_zrc_zip_odata_mpc=>ts_run,
        lv_guid_32 TYPE sysuuid_c32.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_input ).

  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_32 = lv_guid_32.

  er_entity-runid         = lv_guid_32.
  er_entity-targetversion = ls_input-targetversion.
  er_entity-status        = 'CREATED'.
  er_entity-message       = 'Run created successfully'.

ENDMETHOD.


METHOD runset_get_entity.

  READ TABLE it_key_tab WITH KEY name = 'RunId' INTO DATA(ls_key).
  IF sy-subrc = 0.
    er_entity-runid = ls_key-value.
  ENDIF.

  er_entity-status  = 'OK'.
  er_entity-message = 'Status check working'.
ENDMETHOD.
ENDCLASS.
