class ZCL_ZTR_LIST_DPC_EXT definition
  public
  inheriting from ZCL_ZTR_LIST_DPC
  create public .

public section.
protected section.

  methods TRANSPORTREQUEST_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZTR_LIST_DPC_EXT IMPLEMENTATION.


METHOD transportrequest_get_entityset.
  TYPES:
    BEGIN OF ty_transport_request,
      transportrequestid TYPE trkorr,      " Matches metadata: TransportRequestID
      transportfunction  TYPE string,      " Matches metadata: TransportFunction
      transportstatus    TYPE string,      " Matches metadata: TransportStatus
      targetsystem       TYPE tarsystem,   " Matches metadata: TargetSystem
    END OF ty_transport_request.

  DATA: lt_final_data TYPE TABLE OF ty_transport_request.

  " Select data from E070
  SELECT trkorr, trfunction, trstatus, tarsystem
    FROM e070
    INTO TABLE @DATA(lt_e070_data)
    WHERE trstatus IN ('D', 'L').

  IF sy-subrc = 0.
    " Process data
    LOOP AT lt_e070_data ASSIGNING FIELD-SYMBOL(<fs_e070>).
      APPEND INITIAL LINE TO lt_final_data ASSIGNING FIELD-SYMBOL(<fs_final>).

      " Map fields - NOTE THE CHANGED FIELD NAMES:
      <fs_final>-transportrequestid = <fs_e070>-trkorr.      " Was transportrequest
      <fs_final>-targetsystem       = <fs_e070>-tarsystem.

      " Map transport function (this should be TransportFunction, not TransportType)
      CASE <fs_e070>-trfunction.
        WHEN 'K'. <fs_final>-transportfunction = 'Workbench request'.
        WHEN 'W'. <fs_final>-transportfunction = 'Customizing request'.
        WHEN 'T'. <fs_final>-transportfunction = 'Transport of copies'.
        WHEN 'C'. <fs_final>-transportfunction = 'Client transport'.
        WHEN OTHERS. <fs_final>-transportfunction = <fs_e070>-trfunction.
      ENDCASE.

      " Map transport status
      CASE <fs_e070>-trstatus.
        WHEN 'D'. <fs_final>-transportstatus = 'Modifiable'.
        WHEN 'L'. <fs_final>-transportstatus = 'Modifiable, Protected'.
        WHEN 'R'. <fs_final>-transportstatus = 'Released'.
        WHEN OTHERS. <fs_final>-transportstatus = <fs_e070>-trstatus.
      ENDCASE.
    ENDLOOP.

    et_entityset = CORRESPONDING #( lt_final_data ).
  ELSE.
    CLEAR et_entityset.
  ENDIF.
ENDMETHOD.
ENDCLASS.
