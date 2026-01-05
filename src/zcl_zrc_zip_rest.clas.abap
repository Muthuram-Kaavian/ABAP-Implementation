class ZCL_ZRC_ZIP_REST definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZRC_ZIP_REST IMPLEMENTATION.


METHOD if_http_extension~handle_request.

  DATA lv_json TYPE string.
  lv_json = server->request->get_cdata( ).

  TYPES: BEGIN OF ty_req,
           targetversion TYPE string,
         END OF ty_req.
  DATA ls_req TYPE ty_req.

  TRY.
      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_json
        CHANGING  data = ls_req
      ).
    CATCH cx_root INTO DATA(lx).
      server->response->set_status( code = 400 reason = 'Invalid JSON' ).
      server->response->set_cdata( lx->get_text( ) ).
      RETURN.
  ENDTRY.

  IF ls_req-targetversion IS INITIAL.
    server->response->set_status( code = 400 reason = 'TargetVersion missing' ).
    RETURN.
  ENDIF.

  "TODO: Here you must read the correct readiness ZIP for that TargetVersion from APP SERVER (AL11)

  server->response->set_status( code = 501 reason = 'Not implemented' ).
  server->response->set_cdata( |Received TargetVersion: { ls_req-targetversion }| ).

ENDMETHOD.
ENDCLASS.
