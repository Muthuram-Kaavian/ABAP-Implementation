*&---------------------------------------------------------------------*
*& Report ZRC_READINESS_LOGS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRC_READINESS_LOGS.

"---- Parameters ---------------------------------------------------------
PARAMETERS: p_days  TYPE i DEFAULT 7,              " last N days
            p_obj   TYPE string DEFAULT '/SDF/RC', " object pattern
            p_sobj  TYPE string DEFAULT '*',       " subobject pattern
            p_errors TYPE c AS CHECKBOX DEFAULT 'X'. " Only errors checkbox

"---- Types --------------------------------------------------------------
TYPES: BEGIN OF ty_hdr,
         lognumber  TYPE balloghndl,
         aldate     TYPE baldate,
         altime     TYPE baltime,
         object     TYPE balobj,
         subobject  TYPE balsubobj,
         aluser     TYPE syuname,
         extnumber  TYPE balnrext,
       END OF ty_hdr.

TYPES: BEGIN OF ty_log,
         aldate     TYPE baldate,
         altime     TYPE baltime,
         object     TYPE balobj,
         subobject  TYPE balsubobj,
         msgty      TYPE symsgty,
         msgid      TYPE symsgid,
         msgno      TYPE symsgno,
         msgtext    TYPE string,
         user       TYPE syuname,
         extnumber  TYPE balnrext,
       END OF ty_log.

DATA: gt_logs TYPE STANDARD TABLE OF ty_log.

"-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM read_logs_db.
  PERFORM print_list.

"-----------------------------------------------------------------------
FORM read_logs_db.
  DATA: lt_hdr  TYPE STANDARD TABLE OF ty_hdr,
        lt_balm TYPE STANDARD TABLE OF balm,
        ls_log  TYPE ty_log.

  DATA: lv_from TYPE baldate.
  DATA: lv_to   TYPE baldate.

  lv_from = sy-datum - p_days.
  lv_to   = sy-datum.

  " convert * -> % for SQL LIKE
  DATA: lv_obj_like  TYPE string,
        lv_sobj_like TYPE string.

  lv_obj_like = p_obj.
  lv_sobj_like = p_sobj.

  REPLACE ALL OCCURRENCES OF '*' IN lv_obj_like WITH '%'.
  REPLACE ALL OCCURRENCES OF '*' IN lv_sobj_like WITH '%'.

  " 1) fetch headers from BALHDR
  SELECT lognumber
         aldate
         altime
         object
         subobject
         aluser
         extnumber
    FROM balhdr
    INTO TABLE lt_hdr
    WHERE object   LIKE lv_obj_like
      AND subobject LIKE lv_sobj_like
      AND aldate BETWEEN lv_from AND lv_to.

  IF lt_hdr IS INITIAL.
    WRITE: / 'No Readiness Check logs found for given filters'.
    RETURN.
  ENDIF.

  " 2) for each header, fetch its messages from BALM and build output rows
  LOOP AT lt_hdr INTO DATA(ls_hdr).

    CLEAR lt_balm.

    " Only errors filter
    IF p_errors = 'X'.
      SELECT *
        FROM balm
        INTO TABLE lt_balm
        WHERE lognumber = ls_hdr-lognumber
          AND msgty = 'E'.  " Only Errors
    ELSE.
      SELECT *
        FROM balm
        INTO TABLE lt_balm
        WHERE lognumber = ls_hdr-lognumber.
    ENDIF.

    LOOP AT lt_balm INTO DATA(ls_balm).
      DATA: lv_text TYPE string.
      lv_text = ls_balm-msgv1 && ls_balm-msgv2 && ls_balm-msgv3 && ls_balm-msgv4.

      CLEAR ls_log.
      ls_log-aldate    = ls_hdr-aldate.
      ls_log-altime    = ls_hdr-altime.
      ls_log-object    = ls_hdr-object.
      ls_log-subobject = ls_hdr-subobject.
      ls_log-msgty     = ls_balm-msgty.
      ls_log-msgid     = ls_balm-msgid.
      ls_log-msgno     = ls_balm-msgno.
      ls_log-msgtext   = lv_text.
      ls_log-user      = ls_hdr-aluser.
      ls_log-extnumber = ls_hdr-extnumber.
      APPEND ls_log TO gt_logs.
    ENDLOOP.
  ENDLOOP.

  SORT gt_logs BY aldate DESCENDING altime DESCENDING.
ENDFORM.

"-----------------------------------------------------------------------
FORM print_list.
  DATA: ls TYPE ty_log.

  IF gt_logs IS INITIAL.
    WRITE: / 'No logs to display.'.
    RETURN.
  ENDIF.

  SKIP.
  FORMAT COLOR COL_HEADING.
  WRITE: / '================================================================================================='.

  IF p_errors = 'X'.
    WRITE: / | READINESS CHECK - ERROR LOGS ONLY (last { p_days } days) |.
  ELSE.
    WRITE: / | Readiness Check Logs (last { p_days } days)  Object={ p_obj }  Subobject={ p_sobj } |.
  ENDIF.

  WRITE: / '================================================================================================='.
  FORMAT RESET.

  SKIP.
  FORMAT COLOR COL_GROUP.
  WRITE: / 'Date      |Time   |Type|Object        |Subobject   |User       |Message Text'.
  WRITE: / '----------|-------|----|-------------|-----------|----------|-----------------------------------'.
  FORMAT RESET.

  LOOP AT gt_logs INTO ls.
    " Color coding based on message type
    CASE ls-msgty.
      WHEN 'E'.  " Error - Red
        FORMAT COLOR COL_NEGATIVE.
      WHEN 'W'.  " Warning - Yellow
        FORMAT COLOR COL_TOTAL.
      WHEN 'S'.  " Success - Green
        FORMAT COLOR COL_POSITIVE.
      WHEN 'I'.  " Information - Blue
        FORMAT COLOR COL_NORMAL.
      WHEN OTHERS.
        FORMAT RESET.
    ENDCASE.

    " Format the output line
    WRITE: / ls-aldate,
             '|', ls-altime,
             '|', ls-msgty,
             '|', ls-object,
             '|', ls-subobject,
             '|', ls-user,
             '|', ls-msgtext.

    FORMAT RESET.
  ENDLOOP.

  SKIP.
  WRITE: / '================================================================================================='.
  WRITE: / |Total messages: { lines( gt_logs ) }|.
  WRITE: / '================================================================================================='.
ENDFORM.
