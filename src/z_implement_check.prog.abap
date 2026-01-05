REPORT z_implement_check.

PARAMETERS: p_note TYPE c LENGTH 10 OBLIGATORY,
            p_tr   TYPE trkorr       OBLIGATORY.

DATA: lv_note_obj TYPE e071-obj_name.

" IMPORTANT: Make it a TRKORR table (not string table)
DATA: lt_trkorrs TYPE STANDARD TABLE OF trkorr WITH EMPTY KEY,
      lv_trk     TYPE trkorr,
      lv_parent  TYPE trkorr.

lv_note_obj = |{ p_note ALPHA = IN }|.  " e.g. 0002502552

SELECT DISTINCT trkorr
  FROM e071
  WHERE pgmid    = 'R3TR'
    AND object   = 'NOTE'
    AND obj_name = @lv_note_obj
  INTO TABLE @lt_trkorrs.

IF lt_trkorrs IS INITIAL.
  WRITE: / |No CTS entry found for NOTE { lv_note_obj } in E071.|.
  RETURN.
ENDIF.

" Check if the given TR is among the found TRs/tasks
READ TABLE lt_trkorrs WITH KEY table_line = p_tr TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  WRITE: / |OK: Transport { p_tr } already holds SAP Note { lv_note_obj } (E071 contains R3TR NOTE).|.
  RETURN.
ENDIF.

WRITE: / |NOT OK: Transport { p_tr } does NOT hold SAP Note { lv_note_obj }.|.
WRITE: / |It is held in these request/task(s):|.
ULINE.
WRITE: / 'TRKORR', 15 'PARENT'.
ULINE.

LOOP AT lt_trkorrs INTO lv_trk.
  CLEAR lv_parent.
  SELECT SINGLE strkorr
    FROM e070
    WHERE trkorr = @lv_trk
    INTO @lv_parent.

  WRITE: / lv_trk, 15 lv_parent.
ENDLOOP.

ULINE.
WRITE: / |Action: Use one of the above TRKORR (or its PARENT request) to implement the note.|.
