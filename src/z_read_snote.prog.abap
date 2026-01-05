REPORT z_read_snote.

TYPE-POOLS: bcwbn.

DATA ls_note TYPE bcwbn_note.
DATA(lv_note_no) = |2399707|.  " <-- your SAP Note number

"--- show top-level components (for debugging / visibility)
DATA lo_struc TYPE REF TO cl_abap_structdescr.
DATA lt_comp  TYPE abap_component_tab.
lo_struc ?= cl_abap_typedescr=>describe_by_data( ls_note ).
lt_comp   =  lo_struc->get_components( ).

WRITE: / 'Top-level components of BCWBN_NOTE:'.
LOOP AT lt_comp INTO DATA(ls_c).
  WRITE: / ls_c-name, ' -> ', ls_c-type->absolute_name.
ENDLOOP.
ULINE.

"--- try to place the note number into ls_note

FIELD-SYMBOLS: <fs_noteid> TYPE any,
               <fs_key>    TYPE any,
               <fs_inner>  TYPE any.

" 1) direct (top-level) common names
ASSIGN COMPONENT 'NOTEID' OF STRUCTURE ls_note TO <fs_noteid>.
IF <fs_noteid> IS NOT ASSIGNED.
  ASSIGN COMPONENT 'NOTE'     OF STRUCTURE ls_note TO <fs_noteid>.
ENDIF.
IF <fs_noteid> IS NOT ASSIGNED.
  ASSIGN COMPONENT 'NOTE_NO'  OF STRUCTURE ls_note TO <fs_noteid>.
ENDIF.
IF <fs_noteid> IS NOT ASSIGNED.
  ASSIGN COMPONENT 'NOTE_NR'  OF STRUCTURE ls_note TO <fs_noteid>.
ENDIF.
IF <fs_noteid> IS NOT ASSIGNED.
  ASSIGN COMPONENT 'NOTE_KEY' OF STRUCTURE ls_note TO <fs_key>.
  IF <fs_key> IS ASSIGNED.
    " 2) nested under NOTE_KEY
    ASSIGN COMPONENT 'NOTEID' OF STRUCTURE <fs_key> TO <fs_inner>.
    IF <fs_inner> IS NOT ASSIGNED.
      ASSIGN COMPONENT 'NOTE' OF STRUCTURE <fs_key> TO <fs_inner>.
    ENDIF.
    IF <fs_inner> IS NOT ASSIGNED.
      ASSIGN COMPONENT 'NOTE_NO' OF STRUCTURE <fs_key> TO <fs_inner>.
    ENDIF.
    IF <fs_inner> IS ASSIGNED.
      <fs_inner> = lv_note_no.
    ENDIF.
  ENDIF.
ELSE.
  <fs_noteid> = lv_note_no.
ENDIF.

" if we still haven't set anything, stop with a clear message
IF ( <fs_noteid> IS NOT ASSIGNED ) AND ( <fs_inner> IS NOT ASSIGNED ).
  MESSAGE 'Could not locate a NOTE or NOTEID field (even under NOTE_KEY). Check the printed component list.' TYPE 'E'.
  RETURN.
ENDIF.

"--- call the FM
CALL FUNCTION 'SCWB_NOTE_READ'
  CHANGING
    cs_note = ls_note
  EXCEPTIONS
    note_not_found = 1
    OTHERS         = 2.

IF sy-subrc <> 0.
  MESSAGE |SCWB_NOTE_READ failed, sy-subrc={ sy-subrc }| TYPE 'E'.
  RETURN.
ENDIF.

"--- show a few likely “title-ish” fields if present
WRITE: / 'After SCWB_NOTE_READ:'.
FIELD-SYMBOLS <fs_any> TYPE any.
ASSIGN COMPONENT 'TITLE'     OF STRUCTURE ls_note TO <fs_any>.
IF <fs_any> IS ASSIGNED AND <fs_any> IS NOT INITIAL.
  WRITE: / 'TITLE:', <fs_any>.
ENDIF.
ASSIGN COMPONENT 'HEADLINE'  OF STRUCTURE ls_note TO <fs_any>.
IF <fs_any> IS ASSIGNED AND <fs_any> IS NOT INITIAL.
  WRITE: / 'HEADLINE:', <fs_any>.
ENDIF.
ASSIGN COMPONENT 'TXT_HEAD'  OF STRUCTURE ls_note TO <fs_any>.
IF <fs_any> IS ASSIGNED AND <fs_any> IS NOT INITIAL.
  WRITE: / 'TXT_HEAD:', <fs_any>.
ENDIF.
