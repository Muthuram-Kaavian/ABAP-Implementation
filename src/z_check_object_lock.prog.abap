*&---------------------------------------------------------------------*
*& Report Z_CHECK_NOTE_OBJECT_LOCKS
*&---------------------------------------------------------------------*

REPORT z_check_object_locks.

TABLES: dd02l, tadir.

SELECT * FROM dd02l
  WHERE tabname = 'SCUILGET_SAPCORE_INFO'
     OR tabname LIKE '%SCUIL%'.
  WRITE: / 'Table locked:', dd02l-tabname.
ENDSELECT.
