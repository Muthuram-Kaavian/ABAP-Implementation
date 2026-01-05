REPORT z_s4_si_sdmi_autocheck.

CONSTANTS: c_path TYPE string VALUE '/usr/sap/interface/si_sdmi/'.

DATA: lv_ts        TYPE string,
      lv_file_si   TYPE string,
      lv_file_sd   TYPE string,
      lv_rep_sdmi  TYPE progname,
      lv_exist     TYPE c.

DATA: lt_abaplist  TYPE STANDARD TABLE OF abaplist,
      lt_ascii     TYPE STANDARD TABLE OF soli,
      lv_line      TYPE soli-line.

lv_ts        = |{ sy-datum }_{ sy-uzeit }|.
lv_file_si   = c_path && |si_results_|   && lv_ts && |.csv|.
lv_file_sd   = c_path && |sdmi_results_| && lv_ts && |.csv|.

*------- helper: run report (optionally via variant) and dump list to file
FORM run_and_save USING iv_report  TYPE sy-repid
                        iv_variant TYPE raldb-variant
                        iv_file    TYPE string.

  REFRESH lt_abaplist.
  REFRESH lt_ascii.

  " check if variant exists (if given)
  lv_exist = ' '.
  IF iv_variant IS NOT INITIAL.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING  report  = iv_report
                 variant = iv_variant
      IMPORTING  r_c     = lv_exist
      EXCEPTIONS not_authorized = 1 OTHERS = 2.
  ENDIF.

  IF iv_variant IS NOT INITIAL AND lv_exist = 'X'.
    SUBMIT (iv_report)
           USING SELECTION-SET iv_variant
           EXPORTING LIST TO MEMORY
           AND RETURN.
  ELSE.
    " run with default selections (no variant)
    SUBMIT (iv_report)
           EXPORTING LIST TO MEMORY
           AND RETURN.
  ENDIF.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES listobject = lt_abaplist
    EXCEPTIONS not_found = 1 OTHERS = 2.
  IF sy-subrc <> 0 OR lt_abaplist IS INITIAL.
    WRITE: / iv_report, ' produced no list (check notes/authorizations).'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'LIST_TO_ASCI'
    EXPORTING list_index = -1
    TABLES    listasci   = lt_ascii
              listobject = lt_abaplist
    EXCEPTIONS empty_list = 1 OTHERS = 2.

  OPEN DATASET iv_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc <> 0.
    WRITE: / 'Cannot open file:', iv_file.
    RETURN.
  ENDIF.
  LOOP AT lt_ascii INTO lv_line.
    TRANSFER lv_line TO iv_file.
  ENDLOOP.
  CLOSE DATASET iv_file.
  WRITE: / iv_report, ' results saved to:', iv_file.

ENDFORM.

START-OF-SELECTION.
  WRITE: / 'Running automated SI & SDMI checks…'.

  " 1) Simplification Item Check (variant optional)
  PERFORM run_and_save USING '/SDF/RC_START_CHECK' 'Z_ALL_DETAILED' lv_file_si.

  " 2) SDMI report name can differ per release → pick what exists
  SELECT SINGLE name FROM trdir INTO lv_rep_sdmi
    WHERE name = '/SDF/SDMI_CONSISTENCY_CHECK'.
  IF sy-subrc <> 0.
    SELECT SINGLE name FROM trdir INTO lv_rep_sdmi
      WHERE name = '/SDF/RC_SDMI_CHECK'.
  ENDIF.
  IF lv_rep_sdmi IS INITIAL.
    WRITE: / 'No SDMI report found. Check notes/content installation.'.
  ELSE.
    PERFORM run_and_save USING lv_rep_sdmi 'Z_DEFAULT' lv_file_sd.
  ENDIF.

  WRITE: / 'Done. Check AL11 path:', c_path.
