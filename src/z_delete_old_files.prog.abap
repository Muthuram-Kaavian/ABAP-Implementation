REPORT Z_DELETE_OLD_FILES.

*&---------------------------------------------------------------------*
*& Report Z_DELETE_OLD_FILES
*&---------------------------------------------------------------------*
*& This program deletes files from a specified application server
*& directory based on name pattern and age. It includes a test mode
*& and a mandatory authorization check.
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
PARAMETERS: p_path TYPE c LENGTH 128 OBLIGATORY LOWER CASE, "Directory Path
            p_file TYPE c LENGTH 60  OBLIGATORY LOWER CASE DEFAULT '*.tmp', "File Pattern
            p_days TYPE i             OBLIGATORY DEFAULT 7, "Delete files older than (days)
            p_test TYPE c AS CHECKBOX DEFAULT 'X'. "Test Mode (X=Yes, space=No)

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gv_logical_filename TYPE string,
      gv_file_date        TYPE d,
      gv_cutoff_date      TYPE d.

DATA: BEGIN OF gs_file,
        name(255) TYPE c,
        size(20)  TYPE c,
        date(10)  TYPE c,
        time(8)   TYPE c,
        owner(20) TYPE c,
        mode(10)  TYPE c,
        sublist   TYPE c,
      END OF gs_file.

DATA: gt_files LIKE STANDARD TABLE OF gs_file.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  "This is a good place to set default paths if needed, e.g., from a TVARVC variant.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "1. CRITICAL: Authorization Check
  "Check if the user has permission to DELETE files in the specified path.
  "The 'FILENAME' check allows for wildcards, matching the directory.
  AUTHORITY-CHECK OBJECT 'S_DATASET'
    ID 'PROGRAM'  FIELD sy-cprog
    ID 'ACTVT'    FIELD '34'  "34 = Delete
    ID 'FILENAME' FIELD p_path.

  IF sy-subrc <> 0.
    MESSAGE 'You do not have authorization to delete files from this directory.' TYPE 'E'.
  ENDIF.


  "2. Get list of files from the server
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = p_path
      file_mask              = p_file
    TABLES
      dir_list               = gt_files
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.

  IF sy-subrc <> 0 AND sy-subrc <> 7. "Ignore empty directory error
    MESSAGE |Error reading directory { p_path }. SY-SUBRC: { sy-subrc }| TYPE 'E'.
  ELSEIF gt_files IS INITIAL.
    WRITE: / 'No files found matching the pattern in the specified directory.'.
    EXIT.
  ENDIF.


  "3. Process the files
  WRITE: / 'Processing started at:', sy-datum, sy-uzeit.
  IF p_test = 'X'.
    WRITE: / '*** RUNNING IN TEST MODE - NO FILES WILL BE DELETED ***'.
    ULINE.
  ELSE.
    WRITE: / '*** RUNNING IN PRODUCTION MODE - FILES WILL BE DELETED ***'.
    ULINE.
  ENDIF.
  WRITE: / 'Directory:', p_path.
  WRITE: / 'File Pattern:', p_file.
  WRITE: / 'Deleting files older than', p_days, 'days.'.
  ULINE.

  "Calculate the cutoff date
  gv_cutoff_date = sy-datum - p_days.
  WRITE: / 'Files with a modification date before', gv_cutoff_date, 'will be targeted.'.
  ULINE.


  LOOP AT gt_files INTO gs_file.
    "Convert the file's date from YYYYMMDD format in gs_file-date to a date object
    MOVE gs_file-date TO gv_file_date.

    CONCATENATE p_path '/' gs_file-name INTO gv_logical_filename.
    CONDENSE gv_logical_filename NO-GAPS.

    WRITE: / 'Checking file:', gs_file-name, '| Mod. Date:', gv_file_date.

    IF gv_file_date < gv_cutoff_date.
      "This file is older than the cutoff date, so delete it.
      IF p_test = 'X'.
        WRITE: '==> [TEST] Would be DELETED.'.
      ELSE.
        DELETE DATASET gv_logical_filename.
        IF sy-subrc = 0.
          WRITE: '==> DELETED successfully.'.
        ELSE.
          WRITE: '==> ERROR: Deletion failed!'.
        ENDIF.
      ENDIF.
    ELSE.
      WRITE: '==> Kept (not old enough).'.
    ENDIF.
  ENDLOOP.

  ULINE.
  WRITE: / 'Processing finished at:', sy-datum, sy-uzeit.
