REPORT z_sic_find_applog_real.

PARAMETERS: p_user TYPE syuname DEFAULT sy-uname,
            p_max  TYPE i       DEFAULT 30.

DATA: lt_hdr TYPE STANDARD TABLE OF balhdr,
      ls_hdr TYPE balhdr,
      lv_idx TYPE i.

SELECT *
  FROM balhdr
  WHERE aluser = @p_user
  ORDER BY aldate DESCENDING,
           altime DESCENDING,
           lognumber DESCENDING
  INTO TABLE @lt_hdr.

IF lt_hdr IS INITIAL.
  WRITE: / 'No BALHDR entries found for user:', p_user.
  EXIT.
ENDIF.

WRITE: / 'Idx', 6 'LOGNUMBER', 10 'OBJECT', 15 'SUBOBJECT', 22 'ALPROG', 35 'ALDATE', 45 'ALTIME', 55 'ALTEXT'.
ULINE.

lv_idx = 0.

LOOP AT lt_hdr INTO ls_hdr.
  lv_idx = lv_idx + 1.
  IF lv_idx > p_max.
    EXIT.
  ENDIF.

  WRITE: / lv_idx,
           6 ls_hdr-lognumber,
           10 ls_hdr-object,
           15 ls_hdr-subobject,
           22 ls_hdr-alprog,
           35 ls_hdr-aldate,
           45 ls_hdr-altime,
           55 ls_hdr-altext.
ENDLOOP.
