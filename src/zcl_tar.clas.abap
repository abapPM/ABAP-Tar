CLASS zcl_tar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* ABAP Tar
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tar
*
* Tar UStar Format
* Based on https://en.wikipedia.org/wiki/Tar_(computing)
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      BEGIN OF ty_file,
        name     TYPE string,
        date     TYPE d,
        time     TYPE t,
        mode     TYPE i,
        unixtime TYPE i,
        size     TYPE i,
        content  TYPE xstring,
      END OF ty_file,

      ty_files TYPE STANDARD TABLE OF ty_file WITH KEY name.

    "! Load TAR file
    METHODS load
      IMPORTING
        !iv_tar TYPE xstring
      RAISING
        zcx_tar_error.

    "! Create TAR file
    METHODS save
      RETURNING
        VALUE(rv_tar) TYPE xstring
      RAISING
        zcx_tar_error.

    "! Read file from TAR
    METHODS get
      IMPORTING
        !iv_name          TYPE string
      RETURNING
        VALUE(rv_content) TYPE xstring
      RAISING
        zcx_tar_error.

    "! Get all files from TAR
    METHODS get_all
      RETURNING
        VALUE(rt_result) TYPE ty_files
      RAISING
        zcx_tar_error.

    "! Add filed to TAR
    METHODS add
      IMPORTING
        !iv_name    TYPE string
        !iv_content TYPE xsequence
        !iv_date    TYPE d OPTIONAL
        !iv_time    TYPE t OPTIONAL
        !iv_mode    TYPE string OPTIONAL
      RAISING
        zcx_tar_error.

    "! Delete file from TAR
    METHODS delete
      IMPORTING
        !iv_name TYPE string
      RAISING
        zcx_tar_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_null TYPE c LENGTH 1,

      BEGIN OF ty_header,
        name     TYPE c LENGTH 100,
        mode     TYPE c LENGTH 8,
        uid      TYPE c LENGTH 8,
        gid      TYPE c LENGTH 8,
        size     TYPE c LENGTH 12,
        mtime    TYPE c LENGTH 12,
        chksum   TYPE c LENGTH 8,
        typeflag TYPE c LENGTH 1,
        linkname TYPE c LENGTH 100,
        magic    TYPE c LENGTH 6,
        version  TYPE c LENGTH 2,
        uname    TYPE c LENGTH 32,
        gname    TYPE c LENGTH 32,
        devmajor TYPE c LENGTH 8,
        devminor TYPE c LENGTH 8,
        prefix   TYPE c LENGTH 155,
        padding  TYPE c LENGTH 12,
      END OF ty_header.

    CONSTANTS:
      c_blocksize TYPE i VALUE 512,

      BEGIN OF c_typeflag,
        file      TYPE ty_header-typeflag VALUE '0',
        directory TYPE ty_header-typeflag VALUE '5',
      END OF c_typeflag,

      c_epoch TYPE timestamp VALUE '19700101000000'.

    CLASS-DATA:
      go_convert_in  TYPE REF TO cl_abap_conv_in_ce,
      go_convert_out TYPE REF TO cl_abap_conv_out_ce.

    DATA:
      mt_files TYPE ty_files.

    METHODS _null
      RETURNING
        VALUE(rv_result) TYPE ty_null.

    METHODS _pad
      IMPORTING
        !iv_number       TYPE numeric
        !iv_length       TYPE i
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _unpad
      IMPORTING
        !iv_data         TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE i.

    METHODS _from_octal
      IMPORTING
        !iv_octal        TYPE string
      RETURNING
        VALUE(rv_result) TYPE i.

    METHODS _to_octal
      IMPORTING
        !iv_number       TYPE numeric
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _from_xstring
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _to_xstring
      IMPORTING
        !ig_data         TYPE any
      RETURNING
        VALUE(rv_result) TYPE xstring.

    METHODS _from_unixtime
      IMPORTING
        !iv_unixtime TYPE i
      EXPORTING
        !ev_date     TYPE d
        !ev_time     TYPE t.

    METHODS _to_unixtime
      IMPORTING
        !iv_date         TYPE d
        !iv_time         TYPE t
      RETURNING
        VALUE(rv_result) TYPE i.

    METHODS _checksum
      IMPORTING
        VALUE(ig_data)   TYPE any
      RETURNING
        VALUE(rv_result) TYPE i.

ENDCLASS.



CLASS zcl_tar IMPLEMENTATION.


  METHOD add.

    DATA ls_file TYPE ty_file.

    ls_file-name    = iv_name.
    ls_file-content = iv_content.
    ls_file-size    = xstrlen( iv_content ).
    IF iv_date IS INITIAL.
      ls_file-date = sy-datum.
    ELSE.
      ls_file-date = iv_date.
    ENDIF.
    IF iv_time IS INITIAL.
      ls_file-time = sy-uzeit.
    ELSE.
      ls_file-time = iv_time.
    ENDIF.
    IF iv_mode IS INITIAL.
      ls_file-mode = _from_octal( '664' ).  " rw-rw-r--
    ELSE.
      ls_file-mode = _from_octal( iv_mode ).
    ENDIF.
    ls_file-unixtime = _to_unixtime( iv_date = ls_file-date iv_time = ls_file-time ).

    INSERT ls_file INTO TABLE mt_files.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_tar_error.
    ENDIF.

  ENDMETHOD.


  METHOD delete.

    DELETE mt_files WHERE name = iv_name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_tar_error.
    ENDIF.

  ENDMETHOD.


  METHOD get.

    FIELD-SYMBOLS <ls_file> TYPE ty_file.

    READ TABLE mt_files ASSIGNING <ls_file> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      rv_content = <ls_file>-content.
    ELSE.
      RAISE EXCEPTION TYPE zcx_tar_error.
    ENDIF.

  ENDMETHOD.


  METHOD get_all.
    rt_result = mt_files.
  ENDMETHOD.


  METHOD load.

    DATA:
      ls_header TYPE ty_header,
      lv_block  TYPE xstring,
      lv_count  TYPE i,
      lv_length TYPE i,
      lv_offset TYPE i.

    FIELD-SYMBOLS <ls_file> TYPE ty_file.

    lv_length = xstrlen( iv_tar ).

    IF lv_length MOD c_blocksize <> 0.
      RAISE EXCEPTION TYPE zcx_tar_error.
    ENDIF.

    lv_count = lv_length DIV c_blocksize.

    CLEAR mt_files.

    DO lv_count TIMES.

      " Header block
      lv_block = iv_tar+lv_offset(c_blocksize).
      lv_offset = lv_offset + c_blocksize.

      ls_header = _from_xstring( lv_block ).

      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_file>.
      <ls_file>-name     = ls_header-name.
      <ls_file>-size     = _unpad( ls_header-size ).
      <ls_file>-mode     = _unpad( ls_header-mode ).
      <ls_file>-unixtime = _unpad( ls_header-mtime ).

      _from_unixtime(
        EXPORTING
          iv_unixtime = _unpad( ls_header-mtime )
        IMPORTING
          ev_date     = <ls_file>-date
          ev_time     = <ls_file>-time ).

      " Data blocks
      lv_length = <ls_file>-size.
      lv_count  = ( <ls_file>-size DIV c_blocksize ) + 1.

      DO lv_count TIMES.
        IF lv_length > c_blocksize.
          lv_block = iv_tar+lv_offset(c_blocksize).
        ELSE.
          lv_block = iv_tar+lv_offset(lv_length).
        ENDIF.
        <ls_file>-content = <ls_file>-content && lv_block.
        lv_offset = lv_offset + c_blocksize.
        lv_length = lv_length - c_blocksize.
      ENDDO.

    ENDDO.

  ENDMETHOD.


  METHOD save.

    DATA:
      ls_header TYPE ty_header,
      lv_block  TYPE x LENGTH c_blocksize,
      lv_count  TYPE i,
      lv_length TYPE i,
      lv_offset TYPE i.

    FIELD-SYMBOLS <ls_file> TYPE ty_file.

    LOOP AT mt_files ASSIGNING <ls_file>.

      " Header block
      CLEAR ls_header.
      ls_header-name     = <ls_file>-name ##TODO.       " Filename > 100 chars
      ls_header-mode     = _pad( iv_number = <ls_file>-mode iv_length = 7 ).
      ls_header-uid      = _pad( iv_number = 0 iv_length = 7 ).
      ls_header-gid      = _pad( iv_number = 0 iv_length = 7 ).
      ls_header-size     = _pad( iv_number = <ls_file>-size iv_length = 11 ).
      ls_header-mtime    = _pad( iv_number = <ls_file>-unixtime iv_length = 11 ).
      ls_header-chksum   = `        `. " 8 spaces
      ls_header-typeflag = c_typeflag-file.
      ls_header-magic    = 'ustar'.
      ls_header-version  = '00'.
      ls_header-uname    = cl_abap_syst=>get_user_name( ).
      ls_header-chksum   = _pad( iv_number = _checksum( ls_header ) iv_length = 7 ).

      lv_block = _to_xstring( ls_header ).
      rv_tar   = rv_tar && lv_block.

      " Data blocks
      lv_offset = 0.
      lv_length = <ls_file>-size.
      lv_count  = ( lv_length DIV c_blocksize ) + 1.

      DO lv_count TIMES.
        IF lv_length > c_blocksize.
          lv_block = <ls_file>-content+lv_offset(c_blocksize).
        ELSE.
          lv_block = <ls_file>-content+lv_offset(lv_length).
        ENDIF.
        rv_tar    = rv_tar && lv_block.
        lv_offset = lv_offset + c_blocksize.
        lv_length = lv_length - c_blocksize.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD _checksum.

    DATA:
      lv_data TYPE xstring,
      lv_i    TYPE i,
      lv_x    TYPE x LENGTH 1.

    lv_data = _to_xstring( ig_data ).

    DO xstrlen( lv_data ) TIMES.
      lv_x = lv_data+lv_i(1).
      rv_result = rv_result + lv_x.
      lv_i = lv_i + 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_octal.

    DATA lv_offset TYPE i.

    DO strlen( iv_octal ) TIMES.
      rv_result = rv_result * 8 + iv_octal+lv_offset(1).
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_unixtime.

    DATA lv_timestamp TYPE timestampl.

    lv_timestamp = cl_abap_tstmp=>add(
      tstmp = c_epoch
      secs  = iv_unixtime ).

    CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC' INTO DATE ev_date TIME ev_time.

  ENDMETHOD.


  METHOD _from_xstring.

    TRY.
        IF go_convert_in IS INITIAL.
          go_convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_in->convert(
          EXPORTING
            input = iv_data
            n     = xstrlen( iv_data )
          IMPORTING
            data  = rv_result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _null.

    " must be length 4, or it gives a syntax error on lower versions
    DATA lv_x TYPE x LENGTH 4 VALUE '00000000'.

    FIELD-SYMBOLS <lv_y> TYPE c.

    ASSIGN lv_x TO <lv_y> CASTING ##SUBRC_OK.

    rv_result = <lv_y>.

  ENDMETHOD.


  METHOD _pad.

    rv_result = |{ _to_octal( iv_number ) ALIGN = RIGHT PAD = '0' WIDTH = iv_length }|.

  ENDMETHOD.


  METHOD _to_octal.

    DATA lv_number TYPE i.

    lv_number = iv_number.

    WHILE lv_number > 0.
      rv_result = |{ lv_number MOD 8 }{ rv_result }|.
      lv_number = lv_number DIV 8.
    ENDWHILE.

    IF rv_result IS INITIAL.
      rv_result = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD _to_unixtime.

    DATA lv_timestamp TYPE timestamp.

    CONVERT DATE iv_date TIME iv_time INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.

    rv_result = cl_abap_tstmp=>subtract(
      tstmp1 = lv_timestamp
      tstmp2 = c_epoch ).

  ENDMETHOD.


  METHOD _to_xstring.

    TRY.
        IF go_convert_out IS INITIAL.
          go_convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_out->convert(
          EXPORTING
            data   = ig_data
          IMPORTING
            buffer = rv_result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _unpad.

    DATA lv_data TYPE string.

    lv_data = iv_data.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_data WITH ''.
    REPLACE ALL OCCURRENCES OF _null( ) IN lv_data WITH ''.

    rv_result = _from_octal( condense( lv_data ) ).

  ENDMETHOD.
ENDCLASS.
