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

    "! Load TAR file
    METHODS load
      IMPORTING
        !iv_tar TYPE xstring
      EXCEPTIONS
        zcx_tar_error.

    "! Create TAR file
    METHODS save
      RETURNING
        VALUE(rv_tar) TYPE xstring
      EXCEPTIONS
        zcx_tar_error.

    "! Read file from TAR
    METHODS get
      IMPORTING
        !iv_name          TYPE string
      RETURNING
        VALUE(rv_content) TYPE xstring
      EXCEPTIONS
        zcx_tar_error.

    "! Add filed to TAR
    METHODS add
      IMPORTING
        !iv_name    TYPE string
        !iv_content TYPE xsequence
        !iv_date    TYPE d OPTIONAL
        !iv_time    TYPE t OPTIONAL
      EXCEPTIONS
        zcx_tar_error.

    "! Delete file from TAR
    METHODS delete
      IMPORTING
        !iv_name TYPE string
      EXCEPTIONS
        zcx_tar_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_header,
        name     TYPE x LENGTH 100,
        mode     TYPE x LENGTH 8,
        uid      TYPE x LENGTH 8,
        gid      TYPE x LENGTH 8,
        size     TYPE x LENGTH 12,
        mtime    TYPE x LENGTH 12,
        chksum   TYPE x LENGTH 8,
        typeflag TYPE x LENGTH 1,
        linkname TYPE x LENGTH 100,
        magic    TYPE x LENGTH 6,
        version  TYPE x LENGTH 2,
        uname    TYPE x LENGTH 32,
        gname    TYPE x LENGTH 32,
        devmajor TYPE x LENGTH 8,
        devminor TYPE x LENGTH 8,
        prefix   TYPE x LENGTH 155,
        padding  TYPE x LENGTH 12,
      END OF ty_header.

    TYPES:
      BEGIN OF ty_file,
        name    TYPE string,
        date    TYPE d,
        time    TYPE t,
        size    TYPE i,
        content TYPE xstring,
      END OF ty_file.
    TYPES:
      ty_files TYPE STANDARD TABLE OF ty_file WITH KEY name.

    CONSTANTS:
      c_blocksize TYPE i VALUE 512.

    CONSTANTS:
      BEGIN OF c_typeflag,
        file      TYPE ty_header-typeflag VALUE '30',
        directory TYPE ty_header-typeflag VALUE '35',
      END OF c_typeflag.

    DATA:
      mt_files TYPE ty_files.

    METHODS pad
      IMPORTING
        iv_number        TYPE numeric
        iv_length        TYPE i
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS octal
      IMPORTING
        iv_number        TYPE numeric
      RETURNING
        VALUE(rv_result) TYPE string.

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
    INSERT ls_file INTO TABLE mt_files.

  ENDMETHOD.


  METHOD delete.

  ENDMETHOD.


  METHOD get.

  ENDMETHOD.


  METHOD load.

  ENDMETHOD.


  METHOD octal.

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


  METHOD pad.

    rv_result = |{ octal( iv_number ) ALIGN = RIGHT PAD = '0' WIDTH = iv_length }|.

  ENDMETHOD.


  METHOD save.

  ENDMETHOD.
ENDCLASS.
