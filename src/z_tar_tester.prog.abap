REPORT z_tar_tester.

************************************************************************
* ABAP Tar
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tar
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_tar TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_files DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS open_dialog
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS upload
      IMPORTING
        !iv_path         TYPE string
      RETURNING
        VALUE(rv_result) TYPE xstring.

ENDCLASS.
CLASS lcl_files IMPLEMENTATION.

  METHOD open_dialog.

    DATA:
      lt_file_table TYPE filetable,
      ls_file_table LIKE LINE OF lt_file_table,
      lv_filter     TYPE string,
      lv_action     TYPE i,
      lv_rc         TYPE i.

    lv_filter = 'TAR Files (*.tar)|*.tar|' && cl_gui_frontend_services=>filetype_all.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Select TAR File'
        file_filter             = lv_filter
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      MESSAGE 'File open dialog error' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      MESSAGE 'Cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.

    rv_result = ls_file_table-filename.

  ENDMETHOD.

  METHOD upload.

    TYPES:
      ty_hex TYPE x LENGTH 2048.

    DATA:
      lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
      lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      MESSAGE 'File upload error' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_result IN BYTE MODE.
    rv_result = rv_result(lv_length).

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tar.

  p_tar = lcl_files=>open_dialog( ).

START-OF-SELECTION.

  DATA:
    lv_data  TYPE xstring,
    lo_tar   TYPE REF TO zcl_tar,
    lt_files TYPE zcl_tar=>ty_files.

  lv_data = lcl_files=>upload( p_tar ).

  CREATE OBJECT lo_tar.

  lo_tar->load( lv_data ).

  lt_files = lo_tar->get_all( ).

  BREAK-POINT.
