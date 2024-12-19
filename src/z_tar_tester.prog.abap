REPORT z_tar_tester.

************************************************************************
* Tar Tester
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_tar TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_files DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS open_dialog
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS upload
      IMPORTING
        !path         TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS download
      IMPORTING
        !path   TYPE string
        !buffer TYPE xstring.

  PRIVATE SECTION.

    TYPES ty_hex TYPE x LENGTH 2048.

    TYPES ty_data_table TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

ENDCLASS.

CLASS lcl_files IMPLEMENTATION.

  METHOD open_dialog.

    DATA:
      file_table TYPE filetable,
      file_item  LIKE LINE OF file_table,
      action     TYPE i,
      rc         TYPE i.

    DATA(filter) = 'TAR Files (*.tar)|*.tar|' && cl_gui_frontend_services=>filetype_all.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Select TAR File'
        file_filter             = filter
      CHANGING
        file_table              = file_table
        rc                      = rc
        user_action             = action
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
    IF action = cl_gui_frontend_services=>action_cancel.
      MESSAGE 'Cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    READ TABLE file_table INDEX 1 INTO file_item.
    ASSERT sy-subrc = 0.

    result = file_item-filename.

  ENDMETHOD.

  METHOD upload.

    DATA:
      data_table  TYPE ty_data_table,
      data_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = path
        filetype                = 'BIN'
      IMPORTING
        filelength              = data_length
      CHANGING
        data_tab                = data_table
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
      MESSAGE 'File load error' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CONCATENATE LINES OF data_table INTO result IN BYTE MODE.
    result = result(data_length).

  ENDMETHOD.

  METHOD download.

    DATA data_table TYPE ty_data_table.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = buffer
      TABLES
        binary_tab = data_table.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( buffer )
        filename                  = path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = data_table
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      MESSAGE 'File save error' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tar.

  p_tar = lcl_files=>open_dialog( ).

START-OF-SELECTION.

  DATA:
    data     TYPE xstring,
    unpacked TYPE xstring,
    packed   TYPE xstring,
    msg      TYPE string,
    files    TYPE zcl_tar=>ty_files,
    file     TYPE zcl_tar=>ty_file.

  " Upload archive
  data = lcl_files=>upload( p_tar ).

  " Load Test
  TRY.
      DATA(tar_in) = zcl_tar=>new( ).

      " Gunzip
      IF p_tar CP '*.tgz' OR p_tar CP '*.tar.gz'.
        unpacked = tar_in->gunzip( data ).
      ELSE.
        unpacked = data.
      ENDIF.

      tar_in->load( unpacked ).

      files = tar_in->list( ).

    CATCH zcx_error INTO DATA(error).
      msg = error->get_text( ).
      MESSAGE msg TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Save Test
  TRY.
      DATA(tar_out) = zcl_tar=>new( ).

      LOOP AT files INTO file.
        tar_out->append(
          name     = file-name
          content  = tar_in->get( file-name )
          date     = file-date
          time     = file-time
          mode     = file-mode
          typeflag = file-typeflag ).
      ENDLOOP.

      data = tar_out->save( ).

      " Gzip
      packed = tar_out->gzip( data ).

    CATCH zcx_error INTO error.
      msg = error->get_text( ).
      MESSAGE msg TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Download archive
  lcl_files=>download(
    path   = p_tar && '.copy.tar'
    buffer = data ).

  lcl_files=>download(
    path   = p_tar && '.copy.tgz'
    buffer = packed ).
