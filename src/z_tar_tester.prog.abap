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
        VALUE(rv_result) TYPE string.

    CLASS-METHODS upload
      IMPORTING
        !iv_path         TYPE string
      RETURNING
        VALUE(rv_result) TYPE xstring.

    CLASS-METHODS download
      IMPORTING
        !iv_path TYPE string
        !iv_data TYPE xstring.

  PRIVATE SECTION.

    TYPES ty_hex TYPE x LENGTH 2048.

    TYPES ty_data TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

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

    DATA:
      lt_data   TYPE ty_data,
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
      MESSAGE 'File load error' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_result IN BYTE MODE.
    rv_result = rv_result(lv_length).

  ENDMETHOD.

  METHOD download.

    DATA lt_data TYPE ty_data.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = iv_data
      TABLES
        binary_tab = lt_data.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_data )
        filename                  = iv_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_data
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
    gv_data     TYPE xstring,
    gv_unpacked TYPE xstring,
    gv_packed   TYPE xstring,
    gv_msg      TYPE string,
    go_tar_in   TYPE REF TO zcl_tar,
    go_tar_out  TYPE REF TO zcl_tar,
    gx_error    TYPE REF TO zcx_error,
    gt_files    TYPE zcl_tar=>ty_files,
    gs_file     TYPE zcl_tar=>ty_file.

  " Upload archive
  gv_data = lcl_files=>upload( p_tar ).

  " Load Test
  TRY.
      go_tar_in = zcl_tar=>new( ).

      IF p_tar CP '*.tgz'.
        data(obj) = zcl_abapgit_zlib=>decompress( gv_data ).
        "data(obj) = zcl_abapgit_git_pack=>decode( gv_data ).
        "gv_unpacked = go_tar_in->gunzip( gv_data ).
      ELSE.
        gv_unpacked = gv_data.
      ENDIF.

      go_tar_in->load( gv_unpacked ).

      gt_files = go_tar_in->list( ).

    CATCH zcx_error INTO gx_error.
      gv_msg = gx_error->get_text( ).
      MESSAGE gv_msg TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Save Test
  TRY.
      go_tar_out = zcl_tar=>new( ).

      LOOP AT gt_files INTO gs_file.
        go_tar_out->append(
          iv_name     = gs_file-name
          iv_content  = go_tar_in->get( gs_file-name )
          iv_date     = gs_file-date
          iv_time     = gs_file-time
          iv_mode     = gs_file-mode
          iv_typeflag = gs_file-typeflag ).
      ENDLOOP.

      gv_data = go_tar_out->save( ).

    CATCH zcx_error INTO gx_error.
      gv_msg = gx_error->get_text( ).
      MESSAGE gv_msg TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Download archive
  lcl_files=>download(
    iv_path = p_tar && '.copy.tar'
    iv_data = gv_data ).

  " Gzip
  gv_packed = go_tar_in->gzip( gv_data ).

  lcl_files=>download(
    iv_path = p_tar && '.copy.tgz'
    iv_data = gv_packed ).
