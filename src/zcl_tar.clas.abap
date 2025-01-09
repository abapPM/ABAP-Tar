CLASS zcl_tar DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* Tar
*
* Tar UStar and Pax Formats
*
* Based on
* https://en.wikipedia.org/wiki/Tar_(computing)
* https://en.wikipedia.org/wiki/Gzip
* https://en.wikipedia.org/wiki/Pax_(command)
* https://pubs.opengroup.org/onlinepubs/009695399/utilities/pax.html
*
* Note: Supports reading 7-zip tar files with long links but does not
* support writing such files.
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Limitation: Block size is hardcoded to 512 bytes
************************************************************************

  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '2.0.0' ##NEEDED.

    TYPES:
      ty_typeflag TYPE c LENGTH 1,
      BEGIN OF ty_keyword,
        keyword TYPE string,
        value   TYPE string,
      END OF ty_keyword,
      ty_keywords TYPE STANDARD TABLE OF ty_keyword WITH KEY keyword,
      BEGIN OF ty_file,
        name     TYPE string,
        date     TYPE d,
        time     TYPE t,
        mode     TYPE i,
        unixtime TYPE i,
        size     TYPE i,
        typeflag TYPE ty_typeflag,
        content  TYPE xstring,
        keywords TYPE ty_keywords,
      END OF ty_file,
      ty_tar_files TYPE STANDARD TABLE OF ty_file WITH KEY name.

    CONSTANTS:
      BEGIN OF c_typeflag,
        file              TYPE ty_typeflag VALUE '0',
        hard_link         TYPE ty_typeflag VALUE '1',
        symbolic_link     TYPE ty_typeflag VALUE '2',
        character_special TYPE ty_typeflag VALUE '3',
        block_special     TYPE ty_typeflag VALUE '4',
        directory         TYPE ty_typeflag VALUE '5',
        fifo              TYPE ty_typeflag VALUE '6',
        contiguous_file   TYPE ty_typeflag VALUE '7',
        long_link         TYPE ty_typeflag VALUE 'L', " 7-zip
        global_header     TYPE ty_typeflag VALUE 'g', " pax
        extended_header   TYPE ty_typeflag VALUE 'x', " pax
      END OF c_typeflag.

    CLASS-METHODS class_constructor.

    "! Create archive
    CLASS-METHODS new
      IMPORTING
        !force_ustar  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_tar.

    METHODS constructor
      IMPORTING
        !force_ustar TYPE abap_bool.

    "! Load archive
    METHODS load
      IMPORTING
        !tar          TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zcl_tar
      RAISING
        zcx_error.

    "! Create archive
    METHODS save
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_error.

    "! Read file from archive
    METHODS get
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_error.

    "! List the table of contents of an archive (no data)
    METHODS list
      RETURNING
        VALUE(result) TYPE ty_tar_files
      RAISING
        zcx_error.

    "! Number of files in archive
    METHODS file_count
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_error.

    "! Total size of unpackage files in bytes
    METHODS unpacked_size
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_error.

    "! Append file to archive
    METHODS append
      IMPORTING
        !name         TYPE string
        !content      TYPE xsequence
        !date         TYPE d OPTIONAL
        !time         TYPE t OPTIONAL
        !mode         TYPE i OPTIONAL
        !typeflag     TYPE c OPTIONAL
        !keywords     TYPE ty_keywords OPTIONAL " pax
      RETURNING
        VALUE(result) TYPE REF TO zcl_tar
      RAISING
        zcx_error.

    "! Delete file from archive
    METHODS delete
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_tar
      RAISING
        zcx_error.

    "! Gzip archive
    METHODS gzip
      IMPORTING
        !tar          TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_error.

    "! Gunzip archive
    METHODS gunzip
      IMPORTING
        !gzip         TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! Ustar header record (512 bytes)
      BEGIN OF ty_header,
        name     TYPE c LENGTH 100, " Offset 0
        mode     TYPE c LENGTH 8,   " 100
        uid      TYPE c LENGTH 8,   " 108
        gid      TYPE c LENGTH 8,   " 116
        size     TYPE c LENGTH 12,  " 124
        mtime    TYPE c LENGTH 12,  " 136
        chksum   TYPE c LENGTH 8,   " 148
        typeflag TYPE c LENGTH 1,   " 156
        linkname TYPE c LENGTH 100, " 157
        magic    TYPE c LENGTH 6,   " 257
        version  TYPE c LENGTH 2,   " 263
        uname    TYPE c LENGTH 32,  " 265
        gname    TYPE c LENGTH 32,  " 297
        devmajor TYPE c LENGTH 8,   " 329
        devminor TYPE c LENGTH 8,   " 337
        prefix   TYPE c LENGTH 155, " 345
        padding  TYPE c LENGTH 12,  " 500
      END OF ty_header.

    TYPES:
      BEGIN OF ty_tar_item,
        name    TYPE string,
        content TYPE xstring,
      END OF ty_tar_item,
      ty_tar_data TYPE HASHED TABLE OF ty_tar_item WITH UNIQUE KEY name.

    CONSTANTS:
      c_blocksize     TYPE i VALUE 512,
      c_ustar_magic   TYPE c LENGTH 5 VALUE 'ustar',
      c_ustar_version TYPE c LENGTH 2 VALUE '00',
      c_mode_default  TYPE i VALUE 436, " octal 664 rw-rw-r--
      c_path_sep      TYPE c VALUE '/', " unix
      c_epoch         TYPE timestamp VALUE '19700101000000'.

    CLASS-DATA:
      null        TYPE c LENGTH 256,
      convert_in  TYPE REF TO cl_abap_conv_in_ce,
      convert_out TYPE REF TO cl_abap_conv_out_ce.

    DATA:
      force_ustar TYPE abap_bool,
      tar_files   TYPE ty_tar_files,
      tar_data    TYPE ty_tar_data.

    CLASS-METHODS _append_nulls
      CHANGING
        !data TYPE simple.

    CLASS-METHODS _remove_nulls
      CHANGING
        !data TYPE simple.

    CLASS-METHODS _pad
      IMPORTING
        !number       TYPE numeric
        !length       TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS _unpad
      IMPORTING
        !data         TYPE csequence
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS _from_octal
      IMPORTING
        !octal        TYPE string
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS _to_octal
      IMPORTING
        !number       TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS _from_xstring
      IMPORTING
        !data         TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_error.

    CLASS-METHODS _to_xstring
      IMPORTING
        !data         TYPE simple
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_error.

    CLASS-METHODS _from_filename
      IMPORTING
        !filename TYPE string
      EXPORTING
        !prefix   TYPE ty_header-prefix
        !name     TYPE ty_header-name
      RAISING
        zcx_error.

    CLASS-METHODS _to_filename
      IMPORTING
        !prefix       TYPE ty_header-prefix
        !name         TYPE ty_header-name
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS _from_unixtime
      IMPORTING
        !unixtime TYPE i
      EXPORTING
        !date     TYPE d
        !time     TYPE t
      RAISING
        zcx_error.

    CLASS-METHODS _to_unixtime
      IMPORTING
        !date         TYPE d
        !time         TYPE t
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_error.

    CLASS-METHODS _checksum
      IMPORTING
        VALUE(data)   TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_error.

ENDCLASS.



CLASS zcl_tar IMPLEMENTATION.


  METHOD append.

    DATA:
      file TYPE ty_file,
      item TYPE ty_tar_item.

    " TODO: Support long filenames (pax)
    IF strlen( name ) > 100.
      zcx_error=>raise( |Filename longer than 100 characters: { name }| ).
    ENDIF.

    " List
    file-name = name.
    file-size = xstrlen( content ).
    IF date IS INITIAL.
      file-date = sy-datum.
    ELSE.
      file-date = date.
    ENDIF.
    IF time IS INITIAL.
      file-time = sy-uzeit.
    ELSE.
      file-time = time.
    ENDIF.
    IF mode IS INITIAL.
      file-mode = c_mode_default.
    ELSE.
      file-mode = mode.
    ENDIF.
    IF typeflag IS INITIAL.
      file-typeflag = c_typeflag-file.
    ELSE.
      file-typeflag = typeflag.
    ENDIF.
    file-unixtime = _to_unixtime( date = file-date time = file-time ).
    file-keywords = keywords.

    INSERT file INTO TABLE tar_files.
    IF sy-subrc <> 0.
      zcx_error=>raise( 'Error adding file (list)' ).
    ENDIF.

    " Data
    item-name    = name.
    item-content = content.
    INSERT item INTO TABLE tar_data.
    IF sy-subrc <> 0.
      zcx_error=>raise( 'Error adding file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD class_constructor.

    " Generate a char 256 null
    DATA x TYPE x LENGTH 4 VALUE '00000000'.

    FIELD-SYMBOLS <c> TYPE c.

    ASSIGN x TO <c> CASTING ##SUBRC_OK.

    null = <c>.
    DO 8 TIMES.
      null = null && null.
    ENDDO.

    " Initialize converters
    convert_in  = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.


  METHOD constructor.

    me->force_ustar = force_ustar.

  ENDMETHOD.


  METHOD delete.

    DELETE tar_files WHERE name = name.
    IF sy-subrc <> 0.
      zcx_error=>raise( 'Error deleting file (list)' ).
    ENDIF.

    DELETE tar_data WHERE name = name.
    IF sy-subrc <> 0.
      zcx_error=>raise( 'Error deleting file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD file_count.

    LOOP AT tar_files ASSIGNING FIELD-SYMBOL(<file>) WHERE typeflag = c_typeflag-file.
      result += 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD get.

    READ TABLE tar_data ASSIGNING FIELD-SYMBOL(<item>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <item>-content.
    ELSE.
      zcx_error=>raise( 'Error getting file' ).
    ENDIF.

  ENDMETHOD.


  METHOD gunzip.

    cl_abap_gzip=>decompress_binary_with_header(
      EXPORTING
        gzip_in = gzip
      IMPORTING
        raw_out = result ).

  ENDMETHOD.


  METHOD gzip.

    cl_abap_gzip=>compress_binary_with_header(
      EXPORTING
        raw_in   = tar
      IMPORTING
        gzip_out = result ).

  ENDMETHOD.


  METHOD list.

    result = tar_files.

  ENDMETHOD.


  METHOD load.

    DATA:
      header   TYPE ty_header,
      global   TYPE ty_keywords,
      extended TYPE ty_keywords,
      longlink TYPE string,
      file     TYPE ty_file,
      item     TYPE ty_tar_item,
      block    TYPE xstring,
      count    TYPE i,
      length   TYPE i,
      offset   TYPE i.

    DATA(size) = xstrlen( tar ).

    IF size = 0 OR size MOD c_blocksize <> 0.
      zcx_error=>raise( 'Error loading file (blocksize)' ).
    ENDIF.

    CLEAR tar_files.

    DO.
      IF offset + c_blocksize > size.
        EXIT.
      ENDIF.

      " Header block
      block = tar+offset(c_blocksize).
      offset = offset + c_blocksize.

      header = _from_xstring( block ).

      _remove_nulls( CHANGING data = header ).

      IF header IS INITIAL.
        CONTINUE.
      ENDIF.

      " Get extended header for keywords and filename
      CASE header-typeflag.
        WHEN c_typeflag-global_header.
          global = lcl_pax=>decode_keywords( block ).
          CONTINUE.
        WHEN c_typeflag-extended_header.
          extended = lcl_pax=>decode_keywords( block ).
          CONTINUE.
        WHEN c_typeflag-long_link.
          " Two blocks
          DATA(next_block) = tar+offset(c_blocksize).
          longlink = lcl_7zip=>decode_longlink(
            block_1 = block
            block_2 = next_block ).
          offset = offset + c_blocksize.
          CONTINUE.
      ENDCASE.

      IF force_ustar = abap_true.
        IF header-magic <> c_ustar_magic.
          zcx_error=>raise( 'Error loading file (ustar)' ).
        ELSEIF header-version <> c_ustar_version AND header-version <> ` `.
          zcx_error=>raise( 'Error loading file (version)' ).
        ENDIF.
      ENDIF.

      CLEAR file.
      file-name     = _to_filename( prefix = header-prefix name = header-name ).
      file-size     = _unpad( header-size ).
      file-mode     = _unpad( header-mode ).
      file-unixtime = _unpad( header-mtime ).

      _from_unixtime(
        EXPORTING
          unixtime = file-unixtime
        IMPORTING
          date     = file-date
          time     = file-time ).

      IF header-typeflag IS INITIAL.
        file-typeflag = c_typeflag-file.
      ELSE.
        file-typeflag = header-typeflag.
      ENDIF.

      file-keywords = lcl_pax=>merge_keywords(
        global   = global
        extended = extended ).

      " Long filename
      IF longlink IS NOT INITIAL.
        " 7-zip
        file-name = longlink.
        CLEAR longlink.
      ELSE.
        " Pax
        READ TABLE file-keywords ASSIGNING FIELD-SYMBOL(<keyword>)
          WITH TABLE KEY keyword = 'path'.
        IF sy-subrc = 0.
          file-name = <keyword>-value.
        ENDIF.
      ENDIF.

      INSERT file INTO TABLE tar_files.

      CLEAR extended.

      " Data blocks
      CLEAR item.
      item-name = file-name.

      length = file-size.
      count  = ( file-size - 1 ) DIV c_blocksize + 1.

      DO count TIMES.
        IF length > c_blocksize.
          block = tar+offset(c_blocksize).
        ELSE.
          block = tar+offset(length).
        ENDIF.
        item-content = item-content && block.
        offset = offset + c_blocksize.
        length = length - c_blocksize.
      ENDDO.

      INSERT item INTO TABLE tar_data.
    ENDDO.

    result = me.

  ENDMETHOD.


  METHOD new.

    CREATE OBJECT result
      EXPORTING
        force_ustar = force_ustar.

  ENDMETHOD.


  METHOD save.

    DATA:
      header   TYPE ty_header,
      extended TYPE ty_keywords,
      block    TYPE x LENGTH c_blocksize,
      count    TYPE i,
      length   TYPE i,
      offset   TYPE i.

    " TODO?: Support other types
    LOOP AT tar_files ASSIGNING FIELD-SYMBOL(<file>)
      WHERE typeflag = c_typeflag-file OR typeflag = c_typeflag-directory.

      IF strlen( <file>-name ) > 255.
        zcx_error=>raise( 'Error saving file (name)' ).
      ELSEIF <file>-name CA '\'.
        zcx_error=>raise( 'Error saving file (path)' ).
      ENDIF.

      " Add extended header block for pax keywords
      IF <file>-keywords IS NOT INITIAL.
        DATA(pax) = abap_true.
        result = result && lcl_pax=>encode_keywords( <file>-keywords ).
        CONTINUE.
      ENDIF.

      " Header block
      CLEAR header.

      _from_filename(
        EXPORTING
          filename = <file>-name
        IMPORTING
          prefix   = header-prefix
          name     = header-name ).

      header-mode     = _pad( number = <file>-mode length = 7 ).
      header-uid      = ''.
      header-gid      = ''.
      header-size     = _pad( number = <file>-size length = 11 ).
      header-mtime    = _pad( number = <file>-unixtime length = 11 ).
      header-typeflag = <file>-typeflag.
      header-magic    = c_ustar_magic.
      header-version  = c_ustar_version.
      header-uname    = to_lower( cl_abap_syst=>get_user_name( ) ).
      header-gname    = ''.
      header-linkname = ''.
      header-devminor = ''.
      header-devmajor = ''.
      header-padding  = ''.

      _append_nulls( CHANGING data = header ).

      header-chksum = `        `. " 8 spaces
      header-chksum = _pad( number = _checksum( header ) length = 7 ) && null.

      block = _to_xstring( header ).
      result   = result && block.

      " Data blocks
      READ TABLE tar_data ASSIGNING FIELD-SYMBOL(<item>) WITH TABLE KEY name = <file>-name.
      IF sy-subrc <> 0.
        zcx_error=>raise( 'Error saving file (data)' ).
      ENDIF.

      offset = 0.
      length = <file>-size.
      count  = ( length - 1 ) DIV c_blocksize + 1.

      DO count TIMES.
        IF length > c_blocksize.
          block = <item>-content+offset(c_blocksize).
        ELSE.
          block = <item>-content+offset(length).
        ENDIF.
        result    = result && block.
        offset = offset + c_blocksize.
        length = length - c_blocksize.
      ENDDO.

    ENDLOOP.

    IF pax = abap_true.
      " Add two null blocks
      result = result && block && block.
    ENDIF.

  ENDMETHOD.


  METHOD unpacked_size.

    LOOP AT tar_files ASSIGNING FIELD-SYMBOL(<file>) WHERE typeflag = c_typeflag-file.
      result += <file>-size.
    ENDLOOP.

  ENDMETHOD.


  METHOD _append_nulls.

    DATA(count) = 0.
    DO.
      count = count + 1.
      ASSIGN COMPONENT count OF STRUCTURE data TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <field> = <field> && null.
    ENDDO.

  ENDMETHOD.


  METHOD _checksum.

    DATA(xstring) = _to_xstring( data ).
    DATA(i) = 0.

    DO xstrlen( xstring ) TIMES.
      DATA(x) = xstring+i(1).
      result = result + x.
      i += 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_filename.

    DATA:
      temp_name   TYPE string,
      temp_prefix TYPE string.

    temp_name = filename.
    DO.
      IF strlen( temp_name ) <= 100.
        name = temp_name.
        EXIT.
      ENDIF.

      " Shorten name by moving part of path to prefix
      SPLIT temp_name AT c_path_sep INTO temp_prefix temp_name.
      IF sy-subrc <> 0.
        zcx_error=>raise( 'Error file name too long' ).
      ENDIF.

      IF prefix IS INITIAL.
        prefix = temp_prefix.
      ELSE.
        prefix = prefix && c_path_sep && temp_prefix.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD _from_octal.

    DATA(offset) = 0.

    DO strlen( octal ) TIMES.
      result = result * 8 + octal+offset(1).
      offset += 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_unixtime.

    TRY.
        DATA(timestamp) = cl_abap_tstmp=>add(
          tstmp = c_epoch
          secs  = unixtime ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_error=>raise( 'Error converting from UNIX time' ).
    ENDTRY.

    CONVERT TIME STAMP timestamp TIME ZONE 'UTC' INTO DATE date TIME time.

  ENDMETHOD.


  METHOD _from_xstring.

    TRY.
        convert_in->convert(
          EXPORTING
            input = data
            n     = xstrlen( data )
          IMPORTING
            data  = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_error=>raise( 'Error converting from xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _pad.

    result = |{ _to_octal( number ) ALIGN = RIGHT PAD = '0' WIDTH = length }|.

  ENDMETHOD.


  METHOD _remove_nulls.

    DATA(count) = 0.

    DO.
      count = count + 1.
      ASSIGN COMPONENT count OF STRUCTURE data TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      REPLACE ALL OCCURRENCES OF null(1) IN <field> WITH ''.
    ENDDO.

  ENDMETHOD.


  METHOD _to_filename.

    IF prefix IS INITIAL.
      result = name.
    ELSE.
      result = prefix && c_path_sep && name.
    ENDIF.

  ENDMETHOD.


  METHOD _to_octal.

    DATA(temp_number) = CONV i( number ).

    WHILE temp_number > 0.
      result      = |{ temp_number MOD 8 }{ result }|.
      temp_number = temp_number DIV 8.
    ENDWHILE.

    IF result IS INITIAL.
      result = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD _to_unixtime.

    DATA timestamp TYPE timestamp.

    CONVERT DATE date TIME time INTO TIME STAMP timestamp TIME ZONE 'UTC'.

    TRY.
        result = cl_abap_tstmp=>subtract(
          tstmp1 = timestamp
          tstmp2 = c_epoch ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_error=>raise( 'Error converting to UNIX time' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _to_xstring.

    DATA(string_data) = CONV string( data ).

    TRY.
        convert_out->convert(
          EXPORTING
            data   = string_data
          IMPORTING
            buffer = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_error=>raise( 'Error converting to xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _unpad.

    DATA(temp_data) = CONV string( data ).

    temp_data = replace(
      val  = temp_data
      sub  = ` `
      with = ''
      occ  = 0 ).

    result = _from_octal( condense( data ) ).

  ENDMETHOD.
ENDCLASS.
