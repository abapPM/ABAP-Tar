![Version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/abapPM/ABAP-Tar/src/zcl_tar.clas.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/ABAP-Tar?label=License&color=green)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=green)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/ABAP-Tar)](https://api.reuse.software/info/github.com/abapPM/ABAP-Tar)

# Tar for ABAP

This is an implementation of the UStar Tar format as described on [Wikipedia](https://en.wikipedia.org/wiki/Tar_(computing)). 

Archives can be compressed and decompressed using [gzip and gunzip](https://en.wikipedia.org/wiki/Gzip) methods as well.

NO WARRANTIES, [MIT License](LICENSE)

## Usage

### Tar Files

Here's an example of how to tar a list of files.

```abap
  DATA:
    it_files    TYPE zcl_tar=>ty_files.

  DATA:
    lv_packed   TYPE xstring,
    lv_msg      TYPE string,
    lo_tar_out  TYPE REF TO zcl_tar,
    lx_error    TYPE REF TO zcx_error,
    ls_file     TYPE zcl_tar=>ty_file.
  
 TRY.
      lo_tar_out = zcl_tar=>new( ).

      LOOP AT it_files INTO ls_file.
        lo_tar_out->append(
          iv_name     = ls_file-name
          iv_content  = ls_file-content
          iv_date     = ls_file-date
          iv_time     = ls_file-time
          iv_mode     = ls_file-mode
          iv_typeflag = ls_file-typeflag ).
      ENDLOOP.

      lv_data = lo_tar_out->save( ).

      lv_packed = lo_tar_out->gzip( lv_data ).

    CATCH zcx_error INTO lx_error.
      lv_msg = lx_error->get_text( ).
      MESSAGE lv_msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
```

### Untar Files

Here's an example of how to untar a file.

```abap
  DATA:
    iv_filename TYPE string,
    iv_data     TYPE xstring.

  DATA:
    lv_unpacked TYPE xstring,
    lv_filedata TYPE xstring,
    lv_msg      TYPE string,
    lo_tar_in   TYPE REF TO zcl_tar,
    lx_error    TYPE REF TO zcx_error,
    ls_file     TYPE zcl_tar=>ty_file,
    lt_files    TYPE zcl_tar=>ty_files.
  
  TRY.
      lo_tar_in = zcl_tar=>new( ).

      IF iv_filename CP '*.tgz' OR iv_filename CP '*.tar.gz'.
        lv_unpacked = lo_tar_in->gunzip( iv_data ).
      ELSE.
        lv_unpacked = iv_data.
      ENDIF.

      lo_tar_in->load( lv_unpacked ).

      lt_files = lo_tar_in->list( ).

      LOOP AT lt_files INTO ls_file.
        lv_filedata = lo_tar_in->get( ls_file-name ).
        " ...
      ENDLOOP.

    CATCH zcx_error INTO lx_error.
      lv_msg = lx_error->get_text( ).
      MESSAGE lv_msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
```

## Prerequisites

SAP Basis 7.02 or higher

## Installation

Import ABAP Tar to your project using [apm](https://abappm.com).

```abap
IMPORT '*' TO 'z$1_your_project$2' FROM 'tar'.
" or
IMPORT '*' TO '/namespace/$1$2' FROM 'tar'.
```

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](CONTRIBUTING.md), fork this repo, and create a pull request.

You can install the developer version of ABAP Tar using [abapGit](https://github.com/abapGit/abapGit) either by creating a new online repository for https://github.com/abapPM/ABAP-Tar.

Recommended SAP package: `$TAR`

## About

Made with ❤️ in Canada

Copyright 2024 apm.to Inc. <https://apm.to>

Follow [@marcfbe](https://twitter.com/marcfbe) on X/Twitter
