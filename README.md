![Version](https://img.shields.io/endpoint?url=https://shield.abappm.com/github/abapPM/ABAP-Tar/src/zcl_tar.clas.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/ABAP-Tar?label=License&&color=success)](https://github.com/abapPM/ABAP-Tar/blob/main/LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?&color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/ABAP-Tar)](https://api.reuse.software/info/github.com/abapPM/ABAP-Tar)

# Tar for ABAP

This is an implementation of the [ustar](https://en.wikipedia.org/wiki/Tar_(computing)) and [pax](https://en.wikipedia.org/wiki/Pax_(command)) tar formats. 

Archives can be compressed and decompressed using [gzip and gunzip](https://en.wikipedia.org/wiki/Gzip) methods as well.

NO WARRANTIES, [MIT License](https://github.com/abapPM/ABAP-Tar/blob/main/LICENSE)

Limitations: Block size is hardcoded to 512 bytes

## Usage

### Tar Files

Here's an example of how to tar a list of files.

```abap
DATA your_files TYPE zcl_tar=>ty_files.

DATA(tar) = zcl_tar=>new( ).

LOOP AT files INTO ls_file.
  tar->append(
    name     = file-name
    content  = file-content
    date     = file-date
    time     = file-time
    mode     = file-mode
    typeflag = file-typeflag ).
ENDLOOP.

DATA(tar_data) = tar->save( ).

" Gzip
DATA(packed_data) = tar->gzip( tar_data ).
```

### Untar Files

Here's an example of how to untar a file.

```abap
DATA:
  your_data     TYPE xstring,
  unpacked_data TYPE xstring.

DATA(tar) = zcl_tar=>new( ).

" Gunzip
IF filename CP '*.tgz' OR filename CP '*.tar.gz'.
  unpacked_data = tar->gunzip( your_data ).
ELSE.
  unpacked_data = your_data.
ENDIF.

tar->load( unpacked_data ).
```

List the files and get file content:

```abap
DATA(files) = tar->list( ).

LOOP AT files INTO DATA(file).
  DATA(content)  = tar->get( file-name ).
  " ...
ENDLOOP.
```

You can use program `Z_TAR_TESTER` to test the methods:

- upload a tar file
- untar it
- tar it again
- save it as a clone of the original tar file

## Prerequisites

SAP Basis 7.50 or higher

## Installation

Install `tar` as a global module in your system using [apm](https://abappm.com).

or

Specify the `tar` module as a dependency in your project and import it to your namespace using [apm](https://abappm.com).

```abap
IMPORT '*' TO 'z$1_your_project$2' FROM 'tar'.
" or
IMPORT '*' TO '/namespace/$1$2' FROM 'tar'.
```

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/ABAP-Tar/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

You can install the developer version of ABAP Tar using [abapGit](https://github.com/abapGit/abapGit) either by creating a new online repository for `https://github.com/abapPM/ABAP-Tar`.

Recommended SAP package: `$TAR`

## About

Made with ❤️ in Canada

Copyright 2024 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Blueksy and [marcfbe](https://linkedin.com/in/marcfbe) or LinkedIn
