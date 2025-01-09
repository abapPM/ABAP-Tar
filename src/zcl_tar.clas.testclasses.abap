CLASS ltcl_tar_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

* TODO: Add tests for pax format

  PRIVATE SECTION.

    DATA:
      cut          TYPE REF TO zcl_tar,
      tar_data     TYPE xstring,
      tgz_data     TYPE xstring,
      index_js     TYPE xstring,
      package_json TYPE xstring,
      tar_files    TYPE zcl_tar=>ty_tar_files.

    METHODS:
      setup,
      tar FOR TESTING RAISING zcx_error,
      untar FOR TESTING RAISING zcx_error,
      gzip FOR TESTING RAISING zcx_error,
      gunzip  FOR TESTING RAISING zcx_error.

ENDCLASS.

CLASS zcl_tar DEFINITION LOCAL FRIENDS ltcl_tar_tests.

CLASS ltcl_tar_tests IMPLEMENTATION.

  METHOD setup.
    cut = zcl_tar=>new( ).

    " Contains two files: index.js & package.json
    tar_data =
      '7061636B6167652F696E6465782E6A7300000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '3030303036343400000000000000000000000000000000003030303030303030323137003033353630313136363034003030' &&
      '3131343631003000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000075737461720030306265726E6172646D610000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '000000000000000000000000636F6E7374205F203D207265717569726528276C6F6461736827293B0D0A0D0A636F6E737420' &&
      '6E756D62657273203D205B33323432332C34322C32333432342C35333435335D3B0D0A0D0A5F2E65616368286E756D626572' &&
      '732C2066756E6374696F6E286E756D6265722C69297B0D0A20202020636F6E736F6C652E6C6F67286E756D626572293B0D0A' &&
      '7D293B0D0A000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000007061636B6167652F7061636B6167652E6A736F6E000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000003030303036343400000000000000000000000000000000003030' &&
      '3030303030313031360030333536303131363630340030303132333030003000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000075737461720030306265726E6172646D610000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000007B0A2020226E616D65223A20226D' &&
      '62746F6F6C732D6E706D2D64656D6F222C0A20202276657273696F6E223A2022302E302E31222C0A20202264657363726970' &&
      '74696F6E223A20224D79206669727374204E504D207061636B616765222C0A2020226D61696E223A2022696E6465782E6A73' &&
      '222C0A20202273637269707473223A207B0A202020202274657374223A20226563686F205C224572726F723A206E6F207465' &&
      '7374207370656369666965645C2220262620657869742031220A20207D2C0A2020227265706F7369746F7279223A207B0A20' &&
      '2020202274797065223A2022676974222C0A202020202275726C223A20226769742B68747470733A2F2F6769746875622E63' &&
      '6F6D2F6D62746F6F6C732F6E706D2D64656D6F2E676974220A20207D2C0A202022617574686F72223A20224D617263204265' &&
      '726E617264222C0A2020226C6963656E7365223A20224D4954222C0A20202262756773223A207B0A202020202275726C223A' &&
      '202268747470733A2F2F6769746875622E636F6D2F6D62746F6F6C732F6E706D2D64656D6F2F697373756573220A20207D2C' &&
      '0A202022686F6D6570616765223A202268747470733A2F2F6769746875622E636F6D2F6D62746F6F6C732F6E706D2D64656D' &&
      '6F23726561646D65222C0A202022646570656E64656E63696573223A207B0A20202020226C6F64617368223A20225E342E31' &&
      '372E3231220A20207D0A7D0A0000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000' &&
      '00000000000000000000'.

    " Contains two files: index.js & package.json
    tgz_data =
      '1F8B08000000000000FFED554D8FD33010EDB952FF8315A4FD10D9C44ED2AC54C405890387220EDC' &&
      '5858B9CEB431C476B01DB455D5FFBE76EC42B542620FEC82B4FB0E49F4E6E3792699494FD937BA81' &&
      '9CCB066EB2AF66F200C00E7555FD96F728C8E50497F31A1352D7B8723C21554D26F8210E731783B1' &&
      '543BC91568497523E86388FE3F604A1A8BAED16BA4E1FBC0359C9D76AAA1A63D3D7F359BCEA6C12E' &&
      '07E1FA639CD7A7B2A88A32AD8AB428ABA24AE765352F3F8FAED71950D69E45DF14AD07C92C573232' &&
      '293FDFCDA6C8C1E7541D649DDA449BD7DAFBCBBF6EC793431FE73FDEDD0650F26F6BFC61FE0926F5' &&
      'DDF92F4A8C9FE7FF11B0731399482A2059A044ACAC529DB990BDB86840A824F5D61F6E9ADD187B07' &&
      '9CE18C04B601C334EF6DB42CB768CDB55B15EF3F2C51FC9882A3A07CF438FC61021B828D33ECC69D' &&
      '905830D6BB016B15BA4ADE6AADF4024985BC01991E185F7368AE12747282E0865B441217B91FB369' &&
      'E895E156E9ED51C26D3F16B5E1769474D4A0BBC8BC6CADEDCD22CFDD733BAC32A6441EABCF0FD567' &&
      '3EF2A7041D6CABF4582AD50CBD099F4B28A6E30CA419D596EF3E066E356C8EAA8BCAF750CDB93103' &&
      '985FC2AD12D0FB6EDE2FFE8506DA0838BCA41E5CDB25E3707498B0E07DBE2F55462EB3227472BA7F' &&
      '5EBF4F0EB7F072D764000A0000'.

    " With CRLF
    DATA(index_js_txt) =
      |const _ = require('lodash');\r\n| &&
      |\r\n| &&
      |const numbers = [32423,42,23424,53453];\r\n| &&
      |\r\n| &&
      |_.each(numbers, function(number,i)\{\r\n| &&
      |    console.log(number);\r\n| &&
      |\});\r\n|.

    " With LF only
    DATA(package_json_txt) =
      |\{\n| &&
      |  "name": "mbtools-npm-demo",\n| &&
      |  "version": "0.0.1",\n| &&
      |  "description": "My first NPM package",\n| &&
      |  "main": "index.js",\n| &&
      |  "scripts": \{\n| &&
      |    "test": "echo \\"Error: no test specified\\" && exit 1"\n| &&
      |  \},\n| &&
      |  "repository": \{\n| &&
      |    "type": "git",\n| &&
      |    "url": "git+https://github.com/mbtools/npm-demo.git"\n| &&
      |  \},\n| &&
      |  "author": "Marc Bernard",\n| &&
      |  "license": "MIT",\n| &&
      |  "bugs": \{\n| &&
      |    "url": "https://github.com/mbtools/npm-demo/issues"\n| &&
      |  \},\n| &&
      |  "homepage": "https://github.com/mbtools/npm-demo#readme",\n| &&
      |  "dependencies": \{\n| &&
      |    "lodash": "^4.17.21"\n| &&
      |  \}\n| &&
      |\}\n|.

    index_js      = cl_binary_convert=>string_to_xstring_ascii( index_js_txt ).
    package_json  = cl_binary_convert=>string_to_xstring_ascii( package_json_txt ).

    tar_files = VALUE zcl_tar=>ty_tar_files(
      ( name = 'package/index.js' date = '19851026' time = '081500'
        mode = 420 unixtime = 499162500 size = 143 typeflag = '0' )
      ( name = 'package/package.json' date = '19851026' time = '081500'
        mode = 420 unixtime = 499162500 size = 526 typeflag = '0' ) ).

  ENDMETHOD.

  METHOD tar.

    DATA(tar_out) = zcl_tar=>new( ).

    LOOP AT tar_files INTO DATA(file).
      CASE sy-tabix.
        WHEN 1.
          tar_out->append(
            name     = file-name
            content  = index_js
            date     = file-date
            time     = file-time
            mode     = file-mode
            typeflag = file-typeflag ).
        WHEN 2.
          tar_out->append(
            name     = file-name
            content  = package_json
            date     = file-date
            time     = file-time
            mode     = file-mode
            typeflag = file-typeflag ).
      ENDCASE.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = tar_out->save( )
      exp = tar_data ).

  ENDMETHOD.

  METHOD untar.

    DATA(tar_in) = zcl_tar=>new( ).

    tar_in->load( tar_data ).

    DATA(files) = tar_in->list( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( files )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = files
      exp = tar_files ).

  ENDMETHOD.

  METHOD gzip.

    DATA(tar_out) = zcl_tar=>new( ).

    LOOP AT tar_files INTO DATA(file).
      CASE sy-tabix.
        WHEN 1.
          tar_out->append(
            name     = file-name
            content  = index_js
            date     = file-date
            time     = file-time
            mode     = file-mode
            typeflag = file-typeflag ).
        WHEN 2.
          tar_out->append(
            name     = file-name
            content  = package_json
            date     = file-date
            time     = file-time
            mode     = file-mode
            typeflag = file-typeflag ).
      ENDCASE.
    ENDLOOP.

    DATA(tar_data) = tar_out->save( ).

    cl_abap_unit_assert=>assert_equals(
      act = tar_out->gzip( tar_data )
      exp = tgz_data ).

  ENDMETHOD.

  METHOD gunzip.

    DATA(tar_in) = zcl_tar=>new( ).

    DATA(tar_data) = tar_in->gunzip( tgz_data ).

    tar_in->load( tar_data ).

    DATA(files) = tar_in->list( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( files )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = files
      exp = tar_files ).

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_tar_helpers DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      _null FOR TESTING,
      _filename FOR TESTING RAISING zcx_error,
      _checksum FOR TESTING RAISING zcx_error,
      _octal FOR TESTING,
      _pad FOR TESTING,
      _unixtime FOR TESTING RAISING zcx_error,
      _xstring FOR TESTING RAISING zcx_error.

ENDCLASS.

CLASS zcl_tar DEFINITION LOCAL FRIENDS ltcl_tar_helpers.

CLASS ltcl_tar_helpers IMPLEMENTATION.

  METHOD _null.

    DATA:
      null TYPE c LENGTH 1,
      BEGIN OF test_data,
        name TYPE c LENGTH 10,
        size TYPE c LENGTH 5,
        mode TYPE c LENGTH 8,
      END OF test_data.

    null = zcl_tar=>null(1).

    test_data-name = 'test.txt'.
    test_data-size = '12345'.
    test_data-mode = '01234'.

    zcl_tar=>_append_nulls( CHANGING data = test_data ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-name
      exp = 'test.txt' && null && null ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-size
      exp = '12345' ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-mode
      exp = '01234' && null && null && null ).

    zcl_tar=>_remove_nulls( CHANGING data = test_data ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-name
      exp = 'test.txt' ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-size
      exp = '12345' ).

    cl_abap_unit_assert=>assert_equals(
      act = test_data-mode
      exp = '01234' ).

  ENDMETHOD.

  METHOD _filename.

    DATA:
      filename TYPE string,
      prefix   TYPE zcl_tar=>ty_header-prefix,
      name     TYPE zcl_tar=>ty_header-name.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_filename( prefix = 'package/modules' name = 'tar.sh' )
      exp = 'package/modules/tar.sh' ).

    " Short filename
    zcl_tar=>_from_filename(
      EXPORTING
        filename = 'package/modules/tar.sh'
      IMPORTING
        prefix   = prefix
        name     = name ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = name
      exp = 'package/modules/tar.sh' ).

    " Long filename
    filename = 'package/node_modules/node-gyp/node_modules/path-array/' &&
      'node_modules/array-index/node_modules/es6-symbol/case-insensitive-compare.js'.

    zcl_tar=>_from_filename(
      EXPORTING
        filename = filename
      IMPORTING
        prefix   = prefix
        name     = name ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix
      exp = 'package/node_modules/node-gyp' ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix && '/' && name
      exp = filename ).

  ENDMETHOD.

  METHOD _checksum.

    CONSTANTS lc_data TYPE string VALUE 'abc 123'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_checksum( lc_data )
      exp = 476 ).

  ENDMETHOD.

  METHOD _octal.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_octal( 0 )
      exp = '0' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_octal( 143 )
      exp = '217' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_octal( 4565 )
      exp = '10725' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_octal( 498112 )
      exp = '1714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_from_octal( '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_from_octal( '217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_from_octal( '10725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_from_octal( '1714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD _pad.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_pad( number = 0 length = 4 )
      exp = '0000' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_pad( number = 143 length = 8 )
      exp = '00000217' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_pad( number = 4565 length = 8 )
      exp = '00010725' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_pad( number = 498112 length = 12 )
      exp = '000001714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_unpad( '0000' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_unpad( '00000217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_unpad( '00010725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_unpad( '000001714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD _unixtime.

    DATA:
      date TYPE d,
      time TYPE t.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_unixtime( date = '20221126' time = '123456' )
      exp = 1669466096 ).

    zcl_tar=>_from_unixtime(
      EXPORTING
        unixtime = 1669466096
      IMPORTING
        date     = date
        time     = time ).

    cl_abap_unit_assert=>assert_equals(
      act = date
      exp = '20221126' ).

    cl_abap_unit_assert=>assert_equals(
      act = time
      exp = '123456' ).

  ENDMETHOD.

  METHOD _xstring.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_to_xstring( 'abc 123 -' )
      exp = '61626320313233202D' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_tar=>_from_xstring( '61626320313233202D' )
      exp = 'abc 123 -' ).

  ENDMETHOD.
ENDCLASS.
