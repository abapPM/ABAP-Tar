CLASS ltcl_tar_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_tar.

    METHODS:
      setup,
      checksum FOR TESTING,
      octal FOR TESTING,
      pad FOR TESTING,
      unixtime FOR TESTING,
      xstring FOR TESTING.

ENDCLASS.

CLASS zcl_tar DEFINITION LOCAL FRIENDS ltcl_tar_tests.

CLASS ltcl_tar_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD checksum.

    CONSTANTS lc_xdata TYPE xstring VALUE '61626320313233'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_checksum( lc_xdata )
      exp = 476 ).

  ENDMETHOD.

  METHOD octal.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 0 )
      exp = '0' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 143 )
      exp = '217' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 4565 )
      exp = '10725' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 498112 )
      exp = '1714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '10725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '1714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD pad.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 0 iv_length = 4 )
      exp = '0000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 143 iv_length = 8 )
      exp = '00000217' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 4565 iv_length = 8 )
      exp = '00010725' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 498112 iv_length = 12 )
      exp = '000001714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '0000' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '00000217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '00010725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '000001714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD unixtime.

    DATA:
      lv_date TYPE d,
      lv_time TYPE t.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_unixtime( iv_date = '20221126' iv_time = '123456' )
      exp = 1669466096 ).

    mo_cut->_from_unixtime(
      EXPORTING
        iv_unixtime = 1669466096
      IMPORTING
        ev_date     = lv_date
        ev_time     = lv_time ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_date
      exp = '20221126' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_time
      exp = '123456' ).

  ENDMETHOD.

  METHOD xstring.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_xstring( 'abc 123' )
      exp = '61626320313233' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_xstring( '61626320313233' )
      exp = 'abc 123' ).

  ENDMETHOD.

ENDCLASS.
