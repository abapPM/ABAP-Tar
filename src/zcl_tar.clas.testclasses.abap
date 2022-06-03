CLASS ltcl_tar_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_tar.

    METHODS:
      setup,
      octal FOR TESTING,
      pad FOR TESTING.

ENDCLASS.

CLASS zcl_tar DEFINITION LOCAL FRIENDS ltcl_tar_tests.

CLASS ltcl_tar_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD octal.

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->octal( 0 )
      exp =  '0' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->octal( 143 )
      exp =  '217' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->octal( 4565 )
      exp =  '10725' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->octal( 498112 )
      exp =  '1714700' ).

  ENDMETHOD.

  METHOD pad.

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->pad( iv_number = 0 iv_length = 4 )
      exp =  '0000' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->pad( iv_number = 143 iv_length = 8 )
      exp =  '00000217' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->pad( iv_number = 4565 iv_length = 8 )
      exp =  '00010725' ).

    cl_abap_unit_assert=>assert_equals(
      act =  mo_cut->pad( iv_number = 498112 iv_length = 12 )
      exp =  '000001714700' ).

  ENDMETHOD.

ENDCLASS.
