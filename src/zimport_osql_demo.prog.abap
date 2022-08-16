*&---------------------------------------------------------------------*
*& Report ZIMPORT_OSQL_DEMO
*&---------------------------------------------------------------------*
*& Demo-program for ABAP DB preparator. The test-class reads
*& the data-export from the test-data-container ZEXPORT_DEMO
*& and from the cluster ZEXPORT_DEMO and mocks the Open SQL
*& SELECT-Statement "SELECT * FROM scarr".
*& Data-Export is done with program ZEXPORT_GUI.
*&---------------------------------------------------------------------*
REPORT ZIMPORT_OSQL_DEMO.

CLASS demo_osql_environment_tdc DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS mock_osql_call FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS demo_osql_environment_tdc IMPLEMENTATION.

  METHOD mock_osql_call.
    DATA: exp_aggregate TYPE STANDARD TABLE OF demo_cds_aggregate.

    DATA(test_environment) = zimport_osql_test_env_tdc=>activate_osql_test_double(
      tdc_name = 'ZEXPORT_DEMO' tdc_version = 1 tdc_variant = 'ECATTDEFAULT' ).

    SELECT * FROM scarr INTO TABLE @DATA(act_airlines).
    SELECT * FROM demo_cds_aggregate INTO TABLE @DATA(act_aggregate).
    test_environment->clear_doubles( ).

    DATA(exp_airlines) = VALUE ty_scarr(
      ( mandt = sy-mandt carrid = 'AA' carrname = 'American Airlines'
        currcode = 'USD' url = 'http://www.aa.com' ) ).
    exp_aggregate = VALUE #(
      ( carrid = 'AA' sum_fltime = 361 connid = 0017
        sum_distance = 2572 ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_airlines
      act = act_airlines ).
    cl_abap_unit_assert=>assert_equals( exp = exp_aggregate
      act = act_aggregate ).

  ENDMETHOD.

ENDCLASS.

CLASS demo_osql_environment_cls DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS mock_osql_call FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS demo_osql_environment_cls IMPLEMENTATION.

  METHOD mock_osql_call.
    DATA: exp_aggregate TYPE STANDARD TABLE OF demo_cds_aggregate.

    DATA(test_environment) = zimport_osql_test_env_cluster=>activate_osql_test_double(
      testcase_id = 'ZEXPORT_DEMO' ).

    SELECT * FROM scarr INTO TABLE @DATA(act_airlines).
    SELECT * FROM demo_cds_aggregate INTO TABLE @DATA(act_aggregate).
    test_environment->clear_doubles( ).

    DATA(exp_airlines) = VALUE ty_scarr(
      ( mandt = sy-mandt carrid = 'AA' carrname = 'American Airlines'
        currcode = 'USD' url = 'http://www.aa.com' ) ).
    exp_aggregate = VALUE #(
      ( carrid = 'AA' sum_fltime = 361 connid = 0017
        sum_distance = 2572 ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_airlines
      act = act_airlines ).
    cl_abap_unit_assert=>assert_equals( exp = exp_aggregate
      act = act_aggregate ).

  ENDMETHOD.

ENDCLASS.
