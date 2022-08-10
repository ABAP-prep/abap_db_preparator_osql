CONSTANTS: testcase_id TYPE w3objid VALUE 'ZBUNDLE_UNIT_TEST'.

CLASS test_import DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.

    METHODS setup
      RAISING
        zcx_export_error.

    METHODS setup_mime.

    METHODS setup_tables.

    METHODS export
      RAISING
        zcx_export_error.

    METHODS clear_tables.

    METHODS activate_osql_test_environment FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_import IMPLEMENTATION.

  METHOD setup.

    setup_mime( ).
    setup_tables( ).
    export( ).
    COMMIT WORK AND WAIT.

    clear_tables( ).

  ENDMETHOD.

  METHOD setup_mime.
    DATA: mime_key TYPE wwwdatatab.

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key    = mime_key
      EXCEPTIONS
        OTHERS = 0.

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2,
          export_ut3 TYPE zexport_ut3,
          import_ut1 TYPE zimport_ut1,
          import_ut2 TYPE zimport_ut2.

    " setup the tables in this package
    DELETE FROM: zexport_ut1, zimport_ut1,
      zexport_ut2, zexport_ut3, zimport_ut2.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).
    export_ut3 = VALUE #( primary_key = 'ADA' content = '9999' ).
    import_ut1 = VALUE #( primary_key = 'CCC' content = 'imp' ).
    ##LITERAL
    import_ut2 = VALUE #( primary_key = 'CCC' content = '30' ).

    INSERT zexport_ut1 FROM @export_ut1.
    INSERT zexport_ut2 FROM @export_ut2.

    INSERT zimport_ut1 FROM @import_ut1.
    INSERT zimport_ut2 FROM @import_ut2.

    INSERT zexport_ut3 FROM @export_ut3.

  ENDMETHOD.

  METHOD export.
    DATA: dev_package TYPE devclass.

    SELECT SINGLE devclass FROM tadir INTO @dev_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = 'ZIMPORT_BUNDLE_FROM_CLUSTER'.

    DATA(exporter) = NEW zexport_bundle_in_cluster( testcase_id = testcase_id
      dev_package = dev_package title = 'Unit-Test ABAP DB preparator' ).

    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->export( ).

  ENDMETHOD.

  METHOD clear_tables.

    DELETE FROM: zexport_ut1, zexport_ut2.

  ENDMETHOD.

  METHOD activate_osql_test_environment.
    DATA: exp_export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
          exp_export_ut2 TYPE STANDARD TABLE OF zexport_ut2,
          exp_export_ut3 TYPE STANDARD TABLE OF zexport_ut3.

    " when
    DATA(test_environment) = zimport_osql_test_env_cluster=>activate_osql_test_double(
      testcase_id = testcase_id ).

    SELECT * FROM zexport_ut1 INTO TABLE @DATA(act_export_ut1).
    SELECT * FROM zexport_ut2 INTO TABLE @DATA(act_export_ut2).
    SELECT * FROM zexport_ut3 INTO TABLE @DATA(act_export_ut3).

    test_environment->destroy( ).

    " then
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' ) ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' ) ).
    exp_export_ut3 = VALUE #(
      ( client = sy-mandt primary_key = 'ADA' content = '9999' ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut1
      act = act_export_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut2
      act = act_export_ut2 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut3
      act = act_export_ut3 ).

  ENDMETHOD.

ENDCLASS.
