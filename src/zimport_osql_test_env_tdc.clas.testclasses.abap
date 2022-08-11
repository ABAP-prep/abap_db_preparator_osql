CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZEXPORT_UNIT_TEST'.

CLASS test_env_tdc DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.

    METHODS setup
      RAISING
        cx_static_check.

    METHODS create_tdc
      RAISING
        cx_static_check.

    METHODS setup_tables.

    METHODS export_in_ecattdefault_variant
      RAISING
        cx_static_check.

    METHODS clear_tables.

    METHODS activate_test_environment FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_env_tdc IMPLEMENTATION.

  METHOD setup.

    create_tdc( ).
    setup_tables( ).
    export_in_ecattdefault_variant( ).
    COMMIT WORK AND WAIT.
    clear_tables( ).

  ENDMETHOD.

  METHOD create_tdc.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

    cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING
      i_name = tdc_name i_tadir_devclass = '$TMP' i_write_access = abap_true
      IMPORTING e_tdc_ref = DATA(tdc_accessor) ).
    tdc_accessor->commit_changes( i_release_lock = abap_true
      i_commit_mode = abap_false ).

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2,
          export_ut3 TYPE zexport_ut3.

    " setup the tables in this package
    DELETE FROM: zexport_ut1, zexport_ut2, zexport_ut3.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).
    export_ut3 = VALUE #( primary_key = 'ADA' content = '9999' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.
    INSERT zexport_ut3 FROM export_ut3.

  ENDMETHOD.

  METHOD clear_tables.

    DELETE FROM: zexport_ut1, zexport_ut2.

  ENDMETHOD.

  METHOD export_in_ecattdefault_variant.

    DATA(tdc_accessor) = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = tdc_name
      i_testdatacontainer_version = 1 i_write_access = abap_true ).
    DATA(exporter) = NEW zexport_bundle_in_tdc( tdc = tdc_accessor
      variant = 'ECATTDEFAULT' ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'DEMO_CDS_AGGREGATE' ) ).
    exporter->export( transport_request = space ).

  ENDMETHOD.

  METHOD activate_test_environment.
    DATA: exp_export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
          exp_export_ut2 TYPE STANDARD TABLE OF zexport_ut2,
          exp_export_ut3 TYPE STANDARD TABLE OF zexport_ut3,
          exp_aggregate  TYPE STANDARD TABLE OF demo_cds_aggregate.

    " when
    DATA(test_environment) = zimport_osql_test_env_tdc=>activate_osql_test_double(
      tdc_name = tdc_name tdc_version = 1 tdc_variant = 'ECATTDEFAULT' ).

    SELECT * FROM zexport_ut1 INTO TABLE @DATA(act_export_ut1).
    SELECT * FROM zexport_ut2 INTO TABLE @DATA(act_export_ut2).
    SELECT * FROM zexport_ut3 INTO TABLE @DATA(act_export_ut3).
    SELECT * FROM demo_cds_aggregate INTO TABLE @DATA(act_aggregate).

    test_environment->destroy( ).

    " then
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' ) ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' ) ).
    exp_export_ut3 = VALUE #(
      ( client = sy-mandt primary_key = 'ADA' content = '9999' ) ).
    SELECT carrid, connid, sum( fltime ) as sum_fltime, sum( distance ) as sum_distance
      FROM spfli
      GROUP BY carrid, connid ORDER BY carrid, connid
      INTO CORRESPONDING FIELDS OF TABLE @exp_aggregate.
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut1
      act = act_export_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut2
      act = act_export_ut2 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut3
      act = act_export_ut3 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_aggregate
      act = act_aggregate ).

  ENDMETHOD.

ENDCLASS.
