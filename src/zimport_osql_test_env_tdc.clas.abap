CLASS zimport_osql_test_env_tdc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS activate_osql_test_double
      IMPORTING
        !tdc_name               TYPE etobj_name
        !tdc_version            TYPE etobj_ver OPTIONAL
        !tdc_variant            TYPE etvar_id
      RETURNING
        VALUE(test_environment) TYPE REF TO if_osql_test_environment
      RAISING
        zcx_import_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_tdc
      IMPORTING
        tdc_name      TYPE etobj_name
        tdc_version   TYPE etobj_ver
      RETURNING
        VALUE(result) TYPE REF TO cl_apl_ecatt_tdc_api
      RAISING
        zcx_import_error.

    CLASS-METHODS get_tdc_parameter_name
      IMPORTING
        table         TYPE zexport_table_list
      RETURNING
        VALUE(result) TYPE etp_name.

    CLASS-METHODS get_value
      IMPORTING
        table_conjunction TYPE zexport_table_list
        tdc               TYPE REF TO cl_apl_ecatt_tdc_api
        variant           TYPE etvar_id
      EXPORTING
        content           TYPE REF TO data
      RAISING
        cx_ecatt_tdc_access.
ENDCLASS.



CLASS ZIMPORT_OSQL_TEST_ENV_TDC IMPLEMENTATION.


  METHOD activate_osql_test_double.
    DATA: table_list TYPE STANDARD TABLE OF zexport_table_list,
          content    TYPE REF TO data.
    FIELD-SYMBOLS: <content> TYPE STANDARD TABLE.

    TRY.
        DATA(tdc) = get_tdc( tdc_name = tdc_name tdc_version = tdc_version ).
        tdc->get_value(
          EXPORTING
            i_param_name = 'ZEXPORT_TABLE_LIST'
            i_variant_name = tdc_variant
          CHANGING
            e_param_value = table_list ).
        IF table_list IS INITIAL.
          RETURN.
        ENDIF.

        test_environment = cl_osql_test_environment=>create(
          VALUE #( FOR <table> IN table_list ( <table>-source_table ) ) ).
        test_environment->clear_doubles( ).

        LOOP AT table_list REFERENCE INTO DATA(table_conjunction).

          CREATE DATA content TYPE STANDARD TABLE OF (table_conjunction->*-source_table).
          get_value(
            EXPORTING
              table_conjunction = table_conjunction->*
              tdc = tdc
              variant = tdc_variant
            IMPORTING
              content = content ).
          ASSIGN content->* TO <content>.
          test_environment->insert_test_data( <content> ).

        ENDLOOP.
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tdc.

    TRY.
        result = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = tdc_name
          i_testdatacontainer_version = tdc_version ).
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tdc_parameter_name.

    " backwards-compatibility: until commit
    " 550688c21ca119ffaecd4e1c11a68ab504fc53ee
    " tdc-parameter-name was the fake-table name.
    " So 'table-tdc_parameter_name' can be empty.
    IF table-tdc_parameter_name IS INITIAL.
      result = table-fake_table.
    ELSE.
      result = table-tdc_parameter_name.
    ENDIF.

  ENDMETHOD.


  METHOD get_value.
    DATA: content_db_view TYPE REF TO data.
    FIELD-SYMBOLS: <content>         TYPE STANDARD TABLE,
                   <content_db_view> TYPE STANDARD TABLE.

    ASSIGN content->* TO <content>.

    IF zexport_utils=>is_cds_view_entity( table_conjunction-source_table ) = abap_true.
      CREATE DATA content_db_view TYPE STANDARD TABLE OF (table_conjunction-fake_table).
      ASSIGN content_db_view->* TO <content_db_view>.
      tdc->get_value( EXPORTING i_param_name = get_tdc_parameter_name( table_conjunction )
        i_variant_name = variant
        CHANGING e_param_value = <content_db_view> ).
      ##ENH_OK
      MOVE-CORRESPONDING <content_db_view> TO <content>.
    ELSE.
      tdc->get_value( EXPORTING i_param_name = get_tdc_parameter_name( table_conjunction )
        i_variant_name = variant
        CHANGING e_param_value = <content> ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
