class ZIMPORT_OSQL_TEST_ENV_CLUSTER definition
  public
  final
  create public
  for testing .

public section.

  class-methods ACTIVATE_OSQL_TEST_DOUBLE
    importing
      !TESTCASE_ID type W3OBJID
    returning
      value(TEST_ENVIRONMENT) type ref to IF_OSQL_TEST_ENVIRONMENT
    raising
      ZCX_IMPORT_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS import_mime_object
      IMPORTING
        mime_key TYPE wwwdatatab
      RETURNING
        VALUE(result) TYPE abap_trans_srcbind_tab
      RAISING
        zcx_import_object_not_exists .
    CLASS-METHODS get_filesize
      IMPORTING
        mime_key TYPE wwwdatatab
      RETURNING
        VALUE(size) TYPE i .
    CLASS-METHODS deserialize
      IMPORTING
        !binary_content TYPE xstring
      RETURNING
        VALUE(result)   TYPE abap_trans_srcbind_tab.

ENDCLASS.



CLASS ZIMPORT_OSQL_TEST_ENV_CLUSTER IMPLEMENTATION.


  METHOD activate_osql_test_double.
    FIELD-SYMBOLS: <content> TYPE STANDARD TABLE.

    DATA(cluster_objects) = import_mime_object( VALUE #( relid = 'MI' objid = testcase_id ) ).
    IF cluster_objects IS INITIAL.
      RETURN.
    ENDIF.

    test_environment = cl_osql_test_environment=>create(
      VALUE #( FOR <object> IN cluster_objects ( CONV ddstrucobjname( <object>-name ) ) ) ).
    test_environment->clear_doubles( ).

    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <content>.
      test_environment->insert_test_data( <content> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize.
    DATA: binary_table_content TYPE xstring,
          table_list           TYPE STANDARD TABLE OF zexport_table_list.

    IMPORT content = binary_table_content table_list = table_list
      FROM DATA BUFFER binary_content.

    LOOP AT table_list REFERENCE INTO DATA(_table).
      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<object>).
      <object>-name = _table->*-fake_table.
      CREATE DATA <object>-value TYPE STANDARD TABLE OF (_table->*-fake_table).
    ENDLOOP.

    CALL TRANSFORMATION id
      SOURCE XML binary_table_content
      RESULT (result).

  ENDMETHOD.


  METHOD get_filesize.
    DATA: c_size TYPE wwwparams-value.

    SELECT SINGLE value FROM wwwparams INTO c_size
      WHERE relid = mime_key-relid AND objid = mime_key-objid
      AND name = 'filesize'.

    size = c_size.

  ENDMETHOD.


  METHOD import_mime_object.
    DATA: mime_content   TYPE STANDARD TABLE OF w3mime,
          binary_content TYPE xstring.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key          = mime_key
      TABLES
        mime         = mime_content
      EXCEPTIONS
        import_error = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_import_object_not_exists
        EXPORTING
          testcase_id = mime_key-objid.
    ENDIF.

    DATA(length) = get_filesize( mime_key ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = length
      IMPORTING
        buffer       = binary_content
      TABLES
        binary_tab   = mime_content.

    result = deserialize( binary_content ).

  ENDMETHOD.
ENDCLASS.
