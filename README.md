# ABAP Database preparator
This version supports the OSQL-Test-Double-Framework.

## How it works ##
Bundles are stored either in an ECATT test data container or in a cluster (binary MIME-object in transaction smw0).

### Export step ###
The export step is done with with program `zexport_gui`.
Let's assume we export the content of table `SCARR`.

### Import step ###
#### ECATT test data container ####
```abap
CLASS airline_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS mockup_osql_call FOR TESTING.

ENDCLASS.

CLASS airline_test IMPLEMENTATION.

  METHOD mockup_osql_call.

    DATA(test_environment) = zimport_osql_test_env_tdc=>activate_osql_test_double(
      tdc_name = 'ZMY_TDC_NAME' tdc_version = 1 tdc_variant = 'ECATTDEFAULT' ).

    SELECT * FROM scarr INTO TABLE @DATA(act_airlines).
    test_environment->destroy( ).

    DATA(exp_airlines) = VALUE ty_scarr(
      ( carrid = 'TG' carrname = 'Thai airways'
        currcode = 'THB' url = 'https://thaiairways.com' ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_airlines
      act = act_airlines ).

  ENDMETHOD.

ENDCLASS.
```

#### Cluster ####
```abap
CLASS airline_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS mockup_osql_call FOR TESTING.

ENDCLASS.

CLASS flight_test IMPLEMENTATION.

  METHOD mockup_osql_call.

     DATA(test_environment) = zimport_osql_test_env_cluster=>activate_osql_test_double(
       testcase_id = 'ZMY_ID' ).

     SELECT * FROM scarr INTO TABLE @DATA(act_airlines).
     test_environment->destroy( ).

     DATA(exp_airlines) = VALUE ty_scarr(
       ( carrid = 'TG' carrname = 'Thai airways'
         currcode = 'THB' url = 'https://thaiairways.com' ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_airlines
      act = act_airlines ).

ENDMETHOD.

ENDCLASS.
```

## Dependencies
- [ABAP Database preparator](https://github.com/ABAP-prep/abap_db_preparator)

## Cloning this repository ##
Cloning can be done with [abapGit](https://github.com/larshp/abapgit). SAP Netweaver 7.52 or higher is needed.
