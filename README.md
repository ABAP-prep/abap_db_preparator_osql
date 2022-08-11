# ABAP Database preparator
This version supports the OSQL-Test-Double-Framework.

## How it works ##
Bundles are stored either in an ECATT test data container or in a cluster (binary MIME-object in transaction smw0) with program `zexport_gui`. The program `zexport_gui` is located in the [ABAP Database preparator repository](https://github.com/ABAP-prep/abap_db_preparator)
With this repository the bundles can be used as mock data for the OSQL-Test-Double-Framework. The program `zimport_osql_demo` demonstrate this.

## Terms ##

* Bundle: a collection of database records, which are stored outside
  of the database tables.

## Restrictions ##
The mocks can only be created once per testclass with the methods `zimport_osql_test_env_tdc=>activate_osql_test_double` or `zimport_osql_test_env_cluster=>activate_osql_test_double`.

## Dependencies
- [ABAP Database preparator](https://github.com/ABAP-prep/abap_db_preparator)

## Cloning this repository ##
Cloning can be done with [abapGit](https://github.com/larshp/abapgit). SAP Netweaver 7.52 or higher is needed.
