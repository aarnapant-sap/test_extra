*"* use this source file for your ABAP unit test classes

*CLASS lcl_test DEFINITION DEFERRED.
*CLASS cl_abapgit_bg_action_pull DEFINITION LOCAL FRIENDS lcl_test.
*CLASS lcl_test DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  FRIENDS cl_abapgit_bg_action_pull.
*
*  PUBLIC SECTION.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    METHODS:
*      setup,
*      prepare_repo_mock,
*      prepare_log_mock,
*      exec_no_rep_no_pkg_ok FOR TESTING RAISING cx_static_check.
*
*    CLASS-DATA:
*      mt_repo_mock TYPE STANDARD TABLE OF abapgit WITH EMPTY KEY,
*      mt_log_mock  TYPE STANDARD TABLE OF ta4c_agit_applog WITH EMPTY KEY,
*      osql_env     TYPE REF TO if_osql_test_environment.
*    CLASS-DATA:
*      go_pull_mock TYPE REF TO cl_abapgit_bg_action_pull.
*
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*ENDCLASS.
*
*
*CLASS lcl_test IMPLEMENTATION.
*
*  METHOD setup.
*    osql_env->clear_doubles( ).
*
*    prepare_repo_mock( ).
*    prepare_log_mock( ).
*
*  ENDMETHOD.
*
*  METHOD class_setup.
*    go_pull_mock = NEW cl_abapgit_bg_action_pull( ).
*
*    osql_env = cl_osql_test_environment=>create(
*      i_dependency_list = VALUE #( ( 'ABAPGIT' ) ( 'TA4C_AGIT_APPLOG' ) ( 'BALHDR' ) ( 'BALM' ) ) ).
*  ENDMETHOD.
*
*  METHOD prepare_repo_mock.
*    mt_repo_mock = VALUE #( ( type = 'REPO' ) ).
*    osql_env->insert_test_data( mt_log_mock ).
*  ENDMETHOD.
*
*  METHOD prepare_log_mock.
*    mt_log_mock = VALUE #( ( app_log = '1' ) ).
*    osql_env->insert_test_data( mt_log_mock ).
*  ENDMETHOD.
*
*
*  METHOD exec_no_rep_no_pkg_ok.
*
*    DATA ls_param TYPE cl_abapgit_bg_action_pull=>ts_param.
*
*    ls_param-alog_key         = '1'.
*    ls_param-branch           = 'utest'.
*    ls_param-repo_key         = '900000000001'.
*    ls_param-transportrequest = 'TR1'.
*    ls_param-user             = 'UTESTUSR'.
*
*    go_pull_mock->if_abapgit_bg_action~set_param( REF #( ls_param ) ).
*    DATA(result) = go_pull_mock->if_abapgit_bg_action~get_paramstr( ).
*
*    go_pull_mock->if_abapgit_bg_action~execute( iv_paramstr = result ).
*
*    DATA(lo_logger) = cl_abapgit_app_log_factory=>get_instance( )->load_single( iv_app_log = '1' ).
*    lo_logger->get_app_log( )->error_reported( ).
*    cl_abap_unit_assert=>assert_equals( msg = 'Run was not successful'
*                                        act = lo_logger->get_app_log( )->error_reported( )
*                                        exp = abap_false ).
*
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    osql_env->destroy( ).
*  ENDMETHOD.
*
*ENDCLASS.

CLASS lcl_bg_action_utest DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS set_parameter FOR TESTING RAISING cx_static_check.
    METHODS set_paramstr FOR TESTING RAISING cx_static_check.
    METHODS get_param_type FOR TESTING RAISING cx_static_check.
    METHODS set_credentials FOR TESTING RAISING cx_static_check.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS setup RAISING cx_static_check.
    DATA mo_cut TYPE REF TO zcl_abapgit_action_push_v1.

    CONSTANTS cv_test_repo_key TYPE a4c_repo_key VALUE 'UTEST_REPO'.

ENDCLASS.

CLASS lcl_bg_action_utest IMPLEMENTATION.

  METHOD setup.
    mo_cut ?= cl_abapgit_bg_action_factory=>get_action_instance( iv_repo_key = cv_test_repo_key
                                                                 iv_action   = if_abapgit_app_log=>c_action_push ).
  ENDMETHOD.

  METHOD set_parameter.
    "get parameter type
    DATA(lv_type) = mo_cut->zif_abapgit_action_entity~get_param_type( ).
    cl_aunit_assert=>assert_not_initial( lv_type ).

    "create data for parameter type
    DATA ls_set_param TYPE REF TO data.
    CREATE DATA ls_set_param TYPE (lv_type).
    ASSIGN ls_set_param->* TO FIELD-SYMBOL(<ls_set_param>).

    ASSIGN COMPONENT 'STAGED_OBJECTS' OF STRUCTURE <ls_set_param> TO FIELD-SYMBOL(<lt_table>).
    <lt_table> = VALUE tta4c_abapgit_object( ( ) ).
    ASSIGN COMPONENT 'ABAPGIT_COMMENT' OF STRUCTURE <ls_set_param> TO FIELD-SYMBOL(<ls_struc>).
    <ls_struc> = VALUE tsa4c_abapgit_comment( committer = VALUE #( name = 'UTEST_USER' email = 'utest_user@sap.com' )
                                              author    = VALUE #( name = 'UTEST_AUTH' email = 'utest_auth@sap.com' )
                                              comment   = |This is a comment| ).

    mo_cut->zif_abapgit_action_entity~set_param( REF #( <ls_set_param> ) ).
    "cl_aunit_assert=>assert_equals( act = mo_cut->ms_param exp = <ls_param> ).

    "read parameter again and compare with set parameter
    mo_cut->zif_abapgit_action_entity~get_param( RECEIVING rr_param = DATA(lr_get_param) ).

    DATA ls_get_param TYPE REF TO data.
    CREATE DATA ls_get_param TYPE (lv_type).
    ASSIGN lr_get_param->* TO FIELD-SYMBOL(<ls_get_param>).

    cl_aunit_assert=>assert_equals( act = <ls_get_param> exp = <ls_set_param> ).

  ENDMETHOD.

  METHOD set_paramstr.

    DATA lv_set_paramstr TYPE string.
    lv_set_paramstr = lv_set_paramstr && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_set_paramstr = lv_set_paramstr && |<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">|.
    lv_set_paramstr = lv_set_paramstr && |  <asx:values>|.
    lv_set_paramstr = lv_set_paramstr && |    <PARAM>|.
    lv_set_paramstr = lv_set_paramstr && |      <STAGED_OBJECTS>|.
    lv_set_paramstr = lv_set_paramstr && |        <TSA4C_ABAPGIT_OBJECT>|.
    lv_set_paramstr = lv_set_paramstr && |          <OBJECT_REF>|.
    lv_set_paramstr = lv_set_paramstr && |            <URI/>|.
    lv_set_paramstr = lv_set_paramstr && |            <TYPE/>|.
    lv_set_paramstr = lv_set_paramstr && |            <NAME/>|.
    lv_set_paramstr = lv_set_paramstr && |            <PARENT_URI/>|.
    lv_set_paramstr = lv_set_paramstr && |            <PACKAGE_NAME/>|.
    lv_set_paramstr = lv_set_paramstr && |            <DESCRIPTION/>|.
    lv_set_paramstr = lv_set_paramstr && |          </OBJECT_REF>|.
    lv_set_paramstr = lv_set_paramstr && |          <FILES/>|.
    lv_set_paramstr = lv_set_paramstr && |        </TSA4C_ABAPGIT_OBJECT>|.
    lv_set_paramstr = lv_set_paramstr && |      </STAGED_OBJECTS>|.
    lv_set_paramstr = lv_set_paramstr && |      <ABAPGIT_COMMENT>|.
    lv_set_paramstr = lv_set_paramstr && |        <COMMITTER>|.
    lv_set_paramstr = lv_set_paramstr && |          <NAME>UTEST_USER</NAME>|.
    lv_set_paramstr = lv_set_paramstr && |          <EMAIL>utest_user@sap.com</EMAIL>|.
    lv_set_paramstr = lv_set_paramstr && |        </COMMITTER>|.
    lv_set_paramstr = lv_set_paramstr && |        <AUTHOR>|.
    lv_set_paramstr = lv_set_paramstr && |          <NAME>UTEST_AUTH</NAME>|.
    lv_set_paramstr = lv_set_paramstr && |          <EMAIL>utest_auth@sap.com</EMAIL>|.
    lv_set_paramstr = lv_set_paramstr && |        </AUTHOR>|.
    lv_set_paramstr = lv_set_paramstr && |        <COMMENT>This is a comment</COMMENT>|.
    lv_set_paramstr = lv_set_paramstr && |      </ABAPGIT_COMMENT>|.
    lv_set_paramstr = lv_set_paramstr && |    </PARAM>|.
    lv_set_paramstr = lv_set_paramstr && |  </asx:values>|.
    lv_set_paramstr = lv_set_paramstr && |</asx:abap> |.
    CALL TRANSFORMATION id SOURCE XML lv_set_paramstr RESULT XML lv_set_paramstr.

    "set parameter in string format
    mo_cut->zif_abapgit_action_entity~set_paramstr( iv_paramstr = lv_set_paramstr ).

    "read parameter again and compare with set parameter
    DATA(lv_get_paramstr) = mo_cut->zif_abapgit_action_entity~get_paramstr( ).
    CALL TRANSFORMATION id SOURCE XML lv_get_paramstr RESULT XML lv_get_paramstr.

    "compare set and get parameter string
    cl_aunit_assert=>assert_equals( act = lv_set_paramstr exp = lv_get_paramstr ).

  ENDMETHOD.

  METHOD get_param_type.
    DATA(lv_type) = mo_cut->zif_abapgit_action_entity~get_param_type( ).
    cl_aunit_assert=>assert_not_initial( lv_type ).

    "check if type can be used
    DATA ls_param TYPE REF TO data.
    FIELD-SYMBOLS <ls_param> TYPE any.
    CREATE DATA ls_param TYPE (lv_type).

  ENDMETHOD.

  METHOD set_credentials.

    DATA ls_credentials TYPE tsa4c_abapgit_credentials.
    ls_credentials-user     = 'UTEST_USR'.
    ls_credentials-password = 'Test134'.

    mo_cut->zif_abapgit_action_entity~set_credentials( ls_credentials ).
    cl_aunit_assert=>assert_equals( act = mo_cut->zif_abapgit_action_entity~get_credentials( ) exp = ls_credentials ).

  ENDMETHOD.

ENDCLASS.
