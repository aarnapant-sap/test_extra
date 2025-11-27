CLASS zcl_abapgit_action_pull_v1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

      TYPES:
      BEGIN OF ts_param,
        branch           TYPE string,
        transportrequest TYPE trkorr,
        objects_to_pull  TYPE cl_abapgit_res_pull_mod_objs=>ty_modified_objs,
      END OF ts_param .

    INTERFACES zif_abapgit_action_entity .
    aliases:
      get_param for zif_abapgit_action_entity~get_param,
      get_paramstr for zif_abapgit_action_entity~get_paramstr,
      execute for zif_abapgit_action_entity~execute,
      get_credentials for zif_abapgit_action_entity~get_credentials,
      get_param_type for zif_abapgit_action_entity~get_param_type,
      set_credentials for zif_abapgit_action_entity~set_credentials,
      set_param for zif_abapgit_action_entity~set_param,
      initialize for zif_abapgit_action_entity~initialize,
      cleanup for zif_abapgit_action_entity~cleanup,
      set_paramstr for zif_abapgit_action_entity~set_paramstr,
      set_repo_key for zif_abapgit_action_entity~set_repo_key.

  PROTECTED SECTION.
  PRIVATE SECTION.

   DATA mv_repo_key TYPE a4c_repo_key.
    DATA ms_param TYPE ts_param.
    DATA ms_credentials TYPE tsa4c_abapgit_credentials.
    CONSTANTS cv_action TYPE a4c_repo_action VALUE if_abapgit_app_log=>c_action_pull.

    CLASS-METHODS delete_unnecessary_objects
      IMPORTING
        !io_repo   TYPE REF TO cl_abapgit_repo
        !ii_log    TYPE REF TO if_abapgit_log
        !is_checks TYPE if_abapgit_definitions=>ty_deserialize_checks
      RAISING
        cx_abapgit_exception .

ENDCLASS.

CLASS zcl_abapgit_action_pull_v1 IMPLEMENTATION.


  METHOD execute.
    "validate input parameter
    CHECK ms_param IS NOT INITIAL.

    DATA lv_text TYPE c LENGTH 50.

    TRY.

        "get logger
        DATA(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).
        DATA(lo_log) = lo_log_factory->load_single( iv_app_log = iv_app_log ).

        "set operation running
        lo_log->set_run_status( if_abapgit_app_log=>c_run_status-running ).

        "set supplied repository credentials
        IF ms_credentials-user IS NOT INITIAL AND ms_credentials-password IS NOT INITIAL.
          cl_abapgit_default_auth_info=>refresh( ).
          cl_abapgit_default_auth_info=>set_auth_info(
             iv_user     = ms_credentials-user
             iv_password = ms_credentials-password ).
        ENDIF.

        "get online repo
        cl_abapgit_factory=>get_environment( )->set_repo_action( cv_action ).

        " Ensure Listener are added
        cl_abapgit_repo_srv=>get_instance( )->list( ).
        DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
        lo_repo->refresh( ).

        "set default transport request (if provided)
        IF ms_param-transportrequest IS NOT INITIAL.
          cl_abapgit_default_transport=>get_instance( )->set( ms_param-transportrequest ).
        ENDIF.

        DATA(ls_checks) = lo_repo->deserialize_checks( ).

        "set the default transport request
        IF ls_checks-transport-required = abap_true.
          ls_checks-transport-transport = ms_param-transportrequest.
        ENDIF.

        "pull the objects set in objects_to_pull parameter.
        DATA(ls_objects_to_pull) = ms_param-objects_to_pull.

        LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
          IF line_exists( ls_objects_to_pull-overwrite_objects[ object-name = <ls_overwrite>-obj_name ] ).
            DATA(overwrite_object_to_pull) = ls_objects_to_pull-overwrite_objects[ object-name = <ls_overwrite>-obj_name ].

            IF overwrite_object_to_pull-action <> <ls_overwrite>-action
              AND <ls_overwrite>-action = if_abapgit_objects=>c_deserialize_action-delete
                   OR <ls_overwrite>-action = if_abapgit_objects=>c_deserialize_action-delete_add.
              <ls_overwrite>-decision = 'N'.
            ELSE.
              <ls_overwrite>-decision = 'Y'.
            ENDIF.
          ELSE.
            <ls_overwrite>-decision = 'N'.
          ENDIF.
        ENDLOOP.

        LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
          IF line_exists( ls_objects_to_pull-package_warning_objects[ object-name = <ls_warning_package>-obj_name ] ).
            DATA(warning_package_object_to_pull) = ls_objects_to_pull-package_warning_objects[ object-name = <ls_warning_package>-obj_name ].

            IF warning_package_object_to_pull-action <> <ls_warning_package>-action
              AND <ls_warning_package>-action = if_abapgit_objects=>c_deserialize_action-delete
                   OR <ls_warning_package>-action = if_abapgit_objects=>c_deserialize_action-delete_add.
              <ls_warning_package>-decision = 'N'.
            ELSE.
              <ls_warning_package>-decision = 'Y'.
            ENDIF.
          ELSE.
            <ls_warning_package>-decision = 'N'.
          ENDIF.
        ENDLOOP.

        "pass decisions to delete
        delete_unnecessary_objects(
          io_repo   = lo_repo
          is_checks = ls_checks
          ii_log    = lo_log ).

        "pull objects
        lo_repo->deserialize( is_checks = ls_checks
                              ii_log    = lo_log ).

        "log final status
        DATA(lv_run_status) = lo_log->if_abapgit_log~get_status( ).
        CASE lv_run_status.
          WHEN if_abapgit_app_log=>c_run_status-success.
            lo_log->add_text( iv_text = 'Repository pulled successfully'  iv_type = 'S' ).
          WHEN if_abapgit_app_log=>c_run_status-warning.
            lo_log->add_text( iv_text = 'Repository pulled with warnings' iv_type = 'W' ).
          WHEN OTHERS. "no other value expected
            lo_log->add_text( iv_text = 'Repository pulled with error(s)' iv_type = 'E' ).
        ENDCASE.
        lo_log->set_run_status( lv_run_status ).

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.

        "log exception
        IF lo_log IS BOUND.
          lo_log->add_exception( lx_root ).
          lo_log->add_text( iv_text = 'Repository pull aborted' iv_type = 'A' ).
          lo_log->set_run_status( if_abapgit_app_log=>c_run_status-aborted ).
        ENDIF.
        cx_abapgit_exception=>raise( iv_text = 'Repository pull aborted' ix_previous = lx_root ).

    ENDTRY.
  ENDMETHOD.


  METHOD get_credentials.
    rs_credentials = ms_credentials.
  ENDMETHOD.


  METHOD get_param.
    FIELD-SYMBOLS <ls_export> TYPE any.
    TRY.
        CREATE DATA rr_param TYPE ts_param.
        ASSIGN rr_param->* TO <ls_export>.
        <ls_export> = ms_param.

      CATCH cx_sy_assign_error INTO DATA(lx_err).
        cx_abapgit_exception=>raise( iv_text     = |Invalid Parameter|
                                     ix_previous = lx_err ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_paramstr.
    CALL TRANSFORMATION id
      SOURCE param = ms_param
      RESULT XML rv_paramstr.
  ENDMETHOD.


  METHOD get_param_type.
    DATA lo_type TYPE REF TO cl_abap_structdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( ms_param ).
    rv_param_type = lo_type->absolute_name.
  ENDMETHOD.


  METHOD set_credentials.
    ms_credentials = is_credentials.
  ENDMETHOD.


  METHOD set_param.
    FIELD-SYMBOLS <ls_import> TYPE any.
    FIELD-SYMBOLS <ls_param>  TYPE any.
    DATA ls_param TYPE REF TO data.

    TRY.
        CREATE DATA ls_param TYPE ts_param.
        ASSIGN ls_param->* TO <ls_param>.

        ASSIGN ir_param->* TO <ls_import>.
        <ls_param> = <ls_import>.

        ms_param = <ls_param>.

      CATCH cx_sy_assign_error INTO DATA(lx_err).
        cx_abapgit_exception=>raise( iv_text     = |Invalid Parameter|
                                     ix_previous = lx_err ).
    ENDTRY.
  ENDMETHOD.

  method initialize.
    check ms_param is not initial.

    try.
        "Create new log in history table before starting batch processing
        DATA(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).

        DATA(lo_log) = lo_log_factory->create_new( iv_repo_key    = mv_repo_key
                                                   iv_repo_branch = ms_param-branch
                                                   iv_repo_action = cv_action ).
        lo_log->save( ).

        " acquire a lock on the app log entry




    CATCH cx_abapgit_not_found cx_abapgit_app_log INTO DATA(lx_err).
        lo_log->set_run_status( iv_status = 'A' ).
        cx_abapgit_exception=>raise( iv_text     = |Pull cannot be triggered|
                                     ix_previous = lx_err ).
    endtry.
  endmethod.

  method cleanup.
      data(lock_manager) = zcl_agit_lock_manager=>get_instance( ).
         lock_manager->remove_lock( iv_value = mv_repo_key iv_type = cl_abapgit_persistence_db=>c_type_repo ).
  endmethod.


  METHOD set_paramstr.
    TRY.
        CALL TRANSFORMATION id
          SOURCE XML iv_paramstr
          RESULT param = ms_param.

      CATCH cx_transformation_error INTO DATA(lx_err).
        cx_abapgit_exception=>raise( iv_text     = |Invalid Parameter|
                                     ix_previous = lx_err ).
    ENDTRY.
  ENDMETHOD.


  METHOD set_repo_key.
    mv_repo_key = iv_repo_key.
  ENDMETHOD.

  METHOD delete_unnecessary_objects.
       DATA:
      ls_checks TYPE if_abapgit_definitions=>ty_delete_checks,
      ls_tadir  TYPE if_abapgit_definitions=>ty_tadir,
      lt_tadir  TYPE if_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.

    " get confirmed deletions
    LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
      WHERE ( action = if_abapgit_objects=>c_deserialize_action-delete
      OR action = if_abapgit_objects=>c_deserialize_action-delete_add )
      AND decision = if_abapgit_definitions=>c_yes.

      ls_tadir-pgmid    = 'R3TR'.
      ls_tadir-object   = <ls_overwrite>-obj_type.
      ls_tadir-obj_name = <ls_overwrite>-obj_name.
      ls_tadir-devclass = <ls_overwrite>-devclass.
      INSERT ls_tadir INTO TABLE lt_tadir.

    ENDLOOP.

    " todo, check if object type supports deletion of parts to avoid deleting complete object

    " delete objects
    IF lines( lt_tadir ) > 0.
      ls_checks-transport = is_checks-transport.

      cl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                  is_checks = ls_checks
                                  ii_log    = ii_log ).

      io_repo->refresh( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
