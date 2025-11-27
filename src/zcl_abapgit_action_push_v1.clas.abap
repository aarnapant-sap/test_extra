CLASS zcl_abapgit_action_push_v1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_action_entity .

    aliases:
     set_repo_key for zif_abapgit_action_entity~set_repo_key,
     execute for zif_abapgit_action_entity~execute,
     set_credentials for zif_abapgit_action_entity~set_credentials,
     set_param for zif_abapgit_action_entity~set_param,
     set_paramstr for zif_abapgit_action_entity~set_paramstr,
     initialize for zif_abapgit_action_entity~initialize,
     get_param for zif_abapgit_action_entity~get_param,
     get_credentials for zif_abapgit_action_entity~get_credentials,
     get_paramstr for zif_abapgit_action_entity~get_paramstr,
     get_param_type for zif_abapgit_action_entity~get_param_type,
     cleanup for zif_abapgit_action_entity~cleanup.


    TYPES:
      BEGIN OF ts_param,
        staged_objects  TYPE tta4c_abapgit_object,
        abapgit_comment TYPE tsa4c_abapgit_comment,
      END OF ts_param .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_repo_key TYPE a4c_repo_key.
    DATA ms_param TYPE ts_param.
    DATA ms_credentials TYPE tsa4c_abapgit_credentials.
    CONSTANTS cv_action TYPE a4c_repo_action VALUE if_abapgit_app_log=>c_action_push.

ENDCLASS.



CLASS ZCL_ABAPGIT_ACTION_PUSH_V1 IMPLEMENTATION.


  METHOD set_repo_key.
    mv_repo_key = iv_repo_key.
  ENDMETHOD.


  METHOD set_credentials.
    ms_credentials = is_credentials.
  ENDMETHOD.


  METHOD get_param_type.
    DATA lo_type TYPE REF TO cl_abap_structdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( ms_param ).
    rv_param_type = lo_type->absolute_name.
  ENDMETHOD.


  METHOD get_paramstr.

    CALL TRANSFORMATION id
      SOURCE param = ms_param
      RESULT XML rv_paramstr.

  ENDMETHOD.


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


  METHOD execute.

    "validate input parameter
    CHECK ms_param IS NOT INITIAL.

    "file status
    CONSTANTS co_file_state_added    TYPE string VALUE 'A'.
    CONSTANTS co_file_state_modified TYPE string VALUE 'M'.
    CONSTANTS co_file_state_deleted  TYPE string VALUE 'D'.

    DATA lv_text        TYPE c LENGTH 50.
    DATA lo_repo_online TYPE REF TO cl_abapgit_repo_online.
    DATA ls_commit      TYPE cl_abapgit_services_git=>ty_commit_fields.
    DATA lo_stage       TYPE REF TO cl_abapgit_stage.
    DATA ls_files       TYPE if_abapgit_definitions=>ty_stage_files.

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
        DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
        lo_repo_online ?= lo_repo.
       " DATA(lv_repo_branch) = lo_repo_online->if_abapgit_repo_online~get_selected_branch..

        "force refresh on stage, to make sure the latest local and remote files are used
        lo_repo->refresh( ).

        "prepare commit fields
        ls_commit-repo_key        = mv_repo_key.
        ls_commit-committer_name  = ms_param-abapgit_comment-committer-name.
        ls_commit-committer_email = ms_param-abapgit_comment-committer-email.
        ls_commit-author_name     = ms_param-abapgit_comment-author-name.
        ls_commit-author_email    = ms_param-abapgit_comment-author-email.
        ls_commit-comment         = ms_param-abapgit_comment-comment.
        "ls_commit-body            = .

        CREATE OBJECT lo_stage.
        ls_files = cl_abapgit_factory=>get_stage_logic( )->get( io_repo = lo_repo_online
                                                                ii_log = lo_log ).

        "retrieve repository content
        LOOP AT ms_param-staged_objects ASSIGNING FIELD-SYMBOL(<ls_staged_objects>).
          LOOP AT <ls_staged_objects>-files ASSIGNING FIELD-SYMBOL(<ls_staged_objects_files>).
            IF <ls_staged_objects_files>-localstate = co_file_state_added OR <ls_staged_objects_files>-localstate = co_file_state_modified.
              "new file or changed file
              LOOP AT ls_files-local ASSIGNING FIELD-SYMBOL(<ls_files_local>).
                IF <ls_files_local>-file-filename EQ <ls_staged_objects_files>-filename.
                  lo_stage->add( iv_path     = <ls_files_local>-file-path
                                 iv_filename = <ls_files_local>-file-filename
                                 iv_data     = <ls_files_local>-file-data ).
                  lo_log->add_text( iv_text = |File { <ls_files_local>-file-filename } of object { <ls_staged_objects>-object_ref-type } { <ls_staged_objects>-object_ref-name } is selected to be exported |
                                    iv_type = 'I' ).
                  CONTINUE.
                ENDIF.
              ENDLOOP.
            ELSEIF <ls_staged_objects_files>-localstate = co_file_state_deleted.
              "deletion case: file need to be deleted on repository
              lo_stage->rm( iv_path     = <ls_staged_objects_files>-path
                            iv_filename = <ls_staged_objects_files>-filename ).
              lo_log->add_text( iv_text = |File { <ls_staged_objects_files>-filename } of object { <ls_staged_objects>-object_ref-type } { <ls_staged_objects>-object_ref-name } is selected to be removed remotely |
                                iv_type = 'I' ).
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "trigger commit
        cl_abapgit_services_git=>commit( is_commit   = ls_commit
                                         io_repo     = lo_repo_online
                                         io_stage    = lo_stage
                                         io_log      = lo_log ).

        "log final status
        DATA(lv_run_status) = lo_log->if_abapgit_log~get_status( ).
        CASE lv_run_status.
          WHEN if_abapgit_app_log=>c_run_status-success.
            lo_log->add_text( iv_text = 'Objects pushed successfully'  iv_type = 'S' ).
          WHEN if_abapgit_app_log=>c_run_status-warning.
            lo_log->add_text( iv_text = 'Objects pushed with warnings' iv_type = 'W' ).
          WHEN OTHERS. "no other value expected
            lo_log->add_text( iv_text = 'Objects pushed with error(s)' iv_type = 'E' ).
        ENDCASE.
        lo_log->set_run_status( lv_run_status ).

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.

        "log exception
        IF lo_log IS BOUND.
          lo_log->add_exception( lx_root ).
          lo_log->add_text( iv_text = 'Repository push aborted' iv_type = 'A' ).
          lo_log->set_run_status( if_abapgit_app_log=>c_run_status-aborted ).
        ENDIF.
        cx_abapgit_exception=>raise( iv_text = 'Repository push aborted' ix_previous = lx_root ).

    ENDTRY.

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

  METHOD initialize.

    "validate input parameter
    CHECK ms_param IS NOT INITIAL.
    DATA lo_repo_online TYPE REF TO cl_abapgit_repo_online.

    TRY.
        "Create new log in history table before starting batch processing
        DATA(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).

        "Determine repository specific data
        DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
        lo_repo_online ?= lo_repo.
        DATA(lv_repo_branch) = lo_repo_online->if_abapgit_repo_online~get_selected_branch( ).

        DATA(lo_log) = lo_log_factory->create_new( iv_repo_key    = mv_repo_key
                                                   iv_repo_branch = lv_repo_branch
                                                   iv_repo_action = cv_action ).
        lo_log->save( ).

      CATCH cx_abapgit_not_found cx_abapgit_app_log INTO DATA(lx_err).
        lo_log->set_run_status( iv_status = 'A' ).
        cx_abapgit_exception=>raise( iv_text     = |Push action cannot be triggered|
                                     ix_previous = lx_err ).
    ENDTRY.

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


  METHOD get_credentials.
    rs_credentials = ms_credentials.
  ENDMETHOD.
ENDCLASS.
