CLASS zcl_aar_abapgit_res_repo_pull DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_request_pull_data,
        branch                  TYPE string,
        transportrequest        TYPE string,
        user                    TYPE string,
        password                TYPE string,
        overwrite_objects       TYPE cl_abapgit_res_pull_mod_objs=>tt_overwrite_objects,
        package_warning_objects TYPE cl_abapgit_res_pull_mod_objs=>tt_overwrite_objects,
      END OF ty_request_pull_data.
    TYPES: tt_request_data TYPE TABLE OF ty_request_pull_data .

    CONSTANTS co_root_name_pull               TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_class_name                   TYPE string     VALUE 'ZCL_AAR_ABAPGIT_RES_REPO_PULL'.
    CONSTANTS co_st_name_pull_v2              TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL_V2' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v3         TYPE string     VALUE 'application/abapgit.adt.repo.v3+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v2        TYPE string     VALUE 'application/abapgit.adt.repos.v2+xml' ##NO_TEXT.
    CONSTANTS co_root_name_pull_req           TYPE string     VALUE 'ABAPGITPULLREQ' ##NO_TEXT.
    CONSTANTS co_st_name_selective_pull_req   TYPE string     VALUE 'ABAPGIT_ST_REPO_SELECTIVE_PULL' ##NO_TEXT.
    CONSTANTS co_content_type_pull_req_v1     TYPE string     VALUE 'application/abapgit.adt.repo.pull.req.v1+xml' ##NO_TEXT.

    METHODS post REDEFINITION.
    METHODS get  REDEFINITION.

  PROTECTED SECTION.
    METHODS: check_and_create_task.
    DATA: ms_request_data TYPE  ty_request_pull_data,
          mt_e30_use      TYPE e070use.
  PRIVATE SECTION.

   CONSTANTS cv_action TYPE a4c_repo_action VALUE if_abapgit_app_log=>c_action_pull.

   TYPES:
      BEGIN OF ts_param,
        branch           TYPE string,
        transportrequest TYPE trkorr,
        objects_to_pull  TYPE cl_abapgit_res_pull_mod_objs=>ty_modified_objs,
      END OF ts_param .

    DATA ms_param TYPE ts_param.

    METHODS validate_repo_key
      IMPORTING
        iv_repo_key TYPE if_abapgit_persistence=>ty_value
      RAISING
        cx_abapgit_exception.
    METHODS get_repo_app_log_id
      IMPORTING repo_key      TYPE if_abapgit_persistence=>ty_value
      RETURNING VALUE(result) TYPE a4c_app_log_id
      RAISING
                cx_abapgit_exception
                cx_abapgit_not_found.
ENDCLASS.



CLASS zcl_aar_abapgit_res_repo_pull IMPLEMENTATION.


  METHOD get.

    DATA(ls_requested_content_type) =
      request->get_inner_rest_request( )->get_header_field( if_http_header_fields=>content_type ).

    CASE ls_requested_content_type.
      WHEN co_content_type_repos_v2.
        DATA(lo_resp_content_handler) =
          cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text(
                                                                       content_type      = co_content_type_repos_v2
                                                                       strict_conversion = abap_true ).
      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).

    ENDCASE.



    " validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.
        response->set_body_data(
                  content_handler = lo_resp_content_handler
                  data            = |DEMO STATUS| ).

      CATCH cx_st_error cx_abapgit_exception INTO DATA(lx_error).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_error
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD post.

    DATA:
      ls_request_data TYPE ty_request_pull_data,
      lv_repo_key     TYPE if_abapgit_persistence=>ty_value.

    TRY.
        " Get Repository Key
        request->get_uri_attribute( EXPORTING
                                      name = 'key'
                                      mandatory = abap_true
                                    IMPORTING
                                      value = lv_repo_key ).

        zcl_abapgit_action_factory=>check_other_action_running( lv_repo_key ).

        DATA(ls_requested_content_type) =
          request->get_inner_rest_request( )->get_header_field( if_http_header_fields=>content_type ).

        CASE ls_requested_content_type.
          WHEN co_content_type_repo_v3.
            DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_pull_v2
                                      root_name    = co_root_name_pull
                                      content_type = co_content_type_repo_v3 ) ).

          WHEN co_content_type_pull_req_v1.
            lo_request_content_handler = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_selective_pull_req
                                      root_name    = co_root_name_pull_req
                                      content_type = co_content_type_pull_req_v1 ) ).

          WHEN OTHERS.
            response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        ENDCASE.

        " Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).
        ms_request_data = ls_request_data.

        validate_repo_key( lv_repo_key ).

        "set supplied repository credentials
        IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
          cl_abapgit_default_auth_info=>refresh( ).
          cl_abapgit_default_auth_info=>set_auth_info(
             iv_user     = ls_request_data-user
             iv_password = ls_request_data-password ).
        ENDIF.

        " get pull action instance
        data(lo_action) = zcl_abapgit_action_factory=>get_action_instance( iv_repo_key = lv_repo_key
                                                                              iv_action = if_abapgit_app_log=>c_action_pull ).

        " define parameter for pull action
        DATA(lv_type) = lo_action->get_param_type( ).
        DATA ls_param TYPE REF TO data.
        FIELD-SYMBOLS <lv_field> TYPE any.
        CREATE DATA ls_param TYPE (lv_type).
        ASSIGN ls_param->* TO FIELD-SYMBOL(<ls_param>).

        ASSIGN COMPONENT 'BRANCH'           OF STRUCTURE <ls_param> TO <lv_field>.
        <lv_field> = ls_request_data-branch.
        ASSIGN COMPONENT 'TRANSPORTREQUEST' OF STRUCTURE <ls_param> TO <lv_field>.
        <lv_field> = ls_request_data-transportrequest.

        IF ls_request_data-transportrequest IS NOT INITIAL.
          check_and_create_task( ).
          cl_abapgit_default_transport=>get_instance( )->set( iv_transport = CONV trkorr( ls_request_data-transportrequest ) ).
        ENDIF.
        DATA ls_objects_to_pull TYPE cl_abapgit_res_pull_mod_objs=>ty_modified_objs.
        ASSIGN COMPONENT 'OBJECTS_TO_PULL' OF STRUCTURE <ls_param> TO <lv_field>.

        IF ls_requested_content_type EQ co_content_type_pull_req_v1.
          "Selectively pull the objects
          ls_objects_to_pull-overwrite_objects = ls_request_data-overwrite_objects.
          ls_objects_to_pull-package_warning_objects = ls_request_data-package_warning_objects.

        ELSE.
          "For clients not supporting selective pull feature.
          "Pull all objects in case selective pull feature is not supported by clients.
          DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).
          ls_objects_to_pull = NEW cl_abapgit_res_pull_mod_objs(  )->get_all_modified_objects( io_repo = lo_repo ).
        ENDIF.

        <lv_field> = ls_objects_to_pull.

        lo_action->set_param( ir_param = REF #( <ls_param> ) ).

        " provide repository credentials
        DATA ls_credentials TYPE tsa4c_abapgit_credentials.
        ls_credentials-user     = ls_request_data-user.
        ls_credentials-password = ls_request_data-password.
        lo_action->set_credentials( ls_credentials ).

        " Initialize pull action
        lo_action->initialize( ).

        " execute pull action
        lo_action->execute( iv_app_log = get_repo_app_log_id( repo_key = lv_repo_key ) ).

        response->set_status( cl_rest_status_code=>gc_success_accepted ).

        lo_action->cleanup(  ).

        " Handle issues
      CATCH cx_abapgit_bg_action_running INTO DATA(lx_bg_action_running).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_bg_action_running
            iv_http_status = cl_rest_status_code=>gc_client_error_conflict ). "409
      CATCH cx_abapgit_exception cx_abapgit_app_log cx_a4c_logger cx_cbo_job_scheduler cx_uuid_error
          cx_abapgit_not_found INTO DATA(lx_exception).
        ROLLBACK WORK.
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_repo_app_log_id.
    " Get repository under action from the abapGit repo persistence
    DATA(repo_persistence) = cl_abapgit_persist_factory=>get_repo( ).
    DATA(repo) = repo_persistence->read( iv_key = repo_key iv_with_status = abap_true ).
    " Fetch app_log_id
    RETURN repo-app_log_key-app_log.
  ENDMETHOD.

  METHOD check_and_create_task.

    DATA: lt_request_header TYPE trwbo_request_header.
    lt_request_header-trkorr = CONV trkorr( ms_request_data-transportrequest ).
    DATA lt_request_headers TYPE trwbo_request_headers.
    APPEND lt_request_header TO lt_request_headers.

    CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
      EXPORTING
        iv_read_e070   = 'X'
      CHANGING
        cs_request     = lt_request_header
      EXCEPTIONS
        empty_trkorr   = 1
        not_exist_e070 = 1
        OTHERS         = 0.
    IF sy-subrc <> 0.
      "
    ENDIF.

    APPEND lt_request_header TO lt_request_headers.
    "*   get headers of request and all its tasks
    CALL FUNCTION 'TRINT_FOLLOW_STRKORR_BACKWARD'
      CHANGING
        ct_requests = lt_request_headers.

    SELECT SINGLE * FROM @lt_request_headers AS tasks WHERE ( trkorr NE @ms_request_data-transportrequest AND as4user EQ @sy-uname AND trstatus EQ 'D' ) INTO @DATA(lt_tasks) ##ITAB_KEY_IN_SELECT.
    IF lt_tasks IS INITIAL.
      CALL FUNCTION 'TR_INSERT_NEW_COMM' "Create new request/new task
        EXPORTING
          wi_kurztext             = 'created by abapGit'
          wi_trfunction           = 'S'
          wi_strkorr              = CONV strkorr( ms_request_data-transportrequest ) " e070-strkorr  Corresponding request (for tasks only)
        IMPORTING
          we_trkorr               = mt_e30_use-tasknum
        EXCEPTIONS
          client_range_full       = 1
          e070l_insert_error      = 2
          e070l_update_error      = 3
          e070_insert_error       = 4
          e07t_insert_error       = 5
          e070c_insert_error      = 6
          e070m_insert_error      = 7
          no_systemname           = 8
          no_systemtype           = 9
          sap_range_full          = 10
          unallowed_trfunction    = 11
          unallowed_user          = 12
          order_not_found         = 13
          invalid_targetsystem    = 14
          invalid_target_devclass = 15
          invalid_devclass        = 16
          invalid_target_layer    = 17
          invalid_status          = 18
          not_an_order            = 19
          order_lock_failed       = 20
          no_authorization        = 21
          wrong_client            = 22
          file_access_error       = 23
          wrong_category          = 24
          internal_error          = 25.
    ENDIF.
  ENDMETHOD.

  METHOD validate_repo_key.
    DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( iv_repo_key ).

    IF lo_repo IS INITIAL.
      cx_abapgit_exception=>raise( 'repo not found, get' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
