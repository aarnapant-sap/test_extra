class zcl_abapgit_action_factory definition
  public
  final
  create public .

  public section.
    class-methods:
      get_action_instance
        importing iv_repo_key type a4c_repo_key
                  iv_action type a4c_repo_action
        returning value(ro_instance) type ref to zif_abapgit_action_entity
        raising cx_abapgit_exception,

      check_other_action_running
        importing iv_repo_key type a4c_repo_key
        raising cx_abapgit_not_found
                cx_abapgit_action_running
                cx_abapgit_exception.
  protected section.
  private section.
endclass.



class zcl_abapgit_action_factory implementation.


  method check_other_action_running.
      DATA(ls_repo) = cl_abapgit_persist_factory=>get_repo( )->read( iv_key = iv_repo_key iv_with_status = abap_true ).
    IF ls_repo-status = if_abapgit_app_log=>c_run_status-running.
      CASE ls_repo-action.
        WHEN if_abapgit_app_log=>c_action_push.
          cx_abapgit_bg_action_running=>raise( |Another push is currently running| ).
        WHEN if_abapgit_app_log=>c_action_pull.
          cx_abapgit_bg_action_running=>raise( |Another pull is currently running| ).
        WHEN OTHERS.
          cx_abapgit_exception=>raise( |Unknown action type { ls_repo-action }| ).
      ENDCASE.
    ELSEIF ls_repo-status = if_abapgit_app_log=>c_run_status-initial.
      CASE ls_repo-action.
        WHEN if_abapgit_app_log=>c_action_push.
          cx_abapgit_bg_action_running=>raise( |Another push is currently running| ).
        WHEN if_abapgit_app_log=>c_action_pull.
          cx_abapgit_bg_action_running=>raise( |Another push is currently running| ).
        WHEN OTHERS.
          cx_abapgit_exception=>raise( |Unknown action type { ls_repo-action }| ).
      ENDCASE.
    ENDIF.
  endmethod.

  method get_action_instance.
     case iv_action.
        when if_abapgit_app_log=>c_action_pull.
          ro_instance = new zcl_abapgit_action_pull_v1( ).
        when if_abapgit_app_log=>c_action_push.
          ro_instance = new zcl_abapgit_action_push_v1( ).
      endcase.
      ro_instance->set_repo_key( iv_repo_key ).
  endmethod.

endclass.
