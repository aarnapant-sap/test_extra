class zcl_abapgit_app_log_db definition
  public
  final
  create public

  global friends if_abapgit_app_log_factory .

  public section.

    interfaces zif_abapgit_app_log_db .
  protected section.
  private section.
endclass.



class zcl_abapgit_app_log_db implementation.


  method zif_abapgit_app_log_db~mass_read_app_log_by_repo.
    select * from ta4c_agit_applog
      into corresponding fields of table rt_result
      where repo_key = iv_repo_key.
*-- Delete all entries which does not match with passed repo url and develeopment package
*   This section has been added due to the fact fields are not supported in where conditions
*   having STRING as data element
    loop at rt_result assigning field-symbol(<ls_result>).
      if ( ( <ls_result>-repo_url ne iv_repo_url )
        or ( <ls_result>-dev_package ne iv_dev_package ) ).
        delete table rt_result from <ls_result>.
      endif.
    endloop.

  endmethod.

  method zif_abapgit_app_log_db~mass_delete_app_log_by_repo.
    types: begin of ts_app_log,
             key type ta4c_agit_applog-app_log,
           end of ts_app_log.
    data lt_app_log type standard table of ts_app_log.
    "collect keys for application log
    select app_log as key from ta4c_agit_applog
      into corresponding fields of table @lt_app_log
      where repo_key = @iv_repo_key.
    "delete application logs
    data lt_bal_log_handle type bal_t_logh.
    loop at lt_app_log reference into data(lr_app_log).
      data(lt_a4c_log) = cl_a4c_logger_factory=>query_by_external_id( iv_external_id = conv #( lr_app_log->key ) ).
      loop at lt_a4c_log reference into data(lr_a4c_log).
        data(lv_a4c_log_handle) = lr_a4c_log->logger->get_log_handle( ).
        if lv_a4c_log_handle is not initial.
          insert lv_a4c_log_handle into table lt_bal_log_handle.
        endif.
      endloop.
    endloop.
    if lt_bal_log_handle is not initial.
      call function 'BAL_DB_DELETE'
        exporting
          i_t_log_handle = lt_bal_log_handle
        exceptions
          others         = 0.
    endif.
    "delete abapgit logs
    delete from ta4c_agit_applog where repo_key = iv_repo_key.
    call function 'DB_COMMIT'.
  endmethod.

  method zif_abapgit_app_log_db~read_app_log_by_id.
    select single * from ta4c_agit_applog
      into rs_result
      where app_log = iv_app_log.
  endmethod.


  method zif_abapgit_app_log_db~write_app_log.
    modify ta4c_agit_applog from is_data.
  endmethod.

  method zif_abapgit_app_log_db~mass_add_repo_last_run_status.

    data lt_repo_key_range type range of a4c_app_log_id.
    data ls_repo_key_range like line of lt_repo_key_range.
    ls_repo_key_range = value #( sign = 'I' option = 'EQ' low = '' ).
    loop at ct_repo assigning field-symbol(<ls_repo>).
      ls_repo_key_range-low = <ls_repo>-key.
      append ls_repo_key_range to lt_repo_key_range.
    endloop.

    data lt_applog type standard table of ta4c_agit_applog.
    if iv_repo_action is initial.
      "all actions
      select *
           from ta4c_agit_applog
           into corresponding fields of table @lt_applog
           where repo_key in @lt_repo_key_range.
    else.
      if iv_repo_action = if_abapgit_app_log=>c_action_pull.
        select *
             from ta4c_agit_applog
             where repo_key in @lt_repo_key_range
             and  ( action  eq @iv_repo_action or
                    action  is null            or
                    action  is initial )
             into corresponding fields of table @lt_applog.
      else.
        select *
             from ta4c_agit_applog
             into corresponding fields of table @lt_applog
             where repo_key in @lt_repo_key_range
             and   action   eq @iv_repo_action.
      endif.
    endif.
    sort lt_applog by repo_key repo_url repo_branch dev_package.

    data ls_last_applog    type ta4c_agit_applog.
    data lv_last_timestamp type a4c_changed_at.
    loop at ct_repo assigning <ls_repo>.

      clear lv_last_timestamp.
      clear ls_last_applog.
      read table lt_applog reference into data(lr_applog)
           with key repo_key    = <ls_repo>-key
                    repo_url    = <ls_repo>-url
                    repo_branch = <ls_repo>-branch_name
                    dev_package = <ls_repo>-package
                    binary search.
      if sy-subrc = 0.
        loop at lt_applog reference into lr_applog from sy-tabix.
          if lr_applog->repo_key    <> <ls_repo>-key or
             lr_applog->repo_url    <> <ls_repo>-url or
             lr_applog->repo_branch <> <ls_repo>-branch_name or
             lr_applog->dev_package <> <ls_repo>-package.
            exit.
          endif.

          if lr_applog->run_status = if_abapgit_app_log=>c_run_status-running.
            "if too old - abort
            try.
                get time stamp field data(lv_curr_time).
                data(lv_secs) = cl_abap_tstmp=>subtract( tstmp1 = lv_curr_time tstmp2 = lr_applog->changed_at ).
                if lv_secs > 1800. "older than 30min
                  lr_applog->run_status = if_abapgit_app_log=>c_run_status-aborted.
                endif.
              catch cx_parameter_invalid_range cx_parameter_invalid_type ##NO_HANDLER.
                lr_applog->run_status = if_abapgit_app_log=>c_run_status-aborted.
            endtry.
            ls_last_applog = lr_applog->*.
            exit.
          endif.

          " for all else status keep it as it is
          "update latest/max entry
          if lv_last_timestamp is initial or
             lr_applog->changed_at > lv_last_timestamp or
             lr_applog->created_at > lv_last_timestamp.
            ls_last_applog = lr_applog->*.
          endif.
          if lr_applog->changed_at is not initial.
            lv_last_timestamp = lr_applog->changed_at.
          else.
            lv_last_timestamp = lr_applog->created_at.
          endif.
        endloop.

        <ls_repo>-action          = ls_last_applog-action.
        <ls_repo>-status          = ls_last_applog-run_status.
        <ls_repo>-app_log_key     = ls_last_applog-app_log.
        <ls_repo>-deserialized_at = ls_last_applog-changed_at.
        <ls_repo>-deserialized_by = ls_last_applog-changed_by.
        case ls_last_applog-run_status.
          when if_abapgit_app_log=>c_run_status-success.
            if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
              <ls_repo>-status_text = |Pushed successfully|.
            else.
              <ls_repo>-status_text = |Pulled successfully|.
            endif.
          when if_abapgit_app_log=>c_run_status-warning.
            if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
              <ls_repo>-status_text = |Pushed with warnings|.
            else.
              <ls_repo>-status_text = |Pulled with warnings|.
            endif.
          when if_abapgit_app_log=>c_run_status-error.
            if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
              <ls_repo>-status_text = |Pushed with errors|.
            else.
              <ls_repo>-status_text = |Pulled with errors|.
            endif.
          when if_abapgit_app_log=>c_run_status-aborted.
            "if final state is aborted, there should be at least one (object independent) error/aborted message in the log
            data lv_abort_reason type string.
            "2. application log
            if lv_abort_reason is initial.
              try.
                  data(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).
                  data(lo_log) = lo_log_factory->load_single( iv_app_log = ls_last_applog-app_log ).
                  data(lo_app_log) = lo_log->get_app_log( ).
                  lo_app_log->get_symsg_tab( importing et_symsg_tab = data(lt_msg) ).
                  "determine last error/abort message
                  loop at lt_msg reference into data(lr_msg) where msgty ca 'EAX'.
                    if lr_msg->msgid = 'A4C_UTIL' and lr_msg->msgno = '000' and (
                       lr_msg->msgv1 = 'Repository pull aborted' or lr_msg->msgv1 = 'Repository push aborted' ) ##NO_TEXT.
                      "ignore final abort message to get real abort reason
                      if 1 = 2. message e000(a4c_util) with 'ABORT_MESSAGE'. endif.
                      continue.
                    endif.
                    message id lr_msg->msgid type 'S' number lr_msg->msgno
                       with lr_msg->msgv1 lr_msg->msgv2 lr_msg->msgv3 lr_msg->msgv4 into lv_abort_reason.
                  endloop.
                catch cx_abapgit_app_log ##NO_HANDLER.
              endtry.
            endif.
            if lv_abort_reason is initial.
              if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
                <ls_repo>-status_text = |Push aborted|.
              else.
                <ls_repo>-status_text = |Pull aborted|.
              endif.
            else.
              if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
                <ls_repo>-status_text = |Push aborted: { lv_abort_reason }|.
              else.
                <ls_repo>-status_text = |Pull aborted: { lv_abort_reason }|.
              endif.
            endif.
          when if_abapgit_app_log=>c_run_status-running.
            if ls_last_applog-proc_total > 0 and ls_last_applog-proc_curr > 0.
              if ls_last_applog-curr_obj_name is not initial and ls_last_applog-curr_obj_type is not initial.
                if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
                  <ls_repo>-status_text = |Push running... ({ ls_last_applog-proc_curr }/{ ls_last_applog-proc_total }: { ls_last_applog-curr_obj_type } { ls_last_applog-curr_obj_name })|.
                else.
                  <ls_repo>-status_text = |Pull running... ({ ls_last_applog-proc_curr }/{ ls_last_applog-proc_total }: { ls_last_applog-curr_obj_type } { ls_last_applog-curr_obj_name })|.
                endif.
              else.
                if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
                  <ls_repo>-status_text = |Push running... ({ ls_last_applog-proc_curr }/{ ls_last_applog-proc_total })|.
                else.
                  <ls_repo>-status_text = |Pull running... ({ ls_last_applog-proc_curr }/{ ls_last_applog-proc_total })|.
                endif.
              endif.
            else.
              if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
                <ls_repo>-status_text = |Push running...|.
              else.
                <ls_repo>-status_text = |Pull running...|.
              endif.
            endif.
          when if_abapgit_app_log=>c_run_status-initial. "action log is created, pre-action phase started (e.g. comparing local<->remote differences)
            <ls_repo>-status = if_abapgit_app_log=>c_run_status-running.
            if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
              <ls_repo>-status_text = |Push starting...|.
            else.
              <ls_repo>-status_text = |Pull starting...|.
            endif.
          when others.
            <ls_repo>-status = if_abapgit_app_log=>c_run_status-warning.
            if <ls_repo>-action = if_abapgit_app_log=>c_action_push.
              <ls_repo>-status_text = |Push status '{ ls_last_applog-run_status }' unknown|.
            else.
              <ls_repo>-status_text = |Pull status '{ ls_last_applog-run_status }' unknown|.
            endif.
        endcase.
      else.
        <ls_repo>-status = if_abapgit_app_log=>c_run_status-success.
        if iv_repo_action is initial.
          <ls_repo>-status_text = |Linked|.
        elseif iv_repo_action = if_abapgit_app_log=>c_action_push.
          <ls_repo>-status_text = |Not pushed|.
        else.
          <ls_repo>-status_text = |Not pulled|.
        endif.

      endif.

    endloop.

  endmethod.

endclass.
