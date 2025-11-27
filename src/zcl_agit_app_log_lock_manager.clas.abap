class zcl_agit_app_log_lock_manager definition
  public
  final
  create private.

  public section.
    class-methods get_instance
      returning
        value(ro_instance) type ref to zcl_agit_app_log_lock_manager.

    methods:
      lock
        importing
          !iv_type  type if_abapgit_persistence=>ty_type
          !iv_value type if_abapgit_persistence=>ty_value
          !iv_mode  type enqmode default 'S'
        raising
          cx_abapgit_exception,
      remove_lock
        importing
          !iv_type  type if_abapgit_persistence=>ty_type
          !iv_value type if_abapgit_persistence=>ty_value
          !iv_mode  type enqmode default 'X'
        raising
          cx_abapgit_exception,
      is_locked
        importing
          !iv_type         type if_abapgit_persistence=>ty_type
          !iv_value        type if_abapgit_persistence=>ty_content-value
        returning
          value(rv_locked) type abap_bool.

  protected section.
  private section.
    class-data go_instance type ref to zcl_agit_app_log_lock_manager. "class attribute to hold the single instance
    data mv_update_function type funcname .
    methods get_update_function
      returning value(rv_funcname) type funcname.
endclass.



class zcl_agit_app_log_lock_manager implementation.

  method get_instance.
    if go_instance is not bound.
      create object go_instance.
    endif.
    ro_instance = go_instance.
  endmethod.

  method is_locked.
  " for checking for is locked we will have to try to acquire an exclusive non cumulative lock on it
    call function 'ENQUEUE_E_AGIT_LOG_LOCK'
      exporting
        mode_abapgit   = 'X'
        type           = iv_type
        value          = iv_value
        _scope         = 1
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      rv_locked = abap_true.
    else.
      " If the enqueue succeeds (sy-subrc = 0), it means the object was not locked.
      " We must immediately dequeue the lock we just acquired.
      call function 'DEQUEUE_E_AGIT_LOG_LOCK'
        exporting
          mode_abapgit = 'X'
          type         = iv_type
          value        = iv_value.
      rv_locked = abap_false.
    endif.
  endmethod.

  method lock.
    call function 'ENQUEUE_E_AGIT_LOG_LOCK'
      exporting
        mode_abapgit   = iv_mode
        type           = iv_type
        value          = iv_value
        _scope         = 1
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      cx_abapgit_exception=>raise_t100( ).
    endif.
  endmethod.

  method remove_lock.
    call function 'DEQUEUE_E_AGIT_LOG_LOCK'
      exporting
        mode_abapgit   = iv_mode
        type           = iv_type
        value          = iv_value
        _scope         = 1
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      cx_abapgit_exception=>raise_t100( ).
    endif.
  endmethod.

  method get_update_function.
    if mv_update_function is initial.
      mv_update_function = 'CALL_V1_PING'.
      call function 'FUNCTION_EXISTS'
        exporting
          funcname = mv_update_function
        exceptions
          others   = 2.

      if sy-subrc <> 0.
        mv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      endif.
    endif.
    rv_funcname = mv_update_function.
  endmethod.

endclass.
