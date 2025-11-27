class zcl_test_exception_sysubrc definition
  public
  final
  create public .

  public section.
    methods raise_exception
              RAISING
                cx_abapgit_exception.
    interfaces if_oo_adt_classrun .
  protected section.
  private section.
endclass.



class zcl_test_exception_sysubrc implementation.


  method if_oo_adt_classrun~main.
     call function 'DEQUEUE_E_AGIT_APP_LOG'
      exporting
        mode_ta4c_agit_applog   = 'S'
        app_log           = '005056BA40D71ED98C8E689010704A83'
        _scope         = 1
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
     if sy-subrc <> 0.
      out->write( 'no exception is raised' ).
     else.
      out->write( 'exception is not required' ).
     endif.
  endmethod.

  method raise_exception.
*    cx_abapgit_action_running=>raise( |Another push is currently running| ).
  endmethod.

endclass.
