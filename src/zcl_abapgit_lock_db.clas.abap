class zcl_abapgit_lock_db definition
  public
  final
  create public .

  public section.
    interfaces if_oo_adt_classrun.
  protected section.
  private section.
endclass.



class zcl_abapgit_lock_db implementation.

  method if_oo_adt_classrun~main.

    select * from zabapgit_locks into table @data(check) where value = '000000000001'.
    if sy-subrc <> 0.
      out->write( 'not found' ).
    else.
      out->write( 'found' ).
    endif.

  endmethod.

endclass.
