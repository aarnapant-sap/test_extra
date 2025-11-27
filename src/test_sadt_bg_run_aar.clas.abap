class test_sadt_bg_run_aar definition
  public
  final
  create public .

  public section.

    interfaces if_oo_adt_classrun .
  protected section.
  private section.
endclass.



class test_sadt_bg_run_aar implementation.


  method if_oo_adt_classrun~main.
    " declare variable
    data my_data type         sadt_bg_runs.

    " fetch my records from the background runs database
    select single * from sadt_bg_runs where owner = @sy-uname into @my_data.
    out->write( my_data ).

  endmethod.
endclass.
