CLASS zcl_test_push_flow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_test_push_flow IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    cl_adt_rest_push_message_util=>get_instance( )->send_progress_message( text = 'hello' percentage = 10 ).
  ENDMETHOD.
ENDCLASS.
