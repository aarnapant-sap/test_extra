INTERFACE zif_abapgit_action_entity
  PUBLIC .

  TYPES: t_paramstr TYPE string.

  METHODS get_paramstr
    RETURNING VALUE(rv_paramstr) TYPE t_paramstr.

  METHODS get_param
    RETURNING VALUE(rr_param) TYPE REF TO data
    RAISING   cx_abapgit_exception.

  METHODS get_param_type
    RETURNING VALUE(rv_param_type) TYPE string
    RAISING   cx_abapgit_exception.

  METHODS get_credentials
    RETURNING VALUE(rs_credentials) TYPE tsa4c_abapgit_credentials
    RAISING   cx_abapgit_exception.

  METHODS set_paramstr
    IMPORTING iv_paramstr TYPE t_paramstr
    RAISING   cx_abapgit_exception.

  METHODS set_repo_key
    IMPORTING iv_repo_key TYPE a4c_repo_key
    RAISING   cx_abapgit_exception.

  METHODS set_param
    IMPORTING ir_param TYPE REF TO data
    RAISING   cx_abapgit_exception.

  METHODS set_credentials
    IMPORTING is_credentials TYPE tsa4c_abapgit_credentials
    RAISING   cx_abapgit_exception.

  METHODS initialize
    RAISING   cx_abapgit_exception.

  METHODS cleanUp
    RAISING cx_abapgit_exception.

  METHODS execute
    IMPORTING iv_app_log TYPE a4c_app_log_id OPTIONAL
    RAISING   cx_abapgit_exception.

ENDINTERFACE.
