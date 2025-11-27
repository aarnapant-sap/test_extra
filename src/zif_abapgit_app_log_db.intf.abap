interface ZIF_ABAPGIT_APP_LOG_DB
  public .


  types:
    tt_repo_key_range TYPE RANGE OF if_abapgit_persistence=>ty_value .

  class-methods MASS_ADD_REPO_LAST_RUN_STATUS
    importing
      !IV_REPO_ACTION type A4C_REPO_ACTION optional
    changing
      !CT_REPO type IF_ABAPGIT_PERSISTENCE=>TY_REPOS
    exceptions
      CX_ABAPGIT_APP_LOG .
  methods WRITE_APP_LOG
    importing
      !IS_DATA type TA4C_AGIT_APPLOG
    raising
      CX_ABAPGIT_APP_LOG .
  methods READ_APP_LOG_BY_ID
    importing
      !IV_APP_LOG type SYSUUID_C32
    returning
      value(RS_RESULT) type TA4C_AGIT_APPLOG
    exceptions
      CX_ABAPGIT_APP_LOG .
  methods MASS_READ_APP_LOG_BY_REPO
    importing
      !IV_REPO_KEY type A4C_REPO_KEY
      !IV_REPO_URL type A4C_REPO_URL
      !IV_DEV_PACKAGE type DEVCLASS
    returning
      value(RT_RESULT) type A4C_TT_ABAPGIT_APPLOG
    exceptions
      CX_ABAPGIT_APP_LOG .
  methods MASS_DELETE_APP_LOG_BY_REPO
    importing
      !IV_REPO_KEY type A4C_REPO_KEY .
endinterface.
