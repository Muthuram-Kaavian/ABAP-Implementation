class ZCL_Z_DEIMPLEMENT_01_DPC_EXT definition
  public
  inheriting from ZCL_Z_DEIMPLEMENT_01_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.

  methods Z_C_DEIMPLEMENTC_CREATE_ENTITY
    redefinition .
  methods Z_C_DEIMPLEMENTC_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_DEIMPLEMENT_01_DPC_EXT IMPLEMENTATION.


METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.



ENDMETHOD.


METHOD z_c_deimplementc_create_entity.
  DATA: ls_output TYPE zcl_z_deimplement_01_mpc=>ts_z_c_deimplementchecktype.

  " Simple hardcoded response to test if method works
  ls_output-step        = 'DEBUG'.
  ls_output-status      = 'SUCCESS'.
  ls_output-message     = 'Method is working'.
  ls_output-transport   = 'TEST123'.
  ls_output-notenumber  = '1234567'.

  er_entity = ls_output.
ENDMETHOD.


METHOD z_c_deimplementc_get_entityset.

  " Initialize empty entity set
  et_entityset = VALUE #( ).

  " This will automatically return 200 OK for empty successful response

ENDMETHOD.
ENDCLASS.
