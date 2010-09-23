extern ESA_RC apiinit (ADDINFO_rec_typ      * addbuf,
                       ADMIN_PARAMS_rec_typ * admin_params,
                       ERR_STRUCT_rec_typ   * err);

typedef ESA_RC (*apiinit_func_ptr)
(ADDINFO_rec_typ      * addbuf,
 ADMIN_PARAMS_rec_typ * admin_params,
 ERR_STRUCT_rec_typ   * err);

extern ESA_RC apiterm (ADDINFO_rec_typ      * addbuf,
                       ADMIN_PARAMS_rec_typ * admin_params,
                       ERR_STRUCT_rec_typ   * err);

typedef ESA_RC (*apiterm_func_ptr)
(ADDINFO_rec_typ      * addbuf,
 ADMIN_PARAMS_rec_typ * admin_params,
 ERR_STRUCT_rec_typ   * err);
