void foo() {
  debug_dump_rb_tree (what ?
		      (dump_data_fn *) debug_dump_indval_data :
		      (dump_data_fn *) debug_dump_ind_data);
}
