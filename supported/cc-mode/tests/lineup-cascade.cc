void f() {
    result = proc->add(17, proc->add(18, proc->add(19, proc->add(20)
							   ->add(21))
					     ->add(22))
			       ->add(23))
		 ->add(24) +
	offset;
}

/* Local Variables: */
/* c-file-offsets: ((statement-cont . (c-lineup-cascaded-calls +)) (arglist-cont . c-lineup-cascaded-calls) (arglist-cont-nonempty . (c-lineup-cascaded-calls +))) */
/* End: */

