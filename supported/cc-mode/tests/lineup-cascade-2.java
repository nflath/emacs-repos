void f() {
    result = proc.add(17).add(18, proc.add(19).add(20)
				      .add(21))
		 .add(22);
}

/* Local Variables: */
/* c-file-offsets: ((statement-cont . (c-lineup-cascaded-calls +)) (arglist-cont . c-lineup-cascaded-calls) (arglist-cont-nonempty . (c-lineup-cascaded-calls +))) */
/* End: */
