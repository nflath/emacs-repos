void f() {
    result = sandpapper(17, sandpapper(18, sandpapper (19) +
		 sandpapper(20)) + // Would like c-lineup-math _and_ + here.
	     sandpapper(21)) +	// Would like c-lineup-math _and_ + here.
	     sandpapper(22) +
	     offset;
    very_long_variable_name = 75 + foo() + bar()
			      + baz();
    foo[gnu /* bluu */ = 1] = bar +
			      something();
    foo[gnu /* bluu */ = 1] = bar
			    = something();
    foobar = bar
	  += baz();
    foobar == bar
	+= baz();
    foobar != bar
	+= baz();
    foobar >= bar
	+= baz();
    foobar >>= bar
	    += baz();
    foobar_x = bar
	     = baz
	     = squeek;
    foobar_xx = bar,
	    g = something();
    foobar_xxx = asd[bar,
		 g = something()]; // Would like c-lineup-math _and_ + here.
    foo[gnu = 1] = bar
	       g = something(),
	    blur = fgds
		 = ada;
    for (i_varname = 75
		   = asdf,
 another_index_var = 0
		   = grook();
	 asd			// Bogus analysis.
		     asd;	// Bogus.
	 asd			// Bogus analysis.
		     asd	// Bogus.
        )
        {}
    foobar = 1 + 1 +
	     (baz += bar()) /
	     75;
}

/* Local Variables: */
/* c-file-offsets: ((statement-cont . c-lineup-math) (arglist-cont . c-lineup-math) (arglist-cont-nonempty . c-lineup-math)) */
/* End: */

