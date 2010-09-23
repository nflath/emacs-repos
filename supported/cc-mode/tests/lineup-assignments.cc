void f() {
    foobar = bar
	  += baz();
    foobar == bar
    += baz();
    foobar = "bar"
	     "+= baz()";
    foobar == "bar"
	      "+= baz()";
    foobar = g (bar
	     += baz());
    foobar == g (bar
    += baz());
}

/* Local Variables: */
/* c-file-offsets: ((statement-cont . (c-lineup-assignments c-lineup-string-cont)) (arglist-cont . c-lineup-assignments) (arglist-cont-nonempty . c-lineup-assignments)) */
/* End: */

