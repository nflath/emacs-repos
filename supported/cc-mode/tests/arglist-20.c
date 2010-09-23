/* + for arglist-cont and c-lineup-arglist-close-under-paren for
 * arglist-close looks odd together. */

long_function_name (a,
		    b
		   );
long_function_name (
    a,
    b
		   );
long_function_name ({
	a;
    },
    b
    );
long_function_name (
    {
	a;
    },
    b
		   );
long_function_name (a,
		    {
			b;
		    }
		   );
long_function_name (
    a,
    {
	b;
    }
		   );
long_function_name ({
	a;
    },
    {
	b;
    }
    );
long_function_name (
    {
	a;
    },
    {
	b;
    }
		   );

/* Local Variables: */
/* c-file-offsets: ((arglist-close . c-lineup-arglist-close-under-paren)) */
/* End: */

