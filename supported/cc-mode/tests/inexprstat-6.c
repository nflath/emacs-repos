int foo() {
    FOO (
	{
	    a;
	}
	);
    BAR (a,
	{
	    a;
	});
    FOO (
	if (a)
	    x;
	);
    BAR (a,
	if (a)
	    x;
	);
    NIFTY_LITTLE_APPLES ({
	    x;
	    y;
	},
	b);
    NIFTY_LITTLE_APPLES (a, {
	    x;
	    y;
	},
	b);
    NIFTY_LITTLE_APPLES (a, { x;
	    y;
	    z;
	},
	b);
}

/* Local Variables: */
/* c-file-offsets: ((arglist-cont-nonempty . +)) */
/* End: */
