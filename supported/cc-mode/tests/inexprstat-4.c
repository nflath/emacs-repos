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
    NIFTY_LITTLE_KANGAS ({
	    x;
	    y;
	},
	b);
    NIFTY_LITTLE_KANGAS (a, {
	    x;
	    y;
	},
	b);
    NIFTY_LITTLE_KANGAS (a, { x;
	    y;
	    z;
	},
	b);
}
