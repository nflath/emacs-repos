void f()
    {
    // statement-cont, brace-list-intro, brace-list-entry,
    // brace-entry-open, brace-list-close
    foo = new foo[]
	{
	a, b,
	    {
	    c, d,
	    { e, f },
	    g, h
	    },
	i, j
	};
    foo = new foo[]
	{
	    {
	    { e, f },
	    g, h
	    },
	i, j
	};
    foo = new foo[] {
	a, b, {
	    c, d, {
		e, f },
	    g, h
	    },
	i, j
	};
    foo = new foo[] {
	    { { e, f },
	    g, h
	    },
	i, j
	};

    // arglist-intro, arglist-cont, arglist-close
	(
	a,
	    (
	    b,
	    (c)
	    ),
	d
	);
	(
	    (
	    (b),
	    c
	    ),
	d
	);
	(
	    (
	    b
	    ),
	    (
	    c
	    ),
	d
	);

    // arglist-intro, arglist-cont-nonempty, arglist-close
	(a,
	    (
	    b,
	    (c),
	    ),
	d
	);
	((
	    b,
	    (c),
	    ),
	d
	);
	((
	    (b),
	    c,
	    ),
	d
	);
	(a,
	    (
	    b
	    ),
	    (
	    c
	    ),
	d
	);
	((
	    b
	    ),
	    (
	    c
	    ),
	d
	);
	(((
		b
		),
	    c
	    ),
	d
	);

    // statement, statement-block-intro, block-open, block-close
	{
	a;
	    {
	    b;
	    {c;}
	    }
	d;
	}
	{
	    {
	    {b;}
	    c;
	    }
	d;
	}
	{
	a;
	    {
	    b;
	    }
	    {
	    c;
	    }
	d;
	}

    // substatement-block-open
    if (a)
	{
	b;
	}
    if (a)
	{b;}
    }

// class-open, class-close, inline-open, inline-close
class X
    {
    a;
	{
	b;
	}
    void f() {
	}
    void f()
	{
	}
    void f()
	{}
    }
class X
    {
	{
	a;
	}
	{
	b;
	}
    c;
    }
class X {
	{
	a;
	}
    c;
    }
class X {
    a;
    }

/* Local Variables: */
/* c-file-style: "whitesmith" */
/* End: */
