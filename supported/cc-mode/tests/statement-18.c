int foo() {
    if (a +
	b) while (x)
	       y;
    if (a +
	b) foo: while (x)
	       y;
    if (a +
	b) /* x
	      y */ while (x)
	       y;
    if (a +
	b) /* x */ while (x)
	       y; z;
  l: {
	x;
    }
    /* x */ if (a)
	b; z;
  l: {
	x;
    }
    /* x
       y */ if (a)
	b; z;
  l: {
	x;
    }
    /* x */ if (a) {
      l: {
	    x;
	}
    } z;
  l: {
	x;
    }
    /* x
       y */ if (a)
	{
	  l: {
		x;
	    }
	} z;
  l: {
	x;
    } if (a)
	  x;
    if (a)
	x;
    { x;
	y;
    }
}
