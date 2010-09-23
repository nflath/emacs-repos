int foo()
{
    DO_IF_DEBUG(
	if (p (x,
	       z))
	    {
		a;
	    }
	else
	    y;
	,
	if (p (x, z)) {
	    a;
	}
	else {
	    y;
	}
	);
    DO_IF_DEBUG(
	if (p (x, z)) {
	    a;
	}
	,
	if (p (x, z)) {
	    a;
	}
	else {
	    y;
	}
	);
    DO_IF_DEBUG(
	{
	    if (p (x, z))
		a;
	}
	,
	{
	    if (p (x, z)) {
		a;
	    }
	}
	);
}
