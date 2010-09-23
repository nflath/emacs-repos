int a_gcc_extension (void)
{
    int a1 = (
	{
	    int y = foo (); int z;
	    if (y > 0) z = y;
	    else z = - y;
	    z;
	}
	);
    if ( ({
		int y = foo (); int z;
		y;
	    }
	    ) )
	a1 = 17;
    return ({ int y = foo (); int z; y;
	    y;
	});
}
int more() {
    int a1 = 3 +
	(
	{
	    z;
	}
	    );
    int a2 = 3 +
	({
	    z;
	});
    int a3 = 3 + ({
	    z;
	});
    return (3 + ({ int y = foo (); int z; y;
		y;
	    }));
}
