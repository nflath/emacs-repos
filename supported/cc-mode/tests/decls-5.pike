void a1()
{
    blerp ("fie",
	   2) +
	17;
    class Bar
    {
	int i;
    };
}
void a1h()
{
    blerp ("fie",
	   2) +
	17;
    class Bar {
	int i;
    };
}
void a2()
{
    blerp ("fie",
	   2) +
	17;
    program Bar = class
	{
	    int i;
	};
}
void a2h()
{
    blerp ("fie",
	   2) +
	17;
    program Bar = class {
	    int i;
	};
}
void b1()
{
    blerp ("fie",
	   2) +
	17;
    int gnu (int i)
    {
	return i;
    };
    if (a ||
	b)
	{
	    f();
	}
    if (a || b)
	{
	    f();
	}
}
void b1h()
{
    blerp ("fie",
	   2) +
	17;
    int gnu (int i) {
	return i;
    };
    if (a ||
	b) {
	f();
    }
    if (a || b) {
	f();
    }
}
void b2()
{
    blerp ("fie",
	   2) +
	17;
    function gnu = lambda (int i)
		   {
		       return i;
		   };
}
void b2h()
{
    blerp ("fie",
	   2) +
	17;
    function gnu = lambda (int i) {
		       return i;
		   };
}
