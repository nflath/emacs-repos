class Foo {
    static {
	static int a()
	{
	    return 0;
	}
    }
    static int a()
    {
	return 0;
    }
    static {
	static int a() {
	    return 0;
	}
    }
}
