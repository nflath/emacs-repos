namespace foo
{

    void f()
    {
	int body;
    }

    void g()
    {
	int body;
    }

    namespace bar
    {

	void f()
	{
	    int body;
	}

	extern "C"
	{
	    int body;
	}

	void g()
	{
	    int body;
	}

    }

}
