class Foo:
    public A,
    public B
{
private:
    class Bar
	: public Baz1
	, public Baz2 /* ! */
    {
    };
};
