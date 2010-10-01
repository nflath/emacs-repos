public class Foo
{
    public void one(Foo f) 
    {
	synchronized (this)
	    {
		do_it();
	    }
    }
    public synchronized void two(Foo f)
    {
	do_it();
    }
}
