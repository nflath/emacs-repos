public class Test {
    public void run()
	throws org.omg.PortableServer.POAPackage.AdapterAlreadyExists,
	       org.omg.PortableServer.POAPackage.InvalidPolicy,
	       org.omg.PortableServer.POAPackage.BadThingie
    {
	// indent is different between the two run methods, intentional?
    }
    void bad1 (int i,
	       int j)
	throws IOException {
	int k;
    }
}

public class Test {
    public void run()
	throws
	    AnException,
	    AnotherException
    {
	// in this example, the exceptions line up but the first
	// exception is on the next line and indented slightly
	// relative to throws.
    }
}
