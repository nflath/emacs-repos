class Foo
{
    public addWindowListener( new WindowAdapter()
	{
	    public void windowClosed(WindowEvent we)
	    {
		System.exit(0);
	    }
	    public void foo()
	    {}
	}
	);
    public addWindowListener
	( new WindowAdapter() {
		public void windowClosed(WindowEvent we)
		{
		    System.exit(0);
		}
		public void foo()
		{}
	    }
	    );
    button.setActionListener(
			     new java.awt.event.ActionAdaptor()
			     {
				 public void actionPerformed(ActionEvent e)
				 {
				     //Code
				 }
				 public void foo()
				 {}
			     }
			     );
    void foo()
    { // method body
	final int count = 10;
	final Object[] array = new Object[count];
	some_call(new java.util.Enumeration() {
		int index = 0;
		public boolean hasMoreElements() { return i < index; }
		public Object nextElement() { return array[index++]; }
	    });
    }
};
