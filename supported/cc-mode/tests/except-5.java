public class Test {
    protected void processEvent (AWTEvent event)
    {
	if (foo)
	    if (event instanceof FocusEvent)
		try
		    {
			_value = _format.parse (getText()).doubleValue();
		    }
		catch
		    (ParseException e)
		    {
			setText (_format.format (_value));
		    }
		catch
		    (Exception e)
		    {
			showError (e);
		    }
		finally
		    {
			dispose();
		    }
	    else try {_value = _format.parse (getText()).doubleValue();}
		catch (Exception e) {showError (e);}
		finally {dispose();}
	super.processEvent (event);
    }
}
