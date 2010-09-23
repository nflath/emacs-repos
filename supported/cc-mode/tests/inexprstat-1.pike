void|int foo (mapping(int:string) test)
{
    int a = catch
	{
	    write (foo);
	    yeti();
	} + 1 +
	1;
    write (1 + sin(7),
	   foo, gauge {
		   innit();
		   yeti();
	       },
	   gluu
	) + 1 +
	1;
    write
	(1 + sin(7),
	 foo, gauge
	     {
		 innit();
	     }
	    ); // Hmm..
    write (
	foo, gauge {
		innit();
	    },
	gluu
	);
    write
	(
	    foo, gauge
		{
		    innit();
		}
	    ); // Hmm..
}
