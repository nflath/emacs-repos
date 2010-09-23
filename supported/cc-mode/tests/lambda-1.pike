void|int foo (mapping(int:string) test)
{
    function ix = lambda () {
		      write (foo);
		      a = catch {
			      write (foo);
			  };
		  };
    map (foo, lambda
		  (object a,
		   int b) {
		  write (foo);
		  a = catch
		      {write (foo);};
	      });
    map (foo, lambda (object a,
		      int b) {
		  write (foo);});
    map (foo, lambda
		  (object a,
		   int b)
	      {write (foo);
		  a = catch
		      {write (foo);};});
    string *paths = filter (
	mac->pp_get_handles(),
	lambda (string from) {
	    return from;
	});
}
