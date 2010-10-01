void f (int xyz, double
		 foo, char
		      *bar,
    int xyz)
{
    foo (xyz, aaa + bbb + ccc
	      + ddd + eee + fff);
}

void g (
    int xyz, double
	     foo,
    char *bar
    )
{
    asm ("foo %1, %0\n"
	 "bar %0, %1" : /* ) */ "=r"
				(w),
	"=r" (a ? x : y)
	z
	/* : */ : "0"
		  (y));
    asm ("bar %0, %1" : "=r"
			(w));
}

/* Local Variables: */
/* c-file-offsets: ((arglist-cont . c-lineup-argcont) (arglist-cont-nonempty . (c-lineup-argcont +))) */
/* End: */

