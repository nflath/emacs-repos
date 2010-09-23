void f()
{
    asm ("foo %1, %0\n"
	 "bar %0, %1"
	 : "=r" (w),
	   "=r" (x)
	 :
	 "0" (y),
	 "1" (z));
    asm (
	"foo %1, %0\n"
	"bar %0, %1"
	: "=r" (w),
	  "=r" (x)
	:
	"0" (y),
	"1" (z));
    asm ("bar %0, %1" : "=r"
	 (w));
    asm (
	"bar %0, %1" : "=r"
	(w));
}
