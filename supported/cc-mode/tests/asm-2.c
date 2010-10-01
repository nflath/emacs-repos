void f()
{
    __asm__ ("foo %1, %0\n"
	     "bar %0, %1"
	     : "=r" (w),
	       "=r" (x));
    __asm__ (
	"foo %1, %0\n"
	"bar %0, %1"
	: "=r" (w),
	  "=r" (x));
}
