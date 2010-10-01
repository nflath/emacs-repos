#define trycatch do {				\
	try					\
	    {					\
		a;				\
	    }					\
	catch					\
	    (x)					\
	    {					\
		b;				\
	    }					\
    } while (0)
