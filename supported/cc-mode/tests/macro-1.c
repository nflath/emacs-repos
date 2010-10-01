int hello;

#define HELLO 1

#define EXTERNAL_LIST_LOOP(consvar, listp)				\
    for (consvar = listp; !NILP (consvar); consvar = XCDR (consvar))	\
	if (!CONSP (consvar))						\
	    signal_simple_error ("Invalid list format", listp);		\
	else

int goodbye;
