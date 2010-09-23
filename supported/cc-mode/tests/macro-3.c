#define RBSTACK_INIT(rbstack)					\
    struct rbstack_slice PIKE_CONCAT3 (_, rbstack, _top_) = {	\
    };								\
    struct rbstack_ptr rbstack = {				\
    }

#define EXTERNAL_LIST_LOOP(consvar, listp)				\
    consvar = listp;							\
    for (; !NILP (consvar); consvar = XCDR (consvar))			\
	if (!CONSP (consvar))						\
	    signal_simple_error ("Invalid list format", listp);		\
	else
