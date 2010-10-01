/* It's uncertain that the way the surrounding contexts affect the
 * #define's here is entirely satisfactory. */

#if defined (foo)				\
    && defined (bar)
#endif
#if (defined(abc)				\
     && defined (def))
#endif
#define x(y) defined (foo)			\
    && defined (bar)
#define x(y) (defined (foo)			\
	      && defined (bar)			\
	)
#define x(y) (					\
	defined (foo)				\
	&& defined (bar)			\
	)
#define x(a,					\
	  b					\
    )						\
    a b

int x() {
    int x;
#if defined (foo)				\
    && defined (bar)
#endif
#if (defined(abc)				\
     && defined (def))
#endif
#define x(y) defined (foo)			\
	&& defined (bar)
#define x(y) (defined (foo)			\
	      && defined (bar)			\
	)
#define x(y) (					\
	defined (foo)				\
	&& defined (bar)			\
	)
#define x(a,					\
	  b					\
    )						\
    a b
}

int x (int x,
#if defined (foo)				\
    && defined (bar)
#endif
#if (defined(abc)				\
     && defined (def))
#endif
#define x(y) defined (foo)			\
       && defined (bar)
#define x(y) (defined (foo)			\
	      && defined (bar)			\
	   )
#define x(y) (					\
	   defined (foo)			\
	   && defined (bar)			\
	   )
#define x(a,					\
	  b					\
    )						\
       a b
    );
