if
    (a)
    if
	(b)
	x
	    ;
    else
	p
	    (
		x,
		y
		);
else
    z;
while
    (d)
    if
	(q)
	p
	    (x,
	     y
		);
    else
	a =
	    b;
do
    p +
	x;
while
    (q);

#define X					\
    if						\
	(a)					\
	if (b)					\
	    x;					\
	else					\
	    p (x, y);				\
    else					\
	z;					\
    while					\
	(d)					\
	if (q)					\
	    p (x, y);				\
	else					\
	    a = b;				\
    do						\
	p + x;					\
    while					\
	(q);

int x()
{
    X(
	if
	    (a)
	    if (b)
		x;
	    else
		p (x, y);
	else
	    z;
	while
	    (d)
	    if (q)
		p (x, y);
	    else
		a = b;
	do
	    p + x;
	while
	    (q);
	);
}
