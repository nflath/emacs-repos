int foo (void)
{
    switch (x) {
    case 'a':
	switch (y) {
	default:
	    /* a comment with an apostrophe ' */
	    break;
	}
	break;
    case '\'':
	x = 1;
	break;
    }
}
