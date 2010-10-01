int function()
{
    switch (arg) {
    case 1: printf("Action to be taken for"
		   "case 1 here\n");
	printf("foo\n");
	break;
    case 2: printf("Action to be taken for case 2 here\n");
	printf("foo\n");
	break;
    default: printf("Not allowed!\n");
	printf("foo\n");
    }
    return 0;
}
