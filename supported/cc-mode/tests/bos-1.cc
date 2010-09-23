int foo()
{
    if ((loc_block.base = (void*) shmat(id,
#ifdef HPUX
					0, // we must let the system decide
#else	
					(char*)start_addr, // we can decide
#endif	
                                        SHM_RND)) < (void*)0) {
#ifdef DEBUG
	error(__FILE__,__LINE__,("(shmat)"));
#else	
	return ERROR;
#endif	
    }
}


int i;
int j;


int foo()
{
    if ((loc_block.base = (void*) shmat(id,
					0, // we must let the system decide
					(char*)start_addr, // we can decide
                                        SHM_RND)) < (void*)0) {
	error(__FILE__,__LINE__,("(shmat)"));
	return ERROR;
    }
}
