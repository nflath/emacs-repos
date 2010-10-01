void f() {
    if(!catch(my_fd->query_address())
	) {}
    if(!catch(my_fd->query_address())
       && bar) {}
}
