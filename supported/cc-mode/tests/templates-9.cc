typedef std::pair<int
		  , std::pair<long
			      , std::pair<char*
					  , std::pair<nil,nil>
					 >
			     >
		 > list_of_types;

typedef std::pair<
    int, std::pair<
	long, std::pair<
	    char*, std::pair<nil,nil>
	>
    >
> list_of_types;

typedef typename if_true
<
    boost::is_same
    <
	boost::add_pointer<X>
	, int*
    >::value
>::template then
<
    boost::remove_reference<X>
    // else
    , X
>::type modified_X;
