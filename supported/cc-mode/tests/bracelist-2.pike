mixed foo (a,
	   b, c
    );
mixed foo (
    a,
    b, c
    );
mixed foo = {a,
	     b, c
};
mixed foo = ({a,
	      b, c
});
mixed foo = ([
    a:1,
    b:"foo", c:3
]);
mixed foo = (<a,
	      b, c
>);
a = {
    a, b,
    {a, b, {a,
	    b
	}},
    foo ({
	    a; // Ok, it's not valid Pike; just checking.. ;)
	    b;
	},
	c), ([a,
	      b,
	      (<
		  a, b, c
	      >)
    ])}
