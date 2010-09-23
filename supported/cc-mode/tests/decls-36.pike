void f() {
  res->add_child( SemNode(TOKEN_AXIS_COLON_COLON,
			  "descendant-or-self") )->
    add_child( SemNode(TOKEN_NODE, "") );
  res->add_child (Foo(BAR));
}
