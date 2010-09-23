void f() {
  res->add_child( SemNode(TOKEN_AXIS_COLON_COLON,
			  "descendant-or-self") )->
    add_child( SemNode(TOKEN_NODE, "") );
  sem_stack += ({ SemNode(token_data[0], token_data[1]) });
}
