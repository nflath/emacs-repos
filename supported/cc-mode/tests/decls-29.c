// Register "t" as a found type. The premise here is that it's not
// really a type but got recognized as one temporarily while some
// expression was entered.
t foo;

void f() {
    t (sp[-1]);
    t (sp)[-1];
    t (sp);
    foo (t (sp[-1]),
	 t (sp)[-1],
	 t (sp));
    type foo (type (sp[-1]),
	      type (sp)[-1],
	      type (sp));
}
