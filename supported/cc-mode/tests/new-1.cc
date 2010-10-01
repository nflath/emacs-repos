void f() {
    e = *new E1(*ee,*this);
    e = *new (17) E2;
    e = *new (t, p) E3;
    e = *new E4 (18);
    e = *new (x) E5 (p);
    e = *new (x) (E6) (p);
    e = *new (x, u) (E7);
    e = *new (E8) (18);
    e = *new (17) (E9);
    e = *new (E1) (y);
    e = *new (x) (E1);
    e = *new (x) (y);		// Now we give up...
    // e = *new E1(*ee,*this); e = *new (17) E2;
}
