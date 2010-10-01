void f() {
    type id;
    x (id);
    (id, id);
    (id, id) x;			/* Invalid syntax. */
    x (id, id);
    x (id, id) x;		/* Invalid syntax. */
    (id);
    x (t1) x;			/* Invalid syntax. */
    (*x) (id);
    (*x) (t2) x;		/* Invalid syntax. */
    (t3*) (id);
    (t4*) (t5) x;
    /* The following might be a function call too so we only treat it
     * as a cast if the type is known (since extra parentheses around
     * identifiers like this are common in macros). */
    (t6) (id);
    (type) (id);
    (t7) x;
    (t8*) x;
    (x * x);
    (t9) (t10) (1);
    (type) (t11) (1);
    (type) (type) (1);
    (t12()[n]) x;
    x (y) (1);
    x (y) (z) (1);
}
