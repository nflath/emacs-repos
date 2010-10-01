int main() {
    t1 id;
    t2 id = 1;

    t1 (id) = 1;
    t1 (*id) = NULL;
    t1 (id)[n] = {1};
    t1 (*id)[n] = NULL;
    t1 (id) (1);

    // These might be function or macro calls.
    f1 (id) = 1;
    f2 (*id) = NULL;
    f3 (id)[n] = {1};
    f4 (*id)[n] = NULL;
    f5 (id) (1);

    t1 (*id) (NULL);
    v2 (*id) (NULL);
    v9  *id  (NULL);

    t1 (*id) (t7*x);		// The last paren could also be an initializer.
    v10(*id) (v12*x);
    v11 *id  (v13*x);

    t3 (*id) (NULL) + 1;	// Invalid.
    t3  *id  (NULL) + 1;	// Invalid.
    t3  (id) (NULL) + 1;

    v3 (*id) (NULL) + 1;
    v10 *id  (NULL) + 1;
    v17 (id) (NULL) + 1;

    v4 ();
    t5 (*);			// Not valid in a block.
    v20 ()[n];
    v5 (id);
    v6 (id)[n];
    v7 [n];
    v8 [n] = 1;

    t8 * id;
    t9 * id = 1;
    v14 *;
    (v15 * id);
    x (a*b, c*d);
    v16 (*id);
    *v18 = v19;

    if(count_args(CDR(n))==1) {}
}
