void foo() {
    (type) 1;
    (type) -1;
    (type) .5;
    (type) (1 + 2);
    (type) "foo";
    (type) sizeof (x);
    (type) else;		/* Invalid. */
    (type) [17];		/* Invalid. */
    (type), (type);
}
