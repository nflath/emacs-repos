foo() {}
bar (x) {}
main (argc, argv) {}

/* The following are currently not recognized correctly as
 * declarations. */

main (argc, argv)
    int argc;
    char *argv;
{}

foo();
bar (x);
main (argc, argv);
