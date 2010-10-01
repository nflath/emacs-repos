void f() {
    BEGIN_AGGREGATE_ARRAY (MINIMUM (a->size, 120)) {
      foo();
    } END_AGGREGATE_ARRAY;	/* Known bogus fontification. */
}
