void f() {
    foo :: bar x;
    foo (*x) (y);
    foo::gnu (*x) (y);
    foo::bar (*x) (y);
}
