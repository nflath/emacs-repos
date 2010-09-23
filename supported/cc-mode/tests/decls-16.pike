void a(void|Foo x) {}
void b(void|Bar x) {}
void c(Gnu|void x) {}
void d(Gaz|Onk x) {}

Symbol assoc_upper_symbol (int offset)
    // "offset" above was unfontified if that line was changed.
{
    this_program::offset = offset;
}
