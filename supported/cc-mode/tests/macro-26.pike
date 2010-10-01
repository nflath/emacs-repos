static void __stdio_read_oob_callback()
{
#if constant(System.EWOULDBLOCK)
    if (errno() == System.EWOULDBLOCK) {}
#endif
    ::set_read_oob_callback(0);
    x();
}
