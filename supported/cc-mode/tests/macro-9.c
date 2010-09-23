void foo()
{
    new_frame->pc = pc
#ifdef ENTRY_PROLOGUE_SIZE
	+ ENTRY_PROLOGUE_SIZE
#endif /* ENTRY_PROLOGUE_SIZE */
	- 1;
}
