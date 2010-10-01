class __declspec(dllexport) classname {};
void __declspec(noreturn) debug_fatal(const char *fmt, ...) __attribute__((noreturn,format (printf, 1, 2)));
__declspec(noreturn) void debug_fatal(const char *fmt, ...) __attribute__((noreturn,format (printf, 1, 2)));
