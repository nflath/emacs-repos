enum why_code {
    WHY_NOT,
    WHY_EXCEPTION,
    WHY_RERAISE,
    WHY_RETURN,
    WHY_BREAK
};

static PyObject **
unpack_sequence(v, argcnt, why)
    PyObject **v;
    int argcnt;
    enum *why_code;
{
    int i;
}
