Type var = init, x = Type();
Type (*var) = init;
Type var[3 * peq] = init;
Type (var) = init;

int var = init, x = 'x';
int (*var) = init,
    var2 = NULL;
int var[3 * peq] = init;
int (var) = init;

foo_t var = init, x = 'x';
foo_t (*var) = init;
foo_t var[3 * peq] = init;
foo_t (var) = init;

struct Type var,
    var2;
struct Type (*var);
struct Type var[3 * peq];
struct Type (var);

// Local Variables:
// font-lock-maximum-decoration: 2
// End:
