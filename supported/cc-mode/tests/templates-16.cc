// Haven't been able to figure out a way to test this automatically,
// but the bug was that only some of the angle brackets got paren
// syntax due to that the search regexp didn't stop at the "<" of
// "<>".
//
// (This also tests that we don't throw an error on a comma in a
// template arglist with nothing before it.)

struct default_member_getter_policy
    : mpl::if_<
    , return_internal_reference<>
    >,
{};
