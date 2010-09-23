// Test an "Explicit specialisation" of a template class.
// This gave trouble because of the empty angle brackets (2006/7/1)

template <>
struct test <int>
{
    void
    function ()
    { }
};
