simple_struct s1 =
{
    {
        1,
        2,
        3,
        4
    }
};

complex_struct s2 =
{
    1,
    2,
    {
        1,
        2,
        3,
        4
    },
    3,
    4,
    {
        {
            1,
            2,
            3,
            4
        },
        {
            1,
            2,
            3,
            4
        }
    }
};

int func (args)
{
    blah_blah_blah();
    {
        sometype a = {
            { 1, 2, 3 },
            { 4, 5, 6 }
        };
        blah_blah_blah();
    }
}
