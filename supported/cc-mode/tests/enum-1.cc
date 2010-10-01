enum non_nested_type
{
    correct_indentation
};

class wrapper
{
    enum nested_type
    {
        correct_indentation
    };
};

class wrapper
{
    // comment screws up coming indentation
    enum nested_type
    {
        incorrect_indentation
    };
};
