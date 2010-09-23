extern "C"
{
    int top_level_function(int arg1, int arg2)
    {
        int body;
        body = 3;
    }

    int top_level_function_2()
    {
        int body;
    }
}

extern "Grunch" 
{
    int grunch_spam_boozle(GRUNCH_int, GRUNCH_double);
}
