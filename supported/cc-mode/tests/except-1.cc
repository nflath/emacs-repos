int foo() 
{
    try
        {
            f.normalize();
        }
    catch(DivisionByZero)
        {
            cout << "Caught a DivisionByZero exception!\n";
        }

    do
        {
            f.normalize();
        }
    while( true );

    while (DivisionByZero)
        {
            cout << "Caught a DivisionByZero exception!\n";
        }
}
