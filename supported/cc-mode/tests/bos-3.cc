int main()
{
    if ( c_output.isNULL() )
        out_stream = &cout;
    else {
        of_stream.open(c_output);
        if ( !of_stream ) {
            cmd.error()
                << "foobish" << endl;
            return EX_CANTCREAT;
        }
        out_stream = &of_stream;
    }

    *out_stream <<
        "GAstress Run Parameters:\n"
        "------------------------\n"
        "input data files:" << endl;
}
