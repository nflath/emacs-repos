int f(void **forms)
{
    cerr << "Doesn't use the word FOR: "
         << (const char *) forms[1]
         << endl;
   
    cerr << "Uses the word for: "
         << (const char *) forms[4]
         << endl;
}
