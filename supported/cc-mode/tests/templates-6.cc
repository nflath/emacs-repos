template <class x = y>
frob()
{
    if (x) {
        y;
    }
}

template <class X = Y>
frob()
{
    if (X::boo())
        eh();
}


template <class T, class C = Cmp<T> >
int compare(const String<T>& str1, const String<T>& str2)
{
    for (int i=0; i<str1.length() && i<str2.length(); i++)
        if (!C::eq(str1[i],str2[i])) return C::lt(str1[i],str2[i]);
    return str2.length()-str1.length();
}

template <int i = 5>
class xyzzy
{
public:
    static void frob();
};


template <int i = 5>
void xyzzy<i>::frob()
{
    printf("%d\n", i);
};
