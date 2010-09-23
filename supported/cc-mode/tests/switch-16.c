/* Compile time expressions as case labels.  Thanks Reuben Thomas! */
int main (void)
{
    int foo, bar;
    switch (foo)
        {
        case BLAH | 'a':
            bar = 0;
            foo = 2 ;
            break;
        case L'a':
            bar = 0 ;
            break ;
        case 'a' | BLAH:
            bar = 0;
            foo = 2;
            break;
        }
}
