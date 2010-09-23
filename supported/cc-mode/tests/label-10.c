/* Test labels following "else"
   Thanks to Robert Mcdonnell for the bug report! */
void spam( int index )
{
    if ( i == 10 )
	do_something_special();
    else
      silly_label:
	do_something( i );
}
