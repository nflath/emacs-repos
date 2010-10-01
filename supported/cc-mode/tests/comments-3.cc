/* Some tricky block comment cases.  This is not necessarily what one
 * would like to happen in the more deteriorated cases; it just
 * documents how it works currently. */

int main () {
    /*************
    asd
    fasd
    */

    /******
	   asd
	   asd
    *************/

    /**************************

    asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf
    asdf
    ***************************/

    /*
      asdf asdf asdf asdf asd fasdf asdasdf asdf asdfas fasdf asdf asdf

      asdf
    */

    int foo; /****
	   *******
	   asd
	   * asdasd
	   *
	   asd
	     */

    int bar; /****
	   *******
	   */
}
