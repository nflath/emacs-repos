#!/bin/sh
# Fix all http links to the Emacs or Elisp manual.
#
# These are redirected from, e.g., "../emacs" to the existing page at
# www.gnu.org.  The link to the indent manual is deleted, because there is no
# online copy of this.
#
# This script takes exactly 1 parameter, the directory to run in.
# e.g. % 2www.gnu.org.sh ~/cc-mode/html

CURDIR=`pwd`
cd $1
for f in *.html
do mv $f asdf.html
    sed 's%href="../emacs/%href="http://www.gnu.org/software/emacs/manual/html_node/emacs/%g
s%href="../elisp/%href="http://www.gnu.org/software/emacs/manual/html_node/elisp/%g' \
    asdf.html > $f
done

mv Limitations-and-Known-Bugs.html asdf.html
sed "s%<[^<>]*>GNU indent ([^()]*)</a>%GNU indent%" \
asdf.html > Limitations-and-Known-Bugs.html

rm asdf.html
cd $CURDIR
