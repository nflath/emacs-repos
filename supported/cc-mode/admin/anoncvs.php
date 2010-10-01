<!-- -*- html -*- -->
<?php
  $title = "Anonymous CVS";
  include ("header.h");
?>

<p>CC Mode is available through anonymous CVS.  You can get the latest
development version and also (since version 5.30) the fixes that have
been incorporated into the latest release, i.e. what will become
5.31.1, 5.31.2 and so on.  You can browse the CVS repository <a
href="http://cc-mode.cvs.sourceforge.net/cc-mode/cc-mode">here</a>.

<p>To get the latest release with only bug fixes and no new features,
check out the patch branch, which is called <i>Branch_5_31</i>:

<pre>
cvs -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode login</pre>

<p>Just press Enter at the password prompt. Then:

<pre>
cvs -z3 -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode co -rBranch_5_31 cc-mode</pre>

<p>It should always be safe to use the latest version on this branch.
Please report any problems.

<h3>Checking out the development version</h3>

<p>The development version is on the main trunk in cvs:

<pre>
cvs -z3 -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode co cc-mode</pre>

<p>Being under development, there are of course no guarantees that this
version will work all the time, or at all.  That said, it usually works well;
at least I (Alan) use it in my daily work, not only when I hack on it.  It
might not be entirely compatible with user settings, but it can get more
advanced fixes that are considered too risky to be allowed into the patch
branch.

<p>You are especially welcome to report bugs, opinions and patches
regarding the development version.  However if you've found a bug,
it's a good idea to try an update before you report it since chances
are that we have found it ourselves already.

<h3>The source tree</h3>

<p>The CC Mode source will be in the root of the checked out tree.
It's the same thing you'll find in a dist tarball (with a couple of
extra files around it), so it can be byte compiled and used straight
away.  You'll find the regression test suite in the <code>tests</code>
directory; take a look in <code>000tests.el</code> to figure out how
to use it.  The <code>admin</code> directory probably isn't very
interesting; it just contains the source for this web site.

<?php include ("footer.h"); ?>
