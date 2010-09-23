<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.27";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-526.php">user visible changes for
5.26</a>.

<p><a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.27.tar.gz">Download</a>
this CC Mode version.</p>

<p>Note: This is mostly a bugfix release.  The features labeled
experimental in 5.26 remain and are now considered permanent.

<ul>

  <p><li><code>c-style-variables-are-local-p</code> now defaults to
  <code>t</code>.

  <p>This is an incompatible change that has been made to make the
  behavior of the style system wrt global variable settings less
  confusing for non-advanced users.  If you know what this variable
  does you might want to set it to <code>nil</code> in your
  <code>.emacs</code>, otherwise you probably don't have to bother.

  <p>Defaulting <code>c-style-variables-are-local-p</code> to
  <code>t</code> avoids the confusing situation that occurs when a
  user sets some style variables globally and edit both a Java and a
  non-Java file in the same Emacs session.  If the style variables
  aren't buffer local in this case, loading of the second file will
  cause the default style (either "gnu" or "java" by default) to
  override the global settings made by the user.

</ul>

<?php include ("footer.h"); ?>
