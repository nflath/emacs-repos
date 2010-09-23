<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.18";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here is a list of user visible changes for CC Mode 5.18.  See also
the list of <a href="changes-517.php">user visible changes for version
5.17</a>.

<ul>

  <p><li><code>M-a</code> and <code>M-e</code> should now properly
  move into comments when point is before or after a comment, and move
  by sentences when inside a comment.

  <p><li><code>c-electric-slash</code> should be bound in all modes
  now.  Also, <code>c-expand-macro</code> is not bound in Java or IDL
  modes.

  <p><li>Imenu enhancements: Objective-C support donated by Masatake
  (jet) YAMATO; a fix to Java support given by Ake Stenhoff; and
  improvements to C++ support given by Jan Dubois.

  <p><li>Bug fixes.

</ul>

<?php include ("footer.h"); ?>
