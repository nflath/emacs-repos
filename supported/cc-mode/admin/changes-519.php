<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.19";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here is a list of user visible changes for CC Mode 5.19.  See also
the list of <a href="changes-518.php">user visible changes for version
5.18</a>.

<ul>

  <p><li><code>extern-lang-close</code> relative buffer positions have
  changed.

  <p>The used to point to the extern's open brace, but they now point
  to the first non-whitespace character on the line with the open
  brace.

  <p><li><code>c-progress-interval</code>'s semantics have changed
  slightly.

  <p>When set to <code>nil</code>, indentation proceeds silently.
  Previously, even when <code>nil</code>, the start and end messages
  were printed.

  <p><li>The usual assortment of bug fixes.

</ul>

<?php include ("footer.h"); ?>
