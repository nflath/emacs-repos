<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.17";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here are a list of user visible changes with version 5.17.  See
also the list of <a href="changes-516.php">user visible changes for
version 5.16</a>.

<ul>

  <p><li>Recognition of enum declarations in K&R argdecls.

  <p><li>Changes to "python" style to more closely match Python C API
  coding standards.

  <p><li><code>/</code> is bound to <code>c-electric-slash</code> in
  all modes, and <code>C-c C-e</code> is bound to
  <code>c-expand-macro</code> in all languages except Java and IDL.

  <p><li>Important bug fixes, including fixes to M-a and M-e commands
  when moving into comments.

</ul>

<?php include ("footer.h"); ?>
