<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.14";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here are a list of user visible changes with version 5.14.  See
also the list of <a href="changes-v4.php">user visible changes between
version 4 and version 5</a>.

<ul>

  <p><li>Support for CORBA's IDL language.

  <p>There is now a new mode called <code>idl-mode</code>, with all
  the expected hooks, abbreviation tables, etc.  Font-lock is not
  supported by CC Mode, but until font-lock support for IDL gets
  integrated into (X)Emacs, <a href="idl-font-lock.el"> here is a file
  defining some IDL font-lock-keywords</a>

  <p><li>In "java" style, <code>c-hanging-comment-starter-p</code> is
  set to nil by default to preserve Javadoc comments.

  <p><li>A new hook variable: <code>c-initialization-hook</code>.

  <p>This is called only once an (X)Emacs session, when the CC Mode
  package is initialized.

  <p><li>The usual assortment of bug fixes.

</ul>

<?php include ("footer.h"); ?>
