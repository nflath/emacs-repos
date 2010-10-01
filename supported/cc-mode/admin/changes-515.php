<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.15";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here are a list of user visible changes with version 5.15.  See
also the list of <a href="changes-514.php">user visible changes for
version 5.14</a>.

<ul>

  <p><li>A new syntactic symbol: <code>template-args-cont</code>, used
  in C++ template declarations where the argument list spans multiple
  lines.

  <p><li>In <code>"java"</code> style,
  <code>c-hanging-comment-starter-p</code> defaults to
  <code>nil</code> to preserve Javadoc starter lines.

  <p><li>Line oriented comments (i.e. C++ style comments) are now
  recognized by default in all modes, <em>including</em> C mode (as
  per the ANSI 9X C draft standard).  Thus the function
  <code>c-enable-//-in-c-mode</code> has been removed.

  <p><li>Auto-filling of comments has been improved.

  <p>CC Mode will now properly auto-fill both line and block oriented
  comments, and allows you to choose the leader string on block
  oriented continued comments, via the variable
  <code>c-comment-continuation-stars</code>.  See the CC Mode manual
  for details.

  <p><li><code>c-electric-slash</code> is electric in all modes.

  <p><li>The need for <code>c-mode-19.el</code> is automatically
  detected now.

  <p>You do not need to <code>load</code> or <code>require</code> it
  in your <code>.emacs</code> file.

</ul>

<?php include ("footer.h"); ?>
