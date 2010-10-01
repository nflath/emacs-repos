<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode version 5";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>CC Mode version 5 was a major upgrade, as evidenced by the change
in major revision number.  Here is a list of the important user
visible changes in CC Mode 5.

<ul>

  <p><li>CC Mode 5 will not work with Emacs 18, and will only work
  with the latest Emacs and XEmacs releases.

  <p><li><code>c-mode-map</code> is no longer the base keymap for all
  modes.

  <p>This was incompatible with the way Emacs 19 supports menus, so
  now <code>c-mode-base-map</code> is the base map for all modes
  (including <code>c-mode-map</code>).  If you are installing custom
  keybindings into <code>c-mode-map</code> and expecting them to be
  present in all other modes, this will break.  Put your keybindings
  in <code>c-mode-base-map</code> instead.

  <p><li>The function <code>c-electric-delete</code> and variable
  <code>c-delete-function</code> are handled differently now, in order
  to accommodate the separation of the <code>BackSpace</code> and
  <code>Delete</code> keysyms.  CC Mode now binds only the
  <code>Delete</code> keysym to <code>c-electric-delete</code> (which
  runs <code>c-delete-function</code>), and the <code>BackSpace</code>
  keysym to <code>c-electric-backspace</code> (which runs
  <code>c-backspace-function</code>).  See the <a
  href="html-manual/Hungry-deletion%20of%20Whitespace.html">CC Mode
  manual</a> for details.

  <p><li>The single <code>cc-mode.el</code> file was simply too
  unwieldy so I have split the file up.

  <p>See the <a href="src/MANIFEST"><code>MANIFEST</code></a> file for
  details.

  <p><li>Also, all user variables have been converted to Per
  Abrahamsen's <dfn>Custom</dfn> library, and all support for older
  Emacsen have been ripped out.  See the <a href="compat.php">release
  notes</a> below for details of running CC Mode 5 in your version of
  Emacs.

  <p><li>All style variables are now global by default.

  <p>Specifically, the default value for
  <code>c-style-variables-are-local-p</code> is <code>nil</code>.  The
  same rules apply as before, only reversed: if you want the style
  variables to be buffer local, you should set
  <code>c-style-variables-are-local-p</code> to <code>t</code> before
  you load CC Mode.

  <p><li>The usual assortment of bug fixes, most notably for Java
  support.

</ul>

<?php include ("footer.h"); ?>
