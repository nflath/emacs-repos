<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.20";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here is a list of user visible changes for CC Mode 5.20.  See also
the list of <a href="changes-519.php">user visible changes for version
5.19</a>.

<ul>

  <p><li>Multiline macros are now handled, both as they affect
  indentation, and as recognized syntax.

  <p>New syntactic symbol <code>cpp-macro-cont</code> is assigned to
  second and subsequent lines of a multiline macro definition.

  <p><li>A new style <code>"user"</code> which captures all
  non-hook-ified (i.e. top-level) <code>.emacs</code> file variable
  settings and customizations.  Style <code>"cc-mode"</code> is an
  alias for <code>"user"</code> and is deprecated.  <code>"gnu"</code>
  style is still the default however.

  <p><li><code>"java"</code> style now conforms to Sun's JDK coding
  style.

  <p><li>New commands <code>c-beginning-of-defun</code>,
  <code>c-end-of-defun</code> which are not bound by default to
  <code>C-M-a</code> and <code>C-M-e</code>.

  <p><li>New and implementations of <code>M-a</code>
  (<code>c-beginning-of-statement</code>) and <code>M-e</code>
  (<code>c-end-of-statement</code>).

  <p><li>C++ namespace blocks are supported, with new syntactic
  symbols <code>namespace-open</code>, <code>namespace-close</code>,
  and <code>innamespace</code>.

  <p><li>File local variable settings of <code>c-file-style</code> and
  <code>c-file-offsets</code> makes the style variables local to that
  buffer only.

  <p><li>New indentation functions <code>c-lineup-close-paren</code>,
  <code>c-indent-one-line-block</code>,
  <code>c-lineup-dont-change</code>.

  <p><li>Various Imenu patches (thanks to Masatake Yamato, Jan Dubois,
  and Peter Pilgrim).

  <p><li>Performance improvements.  Some improvements affect only
  Emacs or only XEmacs (see the variable
  <code>c-enable-xemacs-performance-kludge-p</code>).

  <p><li>Improvements (hopefully!) to the way CC Mode is loaded.

  <p>You should now be able to do a <code>(require 'cc-mode)</code> to
  get the entire package loaded properly for customization in your
  <code>.emacs</code> file.  A new variable
  <code>c-initialize-on-load</code> controls this and is set to
  <code>t</code> by default.

</ul>

<?php include ("footer.h"); ?>
