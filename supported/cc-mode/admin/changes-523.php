<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.23";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here is a list of user visible changes for CC Mode 5.23.  Note that
there was no public release of 5.22; this went out only to the XEmacs
maintainers and simply fixed some XEmacs specific problems.  See also
a list of <a href="changes-521.php">user visible changes for version
5.21</a>.

<p>Note that 5.23 should be considered experimental.  There are a lot
of new features that haven't been tested much outside the core
maintainer's circle.  We feel pretty confident about this release, but
we're not making it public until we get some wider feedback.

<ul>

  <p><li>The big news is the addition of support for the <a
  href="http://pike.ida.liu.se/">Pike</a> language, courtesy of Martin
  Stjernholm.  There are also two new syntactic symbols specifically
  added for the Pike support: <code>inlambda</code>, and
  <code>lambda-intro-cont</code>.

  <p><li>Martin also added support for Java anonymous classes and the
  GCC extension of allowing statements inside expressions.  There are
  two new syntactic symbols to support these features
  <code>inexpr-class</code> and <code>inexpr-statement</code>, and a
  new lineup function <code>c-lineup-inexpr-stat</code>.

  <p><li>There is a new syntactic symbol <code>brace-entry-open</code>
  that is used inside of brace lists (i.e. static initializers) when a
  list entry starts with an open brace.  Previously, these were
  recognized as <code>brace-list-entry</code>'s.
  <code>c-electric-brace</code> recognizes
  <code>brace-entry-open</code> braces, and the
  <code>brace-list-entry</code> symbol can no longer be electrified.

  <p><li>A new command <code>c-indent-line-or-region</code> which is
  not bound by default.

  <p><li>Improvements to <code>M-C-h</code>
  (<code>c-mark-function</code>).

  <p><li>Parentheses are now electric (via the new command
  <code>c-electric-paren</code>) for auto-reindenting lines when
  parens are typed.

  <p><li>In "gnu" style, <code>inline-open</code> offset is now set to
  zero.

  <p><li>Uniform handling of the <code>inclass</code> syntactic
  symbol.

  <p>The indentation associated with it is now always relative to the
  class opening brace.  This means that the indentation behavior has
  changed in some circumstances, but only if you've put anything
  besides 0 on the <code>class-open</code> syntactic symbol (none of
  the default styles do that).

  <p><li><code>c-enable-xemacs-performance-kludge-p</code> is set to
  <code>nil</code> by default, since for Emacs-friendly styles
  (i.e. where the top-level opening brace starts in column zero)
  setting this variable to <code>t</code> can degrade performance
  significantly.

  <p><li>The usual assortment of bug fixes.

</ul>

<?php include ("footer.h"); ?>
