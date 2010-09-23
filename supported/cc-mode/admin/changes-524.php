<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.24";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>Here is a list of user visible changes since the last public
release of 5.21.  See also the <a href="changes-521.php">user visible
changes for 5.21 and earlier</a>.

<h3>CC Mode 5.22</h3>

<p>There was no net release of 5.22!  This version went only to the
XEmacs developers and was primarily a bug fix release.

<h3>CC Mode 5.23</h3>

<p>Note: Due to the extensive changes, this version was a beta test
release that was never publically announced. Here is a list of changes
in that version:

<ul>

  <p><li>Support for the Pike language added, along with new Pike
  specific syntactic symbols: <code>inlambda</code>,
  <code>lambda-intro-cont</code>

  <p><li>Support for Java anonymous classes via new syntactic symbol
  <code>inexpr-class</code>.  New syntactic symbol
  <code>inexpr-statement</code> for Pike support and gcc-style
  statements inside expressions.  New lineup function
  <code>c-lineup-inexpr-stat</code>.

  <p><li>New syntactic symbol <code>brace-entry-open</code> which is
  used in brace lists (i.e. static initializers) when a list entry
  starts with an open brace.  These used to be recognized as
  <code>brace-list-entry</code>'s.  <code>c-electric-brace</code> also
  recognizes <code>brace-entry-open</code> braces
  (<code>brace-list-entry</code>'s can no longer be electrified).

  <p><li>New command <code>c-indent-line-or-region</code>, not bound
  by default.

  <p><li><em>#</em> is only electric when typed in the indentation of
  a line.

  <p><li>Improvements to <em>M-C-h</em> (<code>c-mark-function</code>)

  <p><li>Parentheses are now electric (via the new command
  <code>c-electric-paren</code>) for auto-reindenting lines when
  parentheses are typed.

  <p><li>In <em>gnu</em> style, <code>inline-open</code> offset is now
  set to zero.

  <p><li>Uniform handling of the <code>inclass</code> syntactic
  symbol.

  <p>The indentation associated with it is now always relative to the
  class opening brace.  This means that the indentation behavior has
  changed in some circumstances, but only if you've put anything
  besides 0 on the <code>class-open</code> syntactic symbol (none of
  the default styles do that).

  <p><li><code>c-enable-xemacs-performance-kludge-p</code> is set to
  nil by default, since for Emacs-friendly styles (i.e. where the
  top-level opening brace starts in column zero) setting this variable
  to t can degrade performance significantly.

</ul>

<h3>CC Mode 5.24</h3>

<ul>

  <p><li><code>c-default-style</code> can now take an association list
  that maps major modes to style names.

  <p>When this variable is an alist, Java mode no longer hardcodes a
  setting to <em>java</em> style.  See the variable's docstring for
  details.

  <p><li>It's now possible to put a list as the offset on a syntactic
  symbol.

  <p>The list is evaluated recursively until a non-nil offset is
  found.  This is useful to combine several lineup functions to act in
  a prioritized order on a single line.  However, none of the supplied
  lineup functions use this feature currently.

  <p><li>New syntactic symbol <code>catch-clause</code>, which is used
  on the <code>catch</code> and <code>finally</code> lines in
  "try-catch" constructs in C++ and Java.

  <p><li>New cleanup <code>brace-catch-brace</code> on
  <code>c-cleanup-list</code>, which does for "catch" lines what
  <code>brace-elseif-brace</code> does for "else if" lines.

  <p><li>The braces of Java anonymous inner classes are treated
  separately from the braces of other classes in auto-newline mode.

  <p>Two new symbols <code>inexpr-class-open</code> and
  <code>inexpr-class-close</code> may be used on
  <code>c-hanging-braces-alist</code> to control the automatic
  newlines used for anonymous classes.

  <p><li><a href="mailto:bug-cc-mode@gnu.org">bug-cc-mode@gnu.org</a>
  is now the primary bug reporting address.

  <p>This is an alias for cc-mode-help@lists.sourceforge.net.

</ul>

<?php include ("footer.h"); ?>
