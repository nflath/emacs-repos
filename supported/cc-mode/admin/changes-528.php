<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.28";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-527.php">user visible changes for
5.27</a>.

<p><a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.28.tar.gz">Download</a>
this CC Mode version.</p>

<ul>

  <p><li>The hardcoded switch to <code>"java"</code> style in Java
  mode is gone.

  <p>CC Mode used to automatically set the style to
  <code>"java"</code> when Java mode is entered.  This has now been
  removed since it caused too much confusion.

  <p>However, to keep backward compatibility to a certain extent, the
  default value for <code>c-default-style</code> now specifies the
  <code>"java"</code> style for <code>java-mode</code>, but
  <code>"gnu"</code> for all other modes (as before).  So you still
  won't notice the change if you haven't touched that variable.

  <p><li>New cleanups, <code>space-before-funcall</code> and
  <code>compact-empty-funcall</code>.

  <p>Two new cleanups have been added to <code>c-cleanup-list</code>:

  <ul>

    <li><code>space-before-funcall</code> causes a space to be
    inserted before the opening parenthesis of a function call, which
    gives the style "<code>foo&nbsp;(bar)</code>".

    <li><code>compact-empty-funcall</code> causes any space before a
    function call opening parenthesis to be removed if there are no
    arguments to the function.  It's typically useful together with
    space-before-funcall to get the style
    "<code>foo&nbsp;(bar)</code>" and "<code>foo()</code>".

  </ul>

  <p><li>Some keywords now automatically trigger reindentation.

  <p>Keywords like <code>else</code>, <code>while</code>,
  <code>catch</code> and <code>finally</code> have been made
  "electric" to make them reindent automatically when they continue an
  earlier statement.  An example:

  <blockquote><pre>
for (i = 0; i < 17; i++)
  if (a[i])
    res += a[i]->offset;
else</pre></blockquote>

  <p>Here, the <code>else</code> should be indented like the preceding
  <code>if</code>, since it continues that statement. CC Mode will
  automatically reindent it after the <code>else</code> has been typed
  in full, since it's not until then it's possible to decide whether
  it's a new statement or a continuation of the preceding
  <code>if</code>.

  <p>CC Mode uses Abbrev mode to achieve this, which is therefore
  turned on by default.

  <p><li><code>M-a</code> and <code>M-e</code> now moves by sentence
  in multiline strings.

  <p>Previously these two keys only moved by sentence in comments,
  which meant that sentence movement didn't work in strings containing
  documentation or other natural language text.

  <p>The reason it's only activated in multiline strings (i.e. strings
  that contain a newline, even when escaped by a '<code>\</code>') is
  to avoid stopping in the short strings that often reside inside
  statements.  Multiline strings almost always contain text in a
  natural language, as opposed to other strings that typically contain
  format specifications, commands, etc.  Also, it's not that
  bothersome that <code>M-a</code> and <code>M-e</code> misses
  sentences in single line strings, since they're short anyway.

  <p><li>Support for autodoc comments in Pike mode.

  <p>Autodoc comments for Pike are used to extract documentation from
  the source, like Javadoc in Java.  Pike mode now recognize this
  markup in comment prefixes and paragraph starts.

  <p><li>The comment prefix regexps on <code>c-comment-prefix</code>
  may be mode specific.

  <p>When <code>c-comment-prefix</code> is an association list, it
  specifies the comment line prefix on a per-mode basis, like
  <code>c-default-style</code> does.  This change came about to
  support the special autodoc comment prefix in Pike mode only.

  <p><li>Better handling of syntactic errors.

  <p>The recovery after unbalanced parens earlier in the buffer has
  been improved; CC Mode now reports them by dinging and giving a
  message stating the offending line, but still recovers and indent
  the following lines in a sane way (most of the time).  An
  <code>else</code> with no matching <code>if</code> is handled
  similarly.  If an error is discovered while indenting a region, the
  whole region is still indented and the error is reported afterwards.

  <p><li>Lineup functions may now return absolute columns.

  <p>A lineup function can give an absolute column to indent the line
  to by returning a vector with the desired column as the first
  element.

  <p><li>More robust and warning-free byte compilation.

  <p>Although this is strictly not a user visible change (well,
  depending on the view of a user), it's still worth mentioning that
  CC Mode now can be compiled in the standard ways without causing
  trouble.  Some code have also been moved between the subpackages to
  enhance the modularity somewhat.  Thanks to Martin Buchholz for
  doing the groundwork.

</ul>

<?php include ("footer.h"); ?>
