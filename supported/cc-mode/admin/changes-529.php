<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.29";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-528.php">user visible changes for
5.28</a>.

<p>Note: This was an unfinished interim release that was never publicly
announced.</p>

<ul>

  <p><li>Changes in analysis of nested syntactic constructs.

  <p>The syntactic analysis engine has better handling of cases where
  several syntactic constructs appear nested on the same line.  They
  are now handled as if each construct started on a line of its own.

  <p>This means that CC Mode now indents some cases differently, and
  although it's more consistent there might be cases where the old way
  gave results that's more to one's liking.  So if you find a
  situation where you think that the indentation has become worse,
  please report it to <a
  href="mailto:bug-cc-mode@gnu.org">bug-cc-mode@gnu.org</a>.

  <p><li>New syntactic symbol <code>substatement-label</code>.  This
  symbol is used when a label is inserted between a statement and its
  substatement.  E.g:

  <blockquote><pre>
if (x)
  x_is_true:
    do_stuff();</pre></blockquote>

  <p><li>Better handling of multiline macros.

  <ul>

    <p><li>Syntactic indentation inside macros: The contents of
    multiline <code>#define</code>'s are now analyzed and indented
    syntactically just like other code.  This can be disabled by the
    new variable <code>c-syntactic-indentation-in-macros</code>.  A
    new syntactic symbol <code>cpp-define-intro</code> has been added
    to control the initial indentation inside <code>#define</code>'s.

    <p><li>New lineup function <code>c-lineup-cpp-define</code>, which
    is now used by default to line up macro continuation lines.  The
    behavior of this function closely mimics the indentation one gets
    if the macro is indented while the line continuation backslashes
    are temporarily removed.  If syntactic indentation in macros is
    turned off, it works much line <code>c-lineup-dont-change</code>,
    which was used earlier, but handles empty lines within the macro
    better.

    <p><li>Automatically inserted newlines continues the macro if used
    within one.  This applies to the newlines inserted by the
    auto-newline mode, and to <code>c-context-line-break</code> and
    <code>c-context-open-line</code>.

    <p><li>Better alignment of line continuation backslashes.
    <code>c-backslash-region</code> tries to adapt to surrounding
    backslashes.  New variable <code>c-backslash-max-column</code>
    which put a limit on how far out backslashes can be moved.

    <p><li>Automatic alignment of line continuation backslashes.  This
    is controlled by the new variable
    <code>c-auto-align-backslashes</code>.  It affects
    <code>c-context-line-break</code>,
    <code>c-context-open-line</code> and newlines inserted in
    auto-newline mode.

    <p><li>Line indentation works better inside macros.  Regardless
    whether syntactic indentation and syntactic indentation inside
    macros are enabled or not, line indentation now ignores the line
    continuation backslashes.  This is most noticeable when syntactic
    indentation is turned off and there are empty lines (save for the
    backslash) in the macro.

  </ul>

  <p><li>The behavior of <code>M-;</code>
  (<code>indent-for-comment</code>) is now configurable through the
  variable <code>c-indent-comment-alist</code>.  The indentation
  behavior is based on the preceding code on the line, e.g. to get two
  spaces after <code>#else</code> and <code>#endif</code> but
  indentation to <code>comment-column</code> in most other cases
  (something which was hardcoded earlier).

  <p><li>New function <code>c-context-open-line</code> which is the
  <code>open-line</code> equivalent of
  <code>c-context-line-break</code>.

  <p><li>New lineup functions:

  <ul>

    <li><code>c-lineup-cascaded-calls</code> lines up series of calls
    separated by "->" or ".".

    <li><code>c-lineup-knr-region-comment</code> gives (what most
    people think is) better indentation of comments in the "K&R
    region" between the function header and its body.

    <li><code>c-lineup-gcc-asm-reg</code> provides better indentation
    inside <code>asm</code> blocks.  Contributed by Kevin Ryde.

    <li><code>c-lineup-argcont</code> lines up continued function
    arguments after the preceding comma.  Contributed by Kevin Ryde.

  </ul>

  <p><li>Better caching of the syntactic context.

  <p>CC Mode caches the positions of the opening parentheses (of any
  kind) of the lists surrounding the point.  Those positions are used
  in many places as anchor points for various searches.  The cache is
  now improved so that it can be reused to a large extent when the
  point is moved.  The less it moves, the less needs to be
  recalculated.

  <p>The effect is that CC Mode should be fast most of the time even
  when opening parens are hung (i.e. aren't in column zero).  It's
  typically only the first time after the point is moved far down in a
  complex file that it'll take noticeable time to find out the
  syntactic context.

  <p><li>Statements are recognized in a more robust way. They are
  handled even when they occur in an "invalid" context, e.g. in a
  function argument.  In practice that can happen when macros are
  involved.

  <p><li>Improved the way c-indent-exp chooses the block to indent.
  It now indents the block for the closest sexp following the point
  whose closing paren ends on a different line.  This means that the
  point doesn't have to be immediately before the block to indent.
  Also, only the block and the closing line is indented; the current
  line is left untouched.

  <p><li>Added toggle for syntactic indentation.  The function
  <code>c-toggle-syntactic-indentation</code> can be used to toggle
  syntactic indentation.

</ul>

<?php include ("footer.h"); ?>
