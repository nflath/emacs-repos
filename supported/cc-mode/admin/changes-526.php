<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.26";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-525.php">user visible changes for
5.25</a>.

<p><a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.26.tar.gz">Download</a>
this CC Mode version.</p>

<p>Note: This release contains changes that might not be compatible
with current user setups (although it's believed that these
incompatibilities will only show up in very uncommon circumstances).
However, since the impact is uncertain, these changes may be rolled
back depending on user feedback.  Therefore there's no forward
compatibility guarantee wrt the new features introduced in this
release.

<ul>

  <p><li>New initialization procedure for the style system.

  <p>When the initial style for a buffer is determined by CC Mode
  (from the variable <code>c-default-style</code>), the global values
  of style variables now take precedence over the values specified by
  the chosen style.  This is different from the old behavior:
  previously, the style-specific settings would override the global
  settings.  This change makes it possible to do simple configuration
  in the intuitive way with Customize or with <code>setq</code> lines
  in one's <code>.emacs</code> file.

  <p>By default, the global value of every style variable is the new
  special symbol <code>set-from-style</code>, which causes the value
  to be taken from the style system.  This means that in effect, only
  an explicit setting of a style variable will cause the "overriding"
  behavior described above.

  <p>Also note that global settings override style-specific settings
  <emph>only</emph> when the initial style of a buffer is chosen by a
  CC Mode major mode function.  When a style is chosen in other ways -
  for example, by a call like <code>(c-set-style "gnu")</code> in a
  hook, or via <code>M-x c-set-style</code> - then the style-specific
  values take precedence over any global style values.  In Lisp terms,
  global values override style-specific values only when the new
  second argument to <code>c-set-style</code> is non-<code>nil</code>;
  see the function documentation for more info.

  <p>The purpose of these changes is to make it easier for users,
  especially novice users, to do simple customizations with Customize
  or with <code>setq</code> in their <code>.emacs</code> files.  On
  the other hand, the new system is intended to be compatible with
  advanced users' customizations as well, such as those that choose
  styles in hooks or whatnot.  This new system is believed to be
  almost entirely compatible with current configurations, in spite of
  the changed precedence between style and global variable settings
  when a buffer's default style is set.

  <p><i>(Thanks to Eric Eide for clarifying this explanation a
  bit.)</i>

  <ul>

    <p><li><code>c-offsets-alist</code> is now a customizable
    variable.

    <p>This became possible as a result of the new initialization
    behavior.

    <p>This variable is treated slightly differently from the other
    style variables; instead of using the symbol
    <code>set-from-style</code>, it will be completed with the
    syntactic symbols it doesn't already contain when the style is
    first initialized.  This means it now defaults to the empty list
    to make all syntactic elements get their values from the style
    system.

    <p><li>Compatibility variable to restore the old behavior.

    <p>In case your configuration doesn't work with this change, you
    can set <code>c-old-style-variable-behavior</code> to non-nil to
    get the old behavior back as far as possible.

  </ul>

  <p><li>Improvements to line breaking and text filling.

  <p>CC Mode now handles this more intelligently and seamlessly wrt
  the surrounding code, especially inside comments.  For details see
  the new chapter about this in the manual.

  <ul>

    <p><li>New variable to recognize comment line prefix decorations.

    <p>The variable <code>c-comment-prefix-regexp</code> has been
    added to properly recognize the line prefix in both block and line
    comments.  It's primarily used to initialize the various paragraph
    recognition and adaptive filling variables that the text handling
    functions uses.

    <p><li>New variable <code>c-block-comment-prefix</code>.

    <p>This is a generalization of the now obsolete variable
    <code>c-comment-continuation-stars</code> to handle arbitrary
    strings.

    <p><li>CC Mode now uses adaptive fill mode.

    <p>This to make it adapt better to the paragraph style inside
    comments.

    <p>It's also possible to use other adaptive filling packages
    inside CC Mode, notably Kyle E. Jones' Filladapt mode (<a
    href="http://wonderworks.com/">http://wonderworks.com/</a>).  A
    new convenience function <code>c-setup-filladapt</code> sets up
    Filladapt for use inside CC Mode.

    <p>Note though that the 2.12 version of Filladapt lacks a feature
    that causes it to work suboptimally when
    <code>c-comment-prefix-regexp</code> can match the empty string
    (which it commonly does).  A <a href="filladapt.php">patch</a>
    for that is available from the CC Mode web site.

    <p><li>It's now possible to selectively turn off auto filling.

    <p>The variable <code>c-ignore-auto-fill</code> is used to ignore
    auto fill mode in specific contexts, e.g. in preprocessor
    directives and in string literals.

    <p><li>New context sensitive line break function
    <code>c-context-line-break</code>.

    <p>It works like <code>newline-and-indent</code> in normal code,
    and adapts the line prefix according to the comment style when
    used inside comments.  If you're normally using
    <code>newline-and-indent</code>, you might want to switch to this
    function.

    <p><li><code>c-hanging-comment-starter-p</code> and
    <code>c-hanging-comment-ender-p</code> are obsolete.

    <p>The new comment handling code no longer consults these two
    variables.  It instead detects how the "hangingness" of the
    comment delimiters look like currently and simply keeps them that
    way.
  </ul>

  <p><li>Fixes to IDL mode.

  <p>It now does a better job in recognizing only the constructs
  relevant to IDL.  E.g. it no longer matches <code>class</code> as
  the beginning of a struct block, but it does match the CORBA 2.3
  <code>valuetype</code> keyword.  Thanks to Eric Eide.

  <p><li>Improvements to the Whitesmith style.

  <p>It now keeps the style consistently on all levels and both when
  opening braces hangs and when they don't.

  <ul>

    <p><li>New lineup function
    <code>c-lineup-whitesmith-in-block</code>.

  </ul>

  <p><li>New lineup functions <code>c-lineup-template-args</code> and
  <code>c-indent-multi-line-block</code>.

  <p>See their docstrings for details.
  <code>c-lineup-template-args</code> does a better job of tracking
  the brackets used as parens in C++ templates, and is used by default
  to line up continued template arguments.

  <p><li><code>c-lineup-comment</code> now preserves alignment with a
  comment on the previous line.

  <p>It used to instead preserve comments that started in the column
  specified by <code>comment-column</code>.

  <p><li><code>c-lineup-C-comments</code> handles "free form" text
  comments.

  <p>In comments with a long delimiter line at the start, the
  indentation is kept unchanged for lines that start with an empty
  comment line prefix.  This is intended for the type of large block
  comments that contain documentation with its own formatting.  In
  these you normally don't want CC Mode to change the indentation.

  <p><li>The <code>c</code> syntactic symbol is now relative to the
  comment start instead of the previous line, to make integers usable
  as lineup arguments.

  <p><li>All lineup functions have gotten docstrings.

  <p><li>More preprocessor directive movement functions.

  <p><code>c-down-conditional</code> does the reverse of
  <code>c-up-conditional</code>.
  <code>c-up-conditional-with-else</code> and
  <code>c-down-conditional-with-else</code> are variants of these that
  also stops at <code>#else</code> lines (suggested by Don Provan).

  <p><li>Minor improvements to many movement functions in tricky
  situations.

</ul>

<?php include ("footer.h"); ?>
