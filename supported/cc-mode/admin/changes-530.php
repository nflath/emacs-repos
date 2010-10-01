<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.30";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-529.php">user visible changes for
5.29</a>.

<p><a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.30.9.tar.gz">Download</a>
this CC Mode version.</p>

<p>There is a lot of change in this version, so it's considered
experimental.  It is however fairly well tested already since the
developers have an extensive test suite to ensure correct syntactic
analysis and font locking.

<ul>

  <p><li>Font lock support.

  <p>CC Mode now provides font lock support for all its languages.
  This supersedes the font lock patterns that have been in the core
  font lock package for C, C++, Java and Objective-C.  Like
  indentation, font locking is done in a uniform way across all
  languages (except the new AWK mode - see below).  That means that
  the new font locking will be different from the old patterns in
  various details for most languages.

  <p>The main goal of the font locking in CC Mode is accuracy, to
  provide a dependable aid in recognizing the various constructs.
  Some, like strings and comments, are easy to recognize while others,
  like declarations and types, can be very tricky.  CC Mode can go to
  great lengths to recognize declarations and casts correctly,
  especially when the types aren't recognized by standard patterns.
  This is a fairly demanding analysis which can be slow on older
  hardware, and it can therefore be disabled by choosing a lower
  decoration level with the variable
  <code>font-lock-maximum-decoration</code>.

  <p>Note that the most demanding font lock level has been tuned with
  lazy fontification in mind, i.e. there should be a support mode that
  waits with the fontification until the text is actually shown
  (e.g. Just-in-time Lock mode, which is the default in Emacs 21, or
  Lazy Lock mode).  Fontifying a file with several thousand lines in
  one go can take the better part of a minute even on a fast system.

  <ul>

    <p><li>The
    (<code>c</code>|<code>c++</code>|<code>objc</code>|<code>java</code>|<code>idl</code>|<code>pike</code>)<code>-font-lock-extra-types</code>
    variables are now used by CC Mode to recognize identifiers that
    are certain to be types.  (They are also used in cases that aren't
    related to font locking.)  At the maximum decoration level, types
    are often recognized properly anyway, so these variables should be
    fairly restrictive and not contain patterns for uncertain types.

    <p><li>Support for documentation comments.  There is a "plugin"
    system to fontify documentation comments like Javadoc and the
    markup within them.  It's independent of the host language, so
    it's possible to e.g. turn on Javadoc font locking in C buffers.
    See the variable <code>c-doc-comment-style</code> for details.

    <p>Currently two kinds of doc comment styles are recognized: Suns
    Javadoc and Autodoc which is used in Pike.  This is by no means a
    complete list of the most common tools; if your doc comment
    extractor of choice is missing then please drop a note to <a
    href="mailto:bug-cc-mode@gnu.org">bug-cc-mode@gnu.org</a>.

    <p><li>Better handling of C++ templates.  As a side effect of the
    more accurate font locking, C++ templates are now handled much
    better.  The angle brackets that delimit them are given
    parenthesis syntax so that they can be navigated like other
    parens.

    <p>This also improves indentation of templates, although there
    still is work to be done in that area.  E.g. it's required that
    multiline template clauses are written in full and then
    refontified to be recognized, and the indentation of nested
    templates is a bit odd and not as configurable as it ought to be.

    <p><li>Especially the support for Objective-C and IDL has gotten
    an overhaul.  The special "@" declarations in Objective-C are
    handled correctly.  All the keywords used in CORBA IDL, PSDL, and
    CIDL are recognized and handled correctly, also wrt indentation.

  </ul>

  <p><li>Support for the AWK language has been introduced.  The
  implementation is based around GNU AWK version 3.1, but it should
  work pretty well with any AWK.  As yet, not all features of CC Mode
  have been adapted for AWK.  Here is a summary:

  <ul>

    <p><li>Indentation Engine

    <p>The CC Mode indentation engine fully supports AWK mode.

    <p>AWK mode handles code formatted in the conventional AWK
    fashion: <code>{</code>s which start actions, user-defined
    functions, or compound statements are placed on the same line as
    the associated construct; the matching <code>}</code>s are
    normally placed under the start of the respective pattern,
    function definition, or structured statement.

    <p>The predefined indentation functions haven't yet been adapted
    for AWK mode, though some of them may work serendipitously.  There
    shouldn't be any problems writing custom indentation functions for
    AWK mode.

    <p>The command <code>C-c C-q</code> (<code>c-indent-defun</code>)
    hasn't yet been adapted for AWK, though in practice it works
    properly nearly all the time.  Should it fail, explicitly set the
    region around the function (using <code>C-u C-SPC</code>:
    <code>C-M-h</code> probably won't work either) then do
    <code>C-M-\</code> (<code>indent-region</code>).

    <p><li>Font Locking

    <p>There is a single level of font locking in AWK mode, rather
    than the three distinct levels the other modes have.  There are
    several idiosyncrasies in AWK mode's font-locking due to the
    peculiarities of the AWK language itself.

    <p><li>Comment Commands

    <p><code>M-;</code> (<code>indent-for-comment</code>) works fine.
    None of the other CC Mode comment formatting commands have yet
    been adapted for AWK mode.

    <p><li>Movement Commands

    <p>Most of the movement commands work in AWK mode.  The most
    important exceptions are <code>M-a</code>
    (<code>c-beginning-of-statement</code>) and <code>M-e</code>
    (<code>c-end-of-statement</code>) which haven't yet been adapted.

    <p>The notion of "defun" has been augmented to include AWK
    pattern-action pairs.  <code>C-M-a</code>
    (<code>c-awk-beginning-of-defun</code>) and <code>C-M-e</code>
    (<code>c-awk-end-of-defun</code>) recognise these pattern-action
    pairs, as well as user defined functions.

    <p><li>Auto-newline Insertion and Clean-ups

    <p>Auto-newline insertion hasn't yet been adapted for AWK.  Some
    of the clean-ups can actually convert good AWK code into
    syntactically invalid code.  These features are best disabled in
    AWK buffers.

  </ul>

  <p><li>New syntactic symbols in IDL mode.  The top level constructs
  "module" and "composition" (from CIDL) are now handled like
  "namespace" in C++: They are given syntactic symbols
  <code>module-open</code>, <code>module-close</code>,
  <code>inmodule</code>, <code>composition-open</code>,
  <code>composition-close</code>, and <code>incomposition</code>.

  <p><li>New lineup function <code>c-lineup-string-cont</code> which
  lines up a continued string under the one it continues.  E.g:

  <blockquote><pre>
result = prefix + "A message "
                  "string.";      <- c-lineup-string-cont</pre></blockquote>

  <p><li>New functions to do hungry delete without enabling hungry
  delete mode.  The functions <code>c-hungry-backspace</code> and
  <code>c-hungry-delete-forward</code> can be bound to keys to get
  this feature without toggling a mode.  Contributed by Kevin Ryde.

  <p><li>Better control over <code>require-final-newline</code>.  It
  is now customizable on a per-mode basis through
  <code>c-require-final-newline</code>.  The default is to set it to t
  only in languages that mandate a final newline in source files (C,
  C++ and Objective-C).

  <p><li>Format change for syntactic context elements.

  <p>The elements in the syntactic context returned by
  <code>c-guess-basic-syntax</code> and stored in
  <code>c-syntactic-context</code> has been changed somewhat to allow
  attaching more information.  They are now lists instead of single
  cons cells.  E.g. a line that previously had the syntactic analysis

  <blockquote><pre>
((inclass . 11) (topmost-intro . 13))</pre></blockquote>

  is now analysed as

  <blockquote><pre>
((inclass 11) (topmost-intro 13))</pre></blockquote>

  <p>In some cases there can be more than one position given for a
  syntactic symbol.

  <p>This change might affect code that call
  <code>c-guess-basic-syntax</code> directly, and custom lineup
  functions if they use <code>c-syntactic-context</code>.  However,
  the argument given to lineup functions is still a single cons cell
  with nil or an integer in the cdr.

  <p><li>API changes for derived modes.

  <p>There have been extensive changes "under the hood" which can
  affect derived mode writers.  Some of these changes are likely to
  cause incompatibilities with existing derived modes, but on the
  other hand care has now been taken to make it possible to extend and
  modify CC Mode with less risk of such problems in the future.

  <ul>

    <p><li>New language variable system.  See the comment blurb near
    the top of <code><a
    href="src/cc-langs.el"/>cc-langs.el</a></code>.  An <a
    href="derived-mode-ex.el">example</a> of how to write a derived
    mode is also available.

    <p><li>New initialization functions.  The initialization procedure
    has been split up into more functions to give better control:
    <code>c-basic-common-init</code>, <code>c-font-lock-init</code>,
    and <code>c-init-language-vars</code>.

  </ul>

  <p><li>Compiled byte code is now (X)Emacs version specific.
  Previously byte compiled versions of CC Mode could be shared between
  major (X)Emacs versions to some extent.  That is no longer the case
  since macros are now used extensively to adapt to (X)Emacs specific
  features without sacrificing performance.

</ul>

<p>Since the last open release was version 5.28, you probably want to
check out the list of <a href="changes-529.php">user visible changes
for 5.29</a> too.

<?php include ("footer.h"); ?>
