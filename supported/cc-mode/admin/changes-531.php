<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.31";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-530.php">user visible changes for
5.30</a>.

<p><a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.31.tar.gz">Download</a>
this CC Mode version.</p>

<p>This version contains only a few new visible features, but significant
internal improvements.

<ul>

   <p><li>Emacs 19.34 and XEmacs 19.15 are no longer supported.
   <br>The minimum versions required are now Emacs 20.4 or XEmacs 21.4.

   <p><li>The CC Mode manual has been extensively revised.
   <br>The information about using CC Mode has been separated from the
   larger and more difficult chapters about configuration.

   <ul>

        <p><li>There are now two variants of the manual - for GNU Emacs
       and XEmacs.  The only difference between them is where some
       cross references point (e.g. GNU Emacs Manual vs. XEmacs
       Manual).  The default variant is for GNU.  To build an XEmacs
       version, you must define the texinfo symbol "XEMACS".  See
       README and cc-mode.texi.

   </ul>

   <p><li>Changes in Key Sequences

   <ul>

       <p><li><code>c-toggle-auto-hungry-state</code> is no longer bound to <code>C-c C-t</code>.

       <p><li><code>c-toggle-hungry-state</code> is no longer bound to <code>C-c C-d</code>.
       <br>(This binding has been taken over by <code>c-hungry-delete-forwards</code>.)

       <p><li><code>c-toggle-auto-state</code> (<code>C-c C-t</code>) has been renamed to c-toggle-auto-newline.
       <br><code>c-toggle-auto-state</code> remains as an alias.

       <p><li><code>c-hungry-backspace</code> has been renamed to c-hungry-delete-backwards.
       <br><code>c-hungry-backspace</code> remains as an alias.

       <p><li><code>c-hungry-delete-backwards</code> and
       <code>c-hungry-delete-forwards</code> now have permanent key
       bindings, respectively <code>C-c C-DEL</code> (or <code>C-c
       DEL</code>, for the benefit of TTYs) and <code>C-c C-d</code>
       (or <code>C-c C-&lt;delete&gt;</code> or <code>C-c
       &lt;delete&gt;</code>).  These commands delete entire blocks of
       whitespace with a single key-sequence.  (N.B. &quot;DEL&quot;
       is the &lt;backspace&gt; key.)

       <p><li>The new command <code>c-toggle-electric-mode</code> is bound to <code>C-c C-l</code>.

       <p><li>The new command <code>c-subword-mode</code> is bound to <code>C-c C-w</code>.

   </ul>

   <p><li><code>C-c C-s</code>
   (<code>c-show-syntactic-information</code>) now highlights the
   anchor position(s).

   <p><li>The new GtkDoc Doc Comment style has become the default for C Mode.
   <br>Contributed by Masatake YAMATO.

   <p><li>New Minor Modes

   <ul>

       <p><li><i>Electric Minor Mode</i> toggles the electric action
       of non-alphabetic keys.
       <p>The new command <code>c-toggle-electric-mode</code> is bound
       to <code>C-c C-l</code>.  Turning the mode off can be helpful
       for editing chaotically indented code and for users new to CC
       Mode, who sometimes find electric indentation disconcerting.
       Its current state is displayed in the mode line with an
       'l', e.g. &quot;C/al&quot;.

       <p><li><i>Subword Minor Mode</i> makes Emacs recognize word
       boundaries at upper case letters in
       <code>StudlyCapsIdentifiers</code>.
       <p>You enable this feature by <code>C-c C-w</code>.  It can
       also be used in non-CC Mode buffers.  :-) Contributed by
       Masatake YAMATO.  Subword Minor Mode doesn't (yet?) work in
       Emacs 20.n.

   </ul>

   <p><li>New clean-ups

   <ul>

       <p><li><code>comment-close-slash</code>.
       <p>With this clean-up, a block (i.e. c-style) comment can be
       terminated by typing a slash at the start of a line.

       <p><li><code>c-one-liner-defun</code>
       <p>This clean-up compresses a short enough defun (for example,
       an AWK pattern/action pair) onto a single line.  &quot;Short
       enough&quot; is configurable.

   </ul>

   <p><li>AWK support
   <p>AWK Mode is now better integrated into CC Mode as a whole.  In
   detail:

   <ul>

       <p><li>Comment and line-breaking commands now work for AWK.

       <p><li><code>M-a</code> and <code>M-e</code>
       (<code>c-beginning/end-of-statement</code>) now work for AWK.

       <p><li>&quot;awk&quot; style, Auto-newline insertion, special AWK initialization hook.
       <p>A new style, &quot;awk&quot; has been introduced, and this
       is now the default style for AWK code.  With its introduction,
       Auto-newline insertion can be used freely for AWK code, and
       there is no longer a need for the special initialization
       function in the AWK Mode hook.

       <p><li>The standard Line-up functions still haven't been
       adapted for AWK.  Some of these may work serendipitously.
       There shouldn't be any problems writing custom indentation
       functions for AWK mode.

       <p><li>AWK Font Locking still hasn't been fully integrated into
       CC Mode.  There is just a single level of font locking in AWK
       mode.

   </ul>

<?php include ("footer.h"); ?>
