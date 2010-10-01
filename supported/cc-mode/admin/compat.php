<!-- -*- html -*- -->
<?php
  $title = "Compatibility Notes";
  include ("header.h");
?>

<p>CC Mode should work out of the box with Emacs &gt;= 20.4, and
XEmacs &gt;= 21.4.  Emacs 19.34 and XEmacs 19.15 are no longer
supported, since CC Mode now uses the <code>syntax-table</code> text
property, which first appeared in Emacs 20.1 and XEmacs 21.4.

<p>As of December 1 2005, <i>Subword Minor Mode</i> doesn't (yet) work
in Emacs 20.n.

<p>If you are going to be editing AWK buffers, note that your (X)Emacs
might be configured to use the older AWK Mode (in the file
<code>awk-mode.elc</code>) supplied with that (X)Emacs.  The file
README in the CC Mode distribution tells you how to configure your
(X)Emacs to use CC Mode for AWK buffers.

<p>If you're using Emacs 21.3 or earlier, or XEmacs 21.3 or earlier,
then the file <code>cc-fix.el(c)</code> is needed and will be loaded
automatically.  It corrects for some bugs in those versions, and also
contains compatibility glue for missing functions in older
versions. <code>cc-fix.el(c)</code> is not needed for later versions.

<?php include ("footer.h"); ?>
