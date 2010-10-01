<!-- -*- html -*- -->
<?php
  $title = "Installation Notes";
  include ("header.h");
?>

<h3>Byte Compiling</h3>

<p>It is <em>highly</em> recommended that you byte-compile CC Mode for
performance reasons.  Running CC Mode non-byte-compiled is not
supported.

<p>You can compile CC Mode in the same way as any other package.  To
compile it from a running (X)Emacs session:

<pre>
M-0 M-x byte-recompile-directory RET /path/to/cc-mode RET</pre>

<p>To compile CC Mode from the shell:

<pre>
% cd /path/to/cc-mode
% $(EMACS) -batch -no-site-file -q -f batch-byte-compile cc-*.el</pre>

where <code>$(EMACS)</code> is either <code>emacs</code> or
<code>xemacs</code> depending on the flavor you use.

<h3>Installing</h3>

<p>Put the compiled files somewhere (X)Emacs will find them, i.e. in
some path that's in the <code>load-path</code> variable.  You must
make sure they are found before any CC Mode files which are
distributed with (X)Emacs.  A directory has higher precendence than
all directories after it in the <code>load-path</code> list.

If you're going to be using AWK Mode, insert the following line
into your <code>.emacs</code> or <code>init.el</code> file:

<pre>
(autoload 'awk-mode "cc-mode" nil t)</pre>

This will cause (X)Emacs to use the new AWK Mode for AWK files, rather
than the older mode contained in the file <code>awk-mode.elc</code>.

<p>To test that you have things set up correctly, visit a C file and
then type:<br>

<pre>
M-x c-version RET
=> Using CC Mode version 5.XX</pre>

where <code>XX</code> is the correct minor revision number.

<?php include ("footer.h"); ?>
