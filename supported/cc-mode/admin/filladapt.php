<!-- -*- html -*- -->
<?php
  $title = "Using Filladapt in CC Mode";
  include ("header.h");
?>

<p>As of version 5.26, CC Mode uses adaptive filling for text in
comments and string literals.  Thus it's now possible to use Kyle
E. Jones' Filladapt package (<a
href="http://wonderworks.com/">http://wonderworks.com/</a>) inside CC
Mode to get better handling of e.g. bulleted lists in comments.

<p>To set up Filladapt for use with CC Mode:

<ol>

  <p><li>Make sure you have Filladapt installed.  It's available from
  <a href="http://wonderworks.com/">the canonical web site</a>.  You
  can also download <a href="filladapt-orig.el">version 2.12</a> from
  the CC Mode site.

  <p>You might instead want to download a <a
  href="filladapt.el">patched version</a> of Filladapt that corrects a
  small problem (see below).  The suggested <a
  href="filladapt.el.diff">patch</a> is also available in context diff
  form if you prefer to patch it yourself.

  <p><li>Configure the Filladapt variables for use in CC Mode.  The
  easiest way to do this is to add something like this your
  <code>.emacs</code>:

  <pre>
(defun my-c-mode-common-hook ()
  (c-setup-filladapt)
  (filladapt-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)</pre>

  <p>This uses the <code>c-setup-filladapt</code> convenience function
  that comes with CC Mode to modify the Filladapt variables according
  to the comment syntax and <code>c-comment-prefix-regexp</code>.

</ol>

<h3>Filladapt problems</h3>

<p>There is a minor problem, or rather lack of a feature, in the
current version of Filladapt (2.12 when this is written) that makes it
do a poor job when the regexp on <code>c-comment-prefix-regexp</code>
matches the empty string.  The effect is that Filladapt fills only
parts of a paragraph, or nothing at all.  You can fix this problem in
three ways:

<ul>

  <p><li>Make sure <code>c-comment-prefix-regexp</code> can't match
  the empty string (by default it does).  This is probably not a good
  idea since many block comment styles use nothing but whitespace
  before the text.

  <p><li>Set up the Filladapt variables yourself instead of using
  <code>c-setup-filladapt</code>.  You'll have to make sure those
  variables agrees with <code>c-block-comment-prefix</code> and
  <code>c-comment-prefix-regexp</code>, or else you'll get very
  awkward behavior when editing comments.

  <p><li>Download this <a href="filladapt.el.diff">patch</a> and apply
  it to the original <a href="filladapt-orig.el">2.12 version</a>.
  There's also an already <a href="filladapt.el">patched</a> version
  available.

  <p>The patch adds a feature to Filladapt which makes it ignore
  tokens that matches the empty string when it analyzes the fill
  prefix.  A variable <code>filladapt-token-match-empty</code>
  controls which tokens are allowed to match the empty string
  (normally only <code>beginning-of-line</code> and
  <code>end-of-line</code> are meaningful).

  <p>To the best of my knowledge, this patch doesn't cause any adverse
  side effects in other modes.  On the contrary, it's quite possible
  that it fixes similar problems elsewhere too.  The patch has been
  submitted to Kyle E. Jones, and hopefully it will be included in a
  future version of Filladapt.

</ul>

<?php include ("footer.h"); ?>
