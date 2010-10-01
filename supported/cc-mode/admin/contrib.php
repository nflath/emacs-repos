<!-- -*- html -*- -->
<?php
  $title = "Contributions and Links";
  include ("header.h");
?>


<h3>C# support</h3>

<p>Brad Merrill has <a
href="http://www.cybercom.net/~zbrad/DotNet/Emacs/">patched</a> CC
Mode 5.28 to support C#.  Dennis Haney has another <a
href="http://davh.dk/script/">alternative</a> in the form of a derived
mode for CC Mode 5.28.  CC Mode will hopefully get built-in support
for C#.</p>

<!-- Font locking is now part of CC Mode.

<h3>Font lock for IDL mode</h3>

<p>Three people have contributed Emacs font-lock definitions for CORBA
IDL code.  These are useful to IDL writers since CC Mode does not
contain any font-lock definitions (for IDL, or any other supported
language), and (X)Emacs itself also doesn't contain IDL font-lock
definitions by default.

<p>I'm sorry, I can't tell you which one to use.  Hopefully someday
the three alternatives will be merged and contributed to (X)Emacs so
you'll just get them automatically.

<ul>

  <li><a href="idl-font-lock.el">Scott Hassan's</a> original.

  <li><a href="idl-font-lock-2.el">Brian Ewins'</a> alternative.

  <li><a
  href="http://www.cs.utah.edu/~eeide/emacs/idl-font-lock.el.gz">A
  link to Eric Eide's</a> third alternative (gzip'd)

</ul>


<h3>Font lock for Pike mode</h3>

<p>Like IDL mode, neither CC Mode nor (X)Emacs does not come with
font-lock definitions for Pike mode.  <a href="pike.el">This
package</a> written by Per Hedbor and others provides that.

-->


<h3>Embedded SQL</h3>

<p>CC Mode doesn't handle language mixes very well, and it probably
won't for some time yet.  Therefore I'll try to put up any
contributions to help out in those areas here.  One of them is
embedded SQL, where Kevin Ruland has contributed an <a
href="plsql.txt">indentation for Oracle ProC embedded SQL</a>.


<!-- The site no longer responds, and Google can't find it anywhere
     else on the net.

<h3>Other stuff</h3>

<p>There's a <a
href="http://apache.bsilabs.com/~tim/cc-mode/description.html">patch</a>
by Timothy M. Schaeffer that tries to achieve tab-size independent
indentation.</p>

-->

<?php include ("footer.h"); ?>
