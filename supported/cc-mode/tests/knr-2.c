/* NB: Earlier less thorough tests coped better with the macro in this
 * position and recognized the arglist with func-decl-cont. The
 * current test checks the declaration more carefully and won't allow
 * this. The right way to cope with it is to introduce some kind of
 * system where CC Mode can be configured to take the syntactic
 * meaning of certain macro expansions into account. */
DEFUN ("lax-plist-put", Flax_plist_put, Slax_plist_put, 3, 3, 0,
       doc: /* ...
	     ... */)
(plist, prop, val)
Lisp_Object plist;
register Lisp_Object prop;
Lisp_Object val;
{
}
