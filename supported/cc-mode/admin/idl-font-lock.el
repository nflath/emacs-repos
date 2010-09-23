;;; idl-font-lock.el --- font lock specifications for CORBA IDL

;; Author: Scott Hassan <hassan@cs.stanford.edu>

(defvar idl-font-lock-keywords
  (let ((prefixes "unsigned\\|short\\|long\\|const")
	(types (concat "int\\|long\\|char\\|float\\|double\\|void\\|struct\\|"
		       "union\\|enum\\|typedef"))
	(ctoken "\\(\\sw\\|\\s_\\|[:~*&]\\)+")
	)
    (list

     (cons (concat
	    "\\b\\("
	    (mapconcat
	     'identity
	     '(
	       "any" "attribute" "boolean" "case"
	       "char" "const" "context" "default" "double"
	       "enum" "exception" "FALSE" "float" "in" "inout"
	       "interface" "long " "module" "Object" "octet"
	       "oneway" "out" "raises" "readonly" "sequence"
	       "short" "string" "struct" "switch" "TRUE"
	       "typedef" "unsigned" "union" "void"
	       )
	     "\\|")
	    "\\)[ \n\t(]")
	   1)
     (list (concat
	    "^\\(" ctoken "[ \t]+\\)?"	; type specs; there can be no
	    "\\(" ctoken "[ \t]+\\)?"	; more than 3 tokens, right?
	    "\\(" ctoken "[ \t]+\\)?"
	    "\\([*&]+[ \t]*\\)?"	; pointer
	    "\\(" ctoken "\\)[ \t]*(")	; name
	   8 'font-lock-function-name-face)

     (list (concat "^\\(typedef[ \t]+struct\\|struct\\|static[ \t]+struct\\)"
		   "[ \t]+\\(" ctoken "\\)[ \t]*\\(\{\\|$\\)")
	   2 'font-lock-function-name-face)

     )
    )
  "*Additional expressions to highlight in IDL mode.")

(put 'idl-mode 'font-lock-defaults '(idl-font-lock-keywords))

(provide 'idl-font-lock)
