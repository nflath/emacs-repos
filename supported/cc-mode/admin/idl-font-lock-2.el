;;; idl-font-lock.el --- font lock specifications for CORBA IDL

;; Author: Brian Ewins <Brian.Ewins@gssec.bt.co.uk>
(defconst idl-font-lock-keywords-1 nil
 "For consideration as a value of `idl-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst idl-font-lock-keywords-2 nil
 "For consideration as a value of `idl-font-lock-keywords'.
This adds highlighting of types and identifier names.")

(defconst idl-font-lock-keywords-3 nil
 "For consideration as a value of `idl-font-lock-keywords'.
This adds highlighting of idldoc documentation tags, such as @see.")


(defvar idl-font-lock-type-regexp
  (concat "\\<\\(any\\|boolean\\| char\\|double\\|float"
	  "\\|long\\|octet\\|short\\|string\\|void\\)\\>")
  "Regexp which should match a primitive IDL type.")

(defvar idl-font-lock-identifier-regexp
  "\\<\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\>"
  "Regexp which should match all IDL identifiers.")
  
(defvar idl-font-lock-class-name-regexp
  "\\<\\([A-Z][a-zA-Z_0-9]*\\)\\>"
  "Regexp which should match an interface, enum, struct, union name.
The name is assumed to begin with a capital letter.")


(defvar idl-modifier-regexp
       (concat "\\<\\(attribute\\|const\\|in\\|inout\\|"
       "oneway\\|out\\|readonly\\|unsigned\\)\\>")
       "Regexp to match modifiers to types in IDL.") 

;; Basic font-lock support:
(setq idl-font-lock-keywords-1
  (list
   ;; Keywords:
   ;; opaque is not included here because its not standard idl, yet.
   ;; this is debatable...
   (list        
    (concat
     "\\<\\("  
     "case\\|" "context\\|" "default\\|" 
     "enum\\|" "exception\\|" "interface\\|" "module\\|" 
     "Object\\|" "raises\\|" "sequence\\|" "struct\\|" 
     "switch\\|" "typedef\\|"  "union\\|" "\\)\\>" )
    '(1 font-lock-keyword-face))

   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   ;; in IDL this picks up #pragma.
   '("^\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face nil t))
   
   ;; Modifiers:
   (list idl-modifier-regexp '(1 font-lock-type-face))
   
   ;; Special constants:
   ;; A hae me doots - the old defn had capitaliased boolean consts but
   ;; CORBA doesnt use these?I've left in both to be safe.
   '("\\<\\(FALSE\\|false\\|nil\\|TRUE\\|true\\)\\>" 
     (1 font-lock-keyword-face))
   
   ;; Class names:
   (list (concat "\\<\\(interface"
		 "\\|enum"
		 "\\|struct"
		 "\\|union"
		 "\\|exception"
		 "\\)\\>\\s *" 
		 idl-font-lock-identifier-regexp)
	 '(2 font-lock-function-name-face))
   (list 
     (concat "^\\s *\\<\\(typedef\\)\\>.*"
	     idl-font-lock-identifier-regexp "\\s *;")
     '(2 font-lock-function-name-face))
   
   ;; Module declarations:
   (list (concat "\\<\\(module\\)\\>\\s *"
                       idl-font-lock-identifier-regexp)
               '(2 font-lock-reference-face)
               (list (concat
                      "\\=::\\(" idl-font-lock-identifier-regexp "\\)")
                     nil nil '(1 (let ((c (char-after (match-end 0))))
                                   (if (and (characterp c)
                                            (= c ?:))
                                       'font-lock-reference-face
                                     'font-lock-type-face)))))
   ;; Methods:
   (list (concat "\\(" idl-font-lock-type-regexp "\\|"
		 idl-font-lock-class-name-regexp "\\)"
		 "\\s *"
		 idl-font-lock-identifier-regexp "\\s *\(")
	 '(4 font-lock-function-name-face))

   ;; Case statements:
   ;; In Idl, any constant expression is allowed. (?)
   '("^\\s *\\<case\\>\\s *\\(.*\\):" 1 font-lock-reference-face)


   ));end idl-font-lock-keywords-1

;; Types and declared variable names:
(setq idl-font-lock-keywords-2
  (append 
   idl-font-lock-keywords-1
   
   ;; Hack: vars are last word on lines ending with semicolons.
   ;; (the exception is typedefs, which are in keywords-1)
   (list 
    (list 
     (concat idl-font-lock-identifier-regexp "\\s *;")
     1 font-lock-variable-name-face)
   
    ;; Keywords followed by a type list:
    ;; 'raises' is always followed by a list of exceptions.
    ;; also 'interface FOO : ...interfaces...'
    ;; the code below fontifies it like wot java-mode does.
    ;; it still has problems with identfiers which begin '::'.
    (list
     (concat "\\<\\(raises\\s *("
	     "\\|interface\\s *[a-zA-Z_][a-zA-Z_0-9]*"
	     "\\s *:\\)\\s *"
	     idl-font-lock-identifier-regexp)
     '(2 (if (= (char-after (match-end 0)) ?:)
	     font-lock-reference-face font-lock-type-face))
     (list (concat "\\=\\(::\\|\\s *\\(,\\)\\s *\\)"
		   idl-font-lock-identifier-regexp)
	   '(goto-char (match-end 0)) nil
	   '(3 (if (= (char-after (match-end 0)) ?:)
		   font-lock-reference-face font-lock-type-face))))

    ;; primitive types, can't be confused with anything else.
    (list idl-font-lock-type-regexp
		 1 font-lock-type-face)
    
    )))

;; next set not done yet.
;; should include keywords for idldoc.
(setq idl-font-lock-keywords-3
      idl-font-lock-keywords-2 )

(defvar idl-font-lock-keywords idl-font-lock-keywords-2
  "Additional expressions to highlight in IDL mode.")

(put 'idl-mode 'font-lock-defaults 
     '((idl-font-lock-keywords
        idl-font-lock-keywords-1 idl-font-lock-keywords-2
        idl-font-lock-keywords-3)
       nil nil ((?_ . "w")) beginning-of-defun
       (font-lock-mark-block-function . mark-defun)))

(provide 'idl-font-lock)
