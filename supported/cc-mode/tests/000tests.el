;; Regression test suite harness

;; You can run the regression test either within your current X/Emacs
;; session, or via batch mode.  I usually run the latter first, to
;; make sure my changes haven't messed anything up.  If this passes,
;; everything is fine.  If it fails, I run the test in the current
;; session until I fix the breakage.

;; To run in the current session, make sure you have all the latest
;; definitions of any functions you've changed.  Then load this file
;; and type:
;;
;;    M-x do-all-tests RET
;;
;; This regression tests all the files in the current directory.  Note
;; that this form of testing takes over X/Emacs until the test
;; completes, and the tests can take a while!

;; To run in batch mode, make sure all the changed .el files are byte
;; compiled and make sure they will be found first on your load-path,
;; then at the shell prompt do this:
;;
;;    % cd .../cc-mode/tests
;;    % xemacs -batch -l 000tests.el -f do-all-tests
;;
;; Obviously replacing `xemacs' with `emacs' if necessary.  To test a single
;; file in batch mode, do this:
;;
;;    % cd .../cc-mode/tests
;;    % xemacs -batch -l 000tests.el --eval '(do-one-test "statement-7.c")'

;; The tests stop if any regression is found.  With in-session
;; testing, you can restart the tests from where it left off by just
;; doing a M-x do-all-tests again.  Batch testing starts over from the
;; beginning.

;; Whenever I add a new syntactic symbol, or add a new case to the big
;; c-guess-basic-syntax cond, or find and fix a new breakage, I create
;; a new regression test by:
;;
;; 1) creating a small, but complete test case in a file with the
;;    proper extension.  This file must have a unique name within the
;;    tests directory (sans the extension).
;; 2) Create a `results' (.res) file with the base name of the file,
;;    appended with .res by doing `M-x resfile'.  The created or
;;    changed results file will be shown in another window.  Verify
;;    that the results are what you expect, then save both files, and
;;    check them into CVS.

;; The test suite can also check the fontification done by the
;; font-lock settings.  If there is a file with the extension .face,
;; the fontification will be tested against it.  Such files are
;; generated with `M-x facefile'.

;; Some times the tests will fail without an actual regression being
;; introduced.  This might happen if, e.g. the default Java style
;; changes.  In this case, you can modify the corresponding .res file
;; instead of fixing the regression, but be VERY careful when doing
;; this.  Make sure you know this is what you want to do!
;;
;; To do this, you want to make sure the test source is indented
;; properly, then regenerate the .res and/or .face files using
;; `resfile' or `facefile', respectively.  There's also a convenience
;; function `shift-res-offsets' that shifts the positions in a .res
;; file after a certain point, which is useful if the length of a line
;; inside a test changes for some reason.

(defvar cc-test-dir nil)
(eval-and-compile
  (setq cc-test-dir
	(let ((file (if (and (boundp 'byte-compile-dest-file)
			     (stringp byte-compile-dest-file))
			byte-compile-dest-file
		      load-file-name)))
	  (if file
	      (file-name-directory file)
	    default-directory)))
  (let ((srcdir (expand-file-name (concat cc-test-dir ".."))))
    (setq load-path (cons srcdir load-path))))

(require 'compile)
(require 'cl)
(require 'font-lock)
(require 'cc-defs)

(defconst cc-test-verbose nil
  "Turn on verbose logging.
Status messages aren't overwritten in batch mode.")

(defconst cc-test-dump-backtraces nil
  "Dump backtraces for evaluation errors.")

(defconst cc-test-benchmark-mode nil
  "Turn off checking of the test results.
Useful when measuring performance, especially when comparing with
simplifications with lesser accuracy.")

;; Set to t to extend the set of faces so that all syntactic parts are
;; tested accurately.  This does otoh hide problems with missing
;; faces.
(defconst cc-test-extend-faces t)

;; Silence the compiler.
(defvar font-lock-fontify-buffer-function)
(defvar deactivate-mark)
(defvar inhibit-point-motion-hooks)
(defvar font-lock-global-modes)
(defvar c-debug-parse-state)

;; Turn on debugging tools in CC Mode.
(setq c-debug-parse-state t)

;; `font-lock-make-faces' is used in Emacs 19.34 to initialize the
;; faces and it uses some X functions.  If we're running
;; noninteractively we don't have any X connection, so provide some
;; dummies.
(when (and (fboundp 'font-lock-make-faces)
	   (not window-system))
  (flet ((x-get-resource (&rest ignored))
	 (x-display-color-p (&rest ignored))
	 (x-display-grayscale-p (&rest ignored))
	 (x-color-values (&rest ignored) '(0 0 0)))
    (font-lock-make-faces)))

(when cc-test-extend-faces
  ;; Make sure all used faces are unique before loading cc-fonts.  We
  ;; might be screwed if it's already loaded - the check for ambiguous
  ;; faces below will complain in that case.

  (defun c-test-construct-face (face &rest fallback-faces)
    ;; If `face' doesn't exist, define it.  The face definition is
    ;; copied from the first of `fallback-faces' that exists.
    (unless (c-face-name-p face)
      (catch 'create-face
	(while fallback-faces
	  (let ((fallback-face (pop fallback-faces)))
	    (when (c-face-name-p fallback-face)
	      (copy-face fallback-face face)
	      (throw 'create-face t))))
	(make-face face))
      (eval `(defvar ,face nil))
      (set face face)))
  (c-test-construct-face 'font-lock-doc-face 'font-lock-comment-face)
  (c-test-construct-face 'font-lock-preprocessor-face
			 'font-lock-builtin-face
			 'font-lock-reference-face)
  (c-test-construct-face 'font-lock-constant-face 'font-lock-reference-face)
  (c-test-construct-face 'font-lock-label-face 'font-lock-reference-face)
  (c-test-construct-face 'font-lock-doc-markup-face 'font-lock-reference-face)
  (c-test-construct-face 'font-lock-negation-char-face 'default)

  ;; In Emacs face names are resolved as variables which can point to
  ;; another face.  Make sure we don't have such indirections when we
  ;; create or check against .face files.
  (mapcar (lambda (face)
	    (when (and (boundp face)
		       (not (eq (symbol-value face) face)))
	      (copy-face (symbol-value face) face)
	      (set face face)))
	  '(font-lock-comment-face
	    font-lock-string-face
	    font-lock-keyword-face
	    font-lock-function-name-face
	    font-lock-variable-name-face
	    font-lock-type-face
	    font-lock-reference-face
	    font-lock-doc-string-face
	    font-lock-constant-face
	    font-lock-preprocessor-face
	    font-lock-builtin-face
	    font-lock-warning-face
	    font-lock-negation-char-face
	    font-lock-comment-delimiter-face)))

(require 'cc-mode)

;; Alist that maps the symbols used for faces in the .face files to
;; their actual names known by font-lock.
(defconst cc-test-face-alist
  `((reg . nil)
    (cmt . font-lock-comment-face)
    (str . font-lock-string-face)
    (key . font-lock-keyword-face)
    (fun . font-lock-function-name-face)
    (var . font-lock-variable-name-face)
    (typ . font-lock-type-face)
    (con . ,c-constant-face-name)
    (ref . ,c-reference-face-name)
    (doc . ,c-doc-face-name)
    (dmk . ,c-doc-markup-face-name)
    (lbl . ,c-label-face-name)
    (cpp . ,c-preprocessor-face-name)
    (err . font-lock-warning-face)
    (nbs . c-nonbreakable-space-face)
    (neg . font-lock-negation-char-face)
    (ant . c-annotation-face)
    ;; The following is used on the comment delimiters themselves in
    ;; Emacs >= 22.  Just make it an alias for the comment face to
    ;; keep compatibility with our current set of test cases.  (It's
    ;; not the kind of fontification we want to test here either,
    ;; since we're mainly interested in the fontification rules we
    ;; define in cc-fonts.el.)
    (cmt . font-lock-comment-delimiter-face)))

(if cc-test-extend-faces
    ;; Check that we don't have duplicates.
    (let ((alist cc-test-face-alist) elem facename)
      (while alist
	(setq elem (car alist)
	      alist (cdr alist))
	(put (car elem) 'cc-test-face-alias (car elem))
	(unless (eq (car elem) 'reg)
	  (when (and (setq facename (get (cdr elem) 'cc-test-face-name))
		     (not (eq facename (car elem))))
	    (message (concat "Ambiguous face %s - can be both %s and %s"
			     " (cc-fonts loaded too early?)")
		     (cdr elem) facename (car elem))
;; 	    (error (concat "Ambiguous face %s - can be both %s and %s"
;; 			   " (cc-fonts loaded too early?)")
;; 		   (cdr elem) facename (car elem))
	    )
	  (put (cdr elem) 'cc-test-face-name (car elem)))))

  ;; Fix aliases.
  (let ((alist cc-test-face-alist) elem facename)
    (while alist
      (setq elem (car alist)
	    alist (cdr alist))
      (put (car elem) 'cc-test-face-alias (car elem))
      (unless (eq (car elem) 'reg)
	(if (and (setq facename (get (cdr elem) 'cc-test-face-name))
		 (not (eq facename (car elem))))
	    (put (car elem) 'cc-test-face-alias facename)
	  (put (cdr elem) 'cc-test-face-name (car elem)))))))

(defconst cc-test-emacs-features
  (let ((features c-emacs-features))
    (setq features (cons (if (string-match "XEmacs" emacs-version)
			     ;; (featurep 'xemacs) doesn't work here
			     ;; for some reason.
			     'xemacs
			   'emacs)
			 features)
	  features (cons (intern (concat (symbol-name (car features))
					 (format "-%s" emacs-major-version)))
			 features)
	  features (cons (intern (concat (symbol-name (car features))
					 (format "-%s" emacs-minor-version)))
			 features))
    features))

(defvar cc-test-skip nil
  "If this is a list that contains any symbol also present on
`cc-test-emacs-features' then any test errors are ignored.  Intended
to be set as a file local variable.")

(defun cc-test-force-font-lock-buffer ()
  ;; Try to forcibly font lock the current buffer, even in batch mode.
  ;; We're doing really dirty things to trick font-lock into action in
  ;; batch mode in the different emacsen.
  (let ((orig-noninteractive-function
	 (and (fboundp 'noninteractive)
	      (symbol-function 'noninteractive)))
	(orig-noninteractive-variable
	 (and (boundp 'noninteractive)
	      (symbol-value 'noninteractive)))
	;; font-lock in XEmacs 19 looks at a variable named `noninteractive'.
	(noninteractive nil))
    (unwind-protect
	(progn
	  (when orig-noninteractive-function
	    ;; XEmacs (at least 21.4) calls `noninteractive' to check
	    ;; for batch mode, so we let it lie.
	    (fset 'noninteractive (lambda () nil)))
	  (font-lock-mode 1)
	  (unless (or (get-text-property (point-min) 'face)
		      (next-single-property-change (point-min) 'face))
	    ;; Some emacsen have already fontified the buffer above,
	    ;; but others need some more coercion..
	    (let (;; Avoid getting some lazy fontification package that
		  ;; might decide that nothing should be done.
		  (font-lock-fontify-buffer-function
		   'font-lock-default-fontify-buffer))
	      (font-lock-fontify-buffer))))
      (when orig-noninteractive-function
	(fset 'noninteractive orig-noninteractive-function)))))

(defconst cc-test-teststyle
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-comment-prefix-regexp       . "\\(//+\\|\\**\\)[.!|]?")
    (c-hanging-braces-alist        . ((block-open after)
				      (brace-list-open)
				      (substatement-open after)
				      (inexpr-class-open after)
				      (inexpr-class-close before)
				      ))
    (c-hanging-colons-alist        . ((member-init-intro before)
				      (inher-intro)
				      (case-label after)
				      (label after)
				      (access-key after)))
    (c-cleanup-list                . (scope-operator
				      empty-defun-braces
				      defun-close-semi))
    (c-offsets-alist
     . ((string                . c-lineup-dont-change)
	(c                     . c-lineup-C-comments)
	(defun-open            . 0)
	(defun-close           . 0)
	(defun-block-intro     . +)
	(class-open            . 0)
	(class-close           . 0)
	(inline-open           . 0)
	(inline-close          . 0)
	(func-decl-cont        . +)
	(knr-argdecl-intro     . +)
	(knr-argdecl           . 0)
	(topmost-intro         . 0)
	(topmost-intro-cont    . c-lineup-topmost-intro-cont)
	(member-init-intro     . +)
	(member-init-cont      . c-lineup-multi-inher)
	(inher-intro           . +)
	(inher-cont            . c-lineup-multi-inher)
	(block-open            . 0)
	(block-close           . 0)
	(brace-list-open       . 0)
	(brace-list-close      . 0)
	(brace-list-intro      . +)
	(brace-list-entry      . 0)
	(statement             . 0)
	(statement-cont        . +)
	(statement-block-intro . +)
	(statement-case-intro  . +)
	(statement-case-open   . 0)
	(substatement          . +)
	(substatement-open     . +)
	(substatement-label    . *)
	(case-label            . 0)
	(access-label          . -)
	(label                 . *)
	(do-while-closure      . 0)
	(else-clause           . 0)
	(catch-clause          . 0)
	(comment-intro         . c-lineup-comment)
	(arglist-intro         . +)
	(arglist-cont          . (c-lineup-gcc-asm-reg 0))
	(arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist))
	(arglist-close         . +)
	(stream-op             . c-lineup-streamop)
	(inclass               . +)
	(cpp-macro             . [0])
	(cpp-macro-cont        . +)
	(cpp-define-intro      . (c-lineup-cpp-define +))
	(friend                . 0)
	(objc-method-intro     . [0])
	(objc-method-args-cont . c-lineup-ObjC-method-args)
	(objc-method-call-cont . c-lineup-ObjC-method-call)
	(extern-lang-open      . 0)
	(extern-lang-close     . 0)
	(inextern-lang         . +)
	(namespace-open        . 0)
	(namespace-close       . 0)
	(innamespace           . +)
	(module-open           . 0)
	(module-close          . 0)
	(inmodule              . 0)
	(composition-open      . 0)
	(composition-close     . 0)
	(incomposition         . 0)
	(template-args-cont    . (c-lineup-template-args +))
	(inlambda              . c-lineup-inexpr-block)
	(lambda-intro-cont     . +)
	(inexpr-statement      . +)
	(inexpr-class          . +)
	))
    (c-echo-syntactic-information-p . t)
    (c-indent-comment-alist . nil)
    )
  "Style for testing.")

(defconst cc-test-javateststyle
  '("teststyle"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist . ((inline-open . 0)
			(topmost-intro-cont    . +)
			(statement-block-intro . +)
			(knr-argdecl-intro     . 5)
			(substatement-open     . +)
			(label                 . 0)
			(statement-case-open   . +)
			(statement-cont        . +)
			(arglist-intro  . c-lineup-arglist-intro-after-paren)
			(arglist-close  . c-lineup-arglist)
			(access-label   . 0)
			(inher-cont     . c-lineup-java-inher)
			(func-decl-cont . c-lineup-java-throws)
			))
    )
  "Style for testing Java code.")

(c-add-style "teststyle" cc-test-teststyle)
(c-add-style "javateststyle" cc-test-javateststyle)

(defconst cc-test-empty-string-regexp
  (concat "\\(" c-string-limit-regexp "\\)"
	  "\\(" c-string-limit-regexp "\\)"))

(defun cc-test-record-syntax (testbuf resultbuf no-error error-fn)
  (set-buffer testbuf)
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((buffer-read-only nil)
	   (syntax
	    (if no-error
		(condition-case err
		    (c-guess-basic-syntax)
		  (error
		   (funcall error-fn err)
		   ""))
	      (c-guess-basic-syntax))))
      (set-buffer resultbuf)
      (insert (format "%s" syntax) "\n")
      (set-buffer testbuf))
    (forward-line 1)))

(defun cc-test-convert-to-rel-offsets (resbuf testbuf)
  ;; resbuf is a buffer assumed to contain recorded syntax.  All
  ;; absolute positions in it are converted to relative with the
  ;; syntax "<l,c>" where "l" is the number of lines back from the
  ;; current line, and "c" is the column.  Positions already on the
  ;; relative form aren't touched.  TESTBUF is used to convert to
  ;; lines and columns.

  (set-buffer resbuf)
  (goto-char (point-min))
  (save-excursion
    (set-buffer testbuf)
    (goto-char (point-min)))

  (let ((last-pos (point)))
    (while (re-search-forward "\\(\\=\\|[^<]\\)\\([0-9]+\\)" nil t)
      (let ((anchor-pos (string-to-int (match-string 2)))
	    moved-lines anchor-rel-line anchor-col)

	;; Why is count-lines a bloody mess? :P
	(setq moved-lines (count-lines (c-point 'bol last-pos)
				       (c-point 'bol)))

	(save-excursion
	  (set-buffer testbuf)
	  (if (> moved-lines 0)
	      (forward-line moved-lines))
	  (setq anchor-rel-line (count-lines anchor-pos (point))
		anchor-col (save-excursion
			     (goto-char anchor-pos)
			     (current-column))))

	(delete-region (match-beginning 2) (match-end 2))
	(insert (format "<%d,%d>" anchor-rel-line anchor-col))

	(setq last-pos (point))))))

(defun cc-test-convert-to-abs-offsets (resbuf testbuf)
  ;; resbuf is a buffer assumed to contain recorded syntax.  All
  ;; relative positions in it are converted to absolute.  Positions
  ;; already on the absolute form aren't touched.  TESTBUF is used to
  ;; convert from lines and columns.

  (set-buffer resbuf)
  (goto-char (point-min))
  (save-excursion
    (set-buffer testbuf)
    (goto-char (point-min)))

  (let ((last-pos (point)))
    (while (re-search-forward
	    ;; A relative line should never be negative, but if it is
	    ;; then we don't want it to break here.
	    "<\\(-?[0-9]+\\),\\([0-9]+\\)>" nil t)
      (let ((anchor-rel-line (string-to-int (match-string 1)))
	    (anchor-col (string-to-int (match-string 2)))
	    moved-lines anchor-pos)

	;; Why is count-lines a bloody mess? :P
	(setq moved-lines (count-lines (c-point 'bol last-pos)
				       (c-point 'bol)))

	(save-excursion
	  (set-buffer testbuf)
	  (if (> moved-lines 0)
	      (forward-line moved-lines))
	  (setq anchor-pos
		(save-excursion
		  (forward-line (- anchor-rel-line))
		  (let (col)
		    (while (and (< (setq col (current-column))
				   anchor-col)
				(not (eolp)))
		      (forward-char))
		    ;; Don't convert the relative offset if we can't
		    ;; find the right column.
		    (and (eq col anchor-col)
			 (point))))))

	(when anchor-pos
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (format "%d" anchor-pos)))

	(setq last-pos (point))))))

(defun cc-test-record-faces (testbuf facebuf check-unknown-faces)
  (set-buffer testbuf)
  (let (faces prev-faces (pos (point)) facenames lines col preceding-entry
	(emacs-strings (not (featurep 'xemacs)))
	in-string)

    (while (progn
	     (unless (eq (setq faces (get-text-property pos 'face)) prev-faces)
	       (setq prev-faces faces)

	       (setq facenames
		     (mapcar
		      (lambda (face)
			;; Translate each face to the short names used
			;; in the .face files and check that we expect
			;; it.
			(let ((name (get face 'cc-test-face-name)))
			  (if name
			      name
			    (if check-unknown-faces
				(error "Unknown face %s" face)
			      face))))
		      (if (listp faces) faces (list faces))))
	       (unless facenames
		 (setq facenames '(reg)))

	       (catch 'record-face
		 ;; XEmacs does not highlight the quotes surrounding
		 ;; string literals as strings, while Emacs does.  The
		 ;; .face files follow the XEmacs variety since the
		 ;; Emacs behavior can be more easily converted for
		 ;; empty strings than the other way around.
		 ;;
		 ;; NB: The case when there are several overlaid faces
		 ;; isn't handled but that shouldn't happen in strings.
		 (when emacs-strings
		   (if in-string
		       (progn
			 (backward-char)
			 (unless (looking-at c-string-limit-regexp)
			   (forward-char)))
		     (when (and (equal facenames '(str))
				(looking-at c-string-limit-regexp))
		       (when (looking-at cc-test-empty-string-regexp)
			 ;; Ignore empty strings altogether.
			 (while (progn
				  (goto-char (match-end 0))
				  (looking-at cc-test-empty-string-regexp)))
			 (throw 'record-face nil))
		       (forward-char)))
		   (setq in-string (equal facenames '(str))))

		 (when (prog1 (and col (>= col (current-column)))
			 (setq col (current-column))
			 (set-buffer facebuf))
		   ;; Since we might modify positions above we need
		   ;; to deal with new face change entries that
		   ;; override earlier ones.
		   (let (pos)

		     (while (and (not (bolp))
				 (progn
				   (c-backward-sexp)
				   (setq pos (point)
					 preceding-entry (read facebuf))
				   (>= (car preceding-entry) col)))
		       (goto-char pos)
		       (setq preceding-entry nil))
		     (delete-region (point) (c-point 'eol))

		     (unless preceding-entry
		       (save-excursion
			 (c-safe
			   (c-backward-sexp)
			   (setq preceding-entry (read facebuf)))))))

		 ;; Don't add a new entry if the preceding one has the
		 ;; same face.
		 (when (and preceding-entry
			    (equal (cdr preceding-entry) facenames))
		   (set-buffer testbuf)
		   (throw 'record-face nil))

		 (insert (format "%s" (setq preceding-entry
					    (cons col facenames))))
		 (set-buffer testbuf)))

	     (setq pos (next-single-property-change (point) 'face))

	     ;; Insert the same amount of line breaks in facebuf as
	     ;; we've passed in testbuf (count-lines is clumsy here).
	     (setq lines 0)
	     (while (re-search-forward "[\n\r]" pos 'move)
	       (setq lines (1+ lines)))
	     (when (> lines 0)
	       (set-buffer facebuf)
	       (insert-char ?\n lines)
	       (set-buffer testbuf)
	       (setq col nil))

	     pos))))

(defvar cc-test-finished-tests nil)
(defvar cc-test-comp-buf nil)
(defvar cc-test-comp-win nil)

(defvar cc-test-last-backtrace nil)

(defconst cc-test-clear-line-string
  (if cc-test-verbose "" (concat "\r" (make-string 60 ?\ ) "\r")))

(defun cc-test-message (msg &rest args)
  (if noninteractive
      (send-string-to-terminal (concat cc-test-clear-line-string
				       (apply 'format msg args) "\n"))
    (apply 'message msg args)))

(defun cc-test-tmp-message (msg &rest args)
  (if cc-test-verbose
      (apply 'cc-test-message msg args)
    (if noninteractive
	(send-string-to-terminal (concat cc-test-clear-line-string
					 (apply 'format msg args) "  "))
      (apply 'message msg args))))

(defun cc-test-log (msg &rest args)
  (if cc-test-comp-buf
      (save-excursion
	(save-selected-window
	  (select-window cc-test-comp-win)
	  (insert (apply 'format msg args))
	  (recenter -1)
	  (sit-for 0)
	  (insert "\n")))
    (apply 'cc-test-message msg args)))

(defun make-test-buffers (filename)
  (let ((testbuf (get-buffer-create "*cc-test*"))
	(resfile (concat (file-name-sans-extension filename) ".res"))
	(facefile (concat (file-name-sans-extension filename) ".face"))
	exp-syntax-buf res-syntax-buf exp-faces-buf res-faces-buf
	(enable-local-eval t))

    ;; Old emacsen displays eight bit chars as octal escapes, which
    ;; makes (current-column) misbehave in `cc-test-record-faces'.
    (when (= emacs-major-version 19)
      (standard-display-european 1))

    ;; Setup the test file buffer.
    (set-buffer testbuf)
    (kill-all-local-variables)
    (buffer-disable-undo testbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename)
    (setq buffer-read-only t) ; Test that we make no (hidden) changes.
    (goto-char (point-min))
    (let ((c-default-style "TESTSTYLE")
	  c-mode-hook c++-mode-hook objc-mode-hook c-mode-common-hook)
      (cond
       ((string-match "\\.cc$" filename) (c++-mode))
       ((string-match "\\.m$" filename) (objc-mode))
       ((string-match "\\.java$" filename)
	(setq c-default-style "JAVATESTSTYLE")
	(java-mode))
       ((string-match "\\.pike$" filename) (pike-mode))
       ((string-match "\\.idl$" filename) (idl-mode))
       ((string-match "\\.awk$" filename) (awk-mode))
       (t (c-mode))))
    (hack-local-variables)

    ;; Setup the expected and resulting analysis buffers.
    (when (file-exists-p resfile)
      (setq exp-syntax-buf (get-buffer-create "*cc-expected-syntax*"))
      (set-buffer exp-syntax-buf)
      (buffer-disable-undo exp-syntax-buf)
      (erase-buffer)
      (insert-file-contents resfile)
      (cc-test-convert-to-abs-offsets exp-syntax-buf testbuf)
      (goto-char (point-min))

      (setq res-syntax-buf (get-buffer-create "*cc-resulting-syntax*"))
      (set-buffer res-syntax-buf)
      (buffer-disable-undo res-syntax-buf)
      (erase-buffer))

    ;; Setup the expected and resulting faces buffers.
    (when (file-exists-p facefile)
      (setq exp-faces-buf (get-buffer-create "*cc-expected-faces*"))
      (set-buffer exp-faces-buf)
      (buffer-disable-undo exp-faces-buf)
      (erase-buffer)
      (insert-file-contents facefile)
      (goto-char (point-min))

      (setq res-faces-buf (get-buffer-create "*cc-resulting-faces*"))
      (set-buffer res-faces-buf)
      (buffer-disable-undo res-faces-buf)
      (erase-buffer))

    (list testbuf res-syntax-buf exp-syntax-buf res-faces-buf exp-faces-buf)))

(defun kill-test-buffers ()
  (let (buf)
    (if (setq buf (get-buffer "*cc-test*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-expected-syntax*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-resulting-syntax*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-expected-faces*"))
	(kill-buffer buf))
    (if (setq buf (get-buffer "*cc-resulting-faces*"))
	(kill-buffer buf))))

(defun c-test-not-loaded (package)
  (when (and (featurep package)
	     (not (get package 'load-warning)))
    (put package 'load-warning t)
    (cc-test-log "Warning: %s is loaded" package)))

(defun do-one-test (filename &optional no-error collect-tests)
  (interactive "fFile to test: ")

  (let ((default-directory cc-test-dir)
	(save-buf (current-buffer))
	(save-point (point))
	(font-lock-maximum-decoration t)
	(font-lock-global-modes nil)
	(enable-local-variables
	 (if (> emacs-major-version 21) ':all t))) ; disable Emacs 22's "safety" features.

    (if (and collect-tests
	     (member filename cc-test-finished-tests))
	(progn
	  (cc-test-message "Skipping %s - already tested" filename)
	  t)

      (let* ((buflist (make-test-buffers filename))
	     (testbuf (car buflist))
	     (res-syntax-buf (nth 1 buflist))
	     (exp-syntax-buf (nth 2 buflist))
	     (res-faces-buf (nth 3 buflist))
	     (exp-faces-buf (nth 4 buflist))
	     (check-syntax exp-syntax-buf)
	     (check-faces exp-faces-buf)
	     (pop-up-windows t)
	     (linenum 1)
	     ignore-errs
	     test-error-found
	     error-found-p
	     expectedindent
	     c-echo-syntactic-information-p
	     font-lock-verbose
	     last-result
	     (test-msg (if cc-test-benchmark-mode "Benching" "Testing")))

	;; These shouldn't be loaded if CC Mode is byte compiled properly.
	(c-test-not-loaded 'cc-langs)
	(c-test-not-loaded 'cc-bytecomp)
	(c-test-not-loaded 'cc-lobotomy)

	(switch-to-buffer testbuf)
	(setq ignore-errs (intersection cc-test-emacs-features
					cc-test-skip))

	(if (and (not check-syntax) (not check-faces))
	    (progn
	      (cc-test-log "Skipping %s - no .res or .face file" filename)
	      t)

	  (when check-syntax
	    (cc-test-tmp-message "%s %s (syntax)" test-msg filename)

	    ;; Collect the syntactic analysis of all lines.
	    (unless cc-test-benchmark-mode
	      (cc-test-record-syntax testbuf res-syntax-buf no-error
				     (lambda (err)
				       (unless error-found-p
					 (setq error-found-p t)
					 (cc-test-log
					  "%s:%d: c-guess-basic-syntax error: %s"
					  filename
					  (1+ (count-lines (point-min) (point)))
					  (error-message-string err))
					 (when cc-test-last-backtrace
					   (cc-test-log "%s" cc-test-last-backtrace)
					   (setq cc-test-last-backtrace nil))))))

	    ;; Record the expected indentation and reindent.  This is done
	    ;; in backward direction to avoid cascading errors.
	    (while (= (forward-line -1) 0)
	      (back-to-indentation)
	      (setq expectedindent (cons (current-column) expectedindent))
	      (unless (eolp)
		;; Do not reindent empty lines; the test cases might have
		;; whitespace at eol trimmed away, so that could produce
		;; false alarms.
		(let ((buffer-read-only nil))
		  (if no-error
		    (condition-case err
			(c-indent-line)
		      (error
		       (unless error-found-p
			 (setq error-found-p t)
			 (cc-test-log
			  "%s:%d: c-indent-line error: %s"
			  filename (1+ (count-lines (point-min)
						    (c-point 'bol)))
			  (error-message-string err))
			 (when cc-test-last-backtrace
			   (cc-test-log "%s" cc-test-last-backtrace)
			   (setq cc-test-last-backtrace nil)))))
		    (c-indent-line))))))

	  ;; Collect the face changes.  Do this after the indentation
	  ;; test so that we check that it doesn't depend on the
	  ;; properties added by the font locking.
	  (when check-faces
	    (cc-test-tmp-message "%s %s (fonts)" test-msg filename)

	    (cc-test-force-font-lock-buffer)
	    (goto-char (point-min))
	    (unless cc-test-benchmark-mode
	      (cc-test-record-faces testbuf res-faces-buf nil)))

	  (cc-test-tmp-message "%s %s" test-msg filename)

	  (unless (or error-found-p cc-test-benchmark-mode)
	    ;; Compare and report.
	    (when check-syntax
	      (set-buffer res-syntax-buf)
	      (goto-char (point-min))
	      (set-buffer exp-syntax-buf)
	      (goto-char (point-min)))
	    (when check-faces
	      (set-buffer res-faces-buf)
	      (goto-char (point-min))
	      (set-buffer exp-faces-buf)
	      (goto-char (point-min)))
	    (set-buffer testbuf)
	    (goto-char (point-min))

	    (catch 'break-loop
	      (while (not (eobp))
		(let (result expected regression-comment indent-err)

		  (flet ((regression-msg (msg &rest args)
			  (unless ignore-errs
			    (setq msg (apply 'format msg args))
			    (cc-test-log "%s:%d: %s" filename linenum msg)
			    (set-buffer testbuf)
			    (let ((buffer-read-only nil))
			      (beginning-of-line)
			      (unless (re-search-forward
				       (concat "\\s *" c-comment-start-regexp)
				       (c-point 'eol) t)
				(indent-for-comment))
			      (when (re-search-forward
				     "\\*/" (c-point 'eol) 'move)
				(goto-char (match-beginning 0)))
			      (unless regression-comment
				(setq regression-comment t)
				(delete-horizontal-space)
				(insert " !!! "))
			      (insert msg ". "))
			    (setq error-found-p t))
			  (setq test-error-found t)))

		    (when check-syntax
		      (set-buffer exp-syntax-buf)
		      (if (eobp)
			  ;; Check for premature end of the .res file here
			  ;; to avoid noise from the errors below.
			  (progn
			    (cc-test-log
			     "%s:%d: Unexpected end of .res file"
			     filename linenum)
			    (setq error-found-p t
				  check-syntax nil))

			;; Compare the syntax analysis.
			(setq result (progn
				       (set-buffer res-syntax-buf)
				       (buffer-substring (c-point 'bol)
							 (c-point 'eol)))
			      expected (progn
					 (set-buffer exp-syntax-buf)
					 (buffer-substring (c-point 'bol)
							   (c-point 'eol))))
			(unless (string= result expected)
			  (regression-msg "Expected analysis %s, got %s"
					  expected result))

			;; Compare indentation.
			(set-buffer testbuf)
			(unless (= (car expectedindent)
				   (progn (back-to-indentation)
					  (current-column)))
			  (regression-msg "Expected indentation %d, got %d"
					  (car expectedindent)
					  (current-column))
			  (setq indent-err t))

			(set-buffer res-syntax-buf)
			(forward-line 1)
			(set-buffer exp-syntax-buf)
			(forward-line 1)))

		    ;; Compare faces.  Only report the first inconsistency on
		    ;; the line.
		    (when check-faces
		      (set-buffer exp-faces-buf)
		      (if (eobp)
			  ;; Check for premature end of the .face file here
			  ;; to avoid noise from the errors below.
			  (progn
			    (cc-test-log
			     "%s:%d: Unexpected end of .face file"
			     filename linenum)
			    (setq error-found-p t
				  check-faces nil))

			;; Don't report fontification errors if there already
			;; are indentation errors on this line.
			(unless indent-err
			  (while
			      (progn
				(set-buffer res-faces-buf)
				(skip-chars-forward " \t")
				(setq result (and (not (eolp))
						  (read res-faces-buf)))
				(set-buffer exp-faces-buf)
				;; If face aliasing is done we might have
				;; entries in the expected buffer where a face
				;; is changed to itself, so we got to check
				;; and ignore that.
				(while (progn
					 (skip-chars-forward " \t")
					 (setq expected
					       (and (not (eolp))
						    (read exp-faces-buf)))
					 (when expected
					   (setcdr
					    expected
					    (delete-duplicates
					     (mapcar
					      (lambda (face)
						(get face 'cc-test-face-alias))
					      (cdr expected)))))
					 (and last-result
					      expected
					      (equal (cdr last-result)
						     (cdr expected)))))
				(prog1 (and (or result expected)
					    (equal result expected))
				  (when result
				    (setq last-result result)))))

			  (cond
			   ((not (or result expected)))
			   ((not result)
			    (regression-msg
			     "Expected %s face at column %d"
			     (cdr expected) (car expected)))
			   ((not expected)
			    (regression-msg
			     "Got unexpected %s face at column %d"
			     (cdr result) (car result)))
			   ((eq (car result) (car expected))
			    (if (and (featurep 'xemacs)
				     (<= emacs-major-version 20)
				     (equal (cdr result) '(doc))
				     (equal (cdr expected) '(str)))
				;; `font-lock-fontify-syntactically-region'
				;; in XEmacs <= 20 contains Lisp
				;; specific crud that affects all modes:
				;; Any string at nesting level 1 is
				;; fontified with the doc face.  Argh!  Yuck!
				nil
			      (regression-msg
			       "Expected %s face at column %d, got %s face"
			       (cdr expected) (car expected)
			       (cdr result))))
			   ((equal (cdr result) (cdr expected))
			    (regression-msg
			     "Expected %s face at column %d, got it at %d"
			     (cdr expected) (car expected)
			     (car result)))
			   (t
			    (regression-msg
			     (concat "Expected %s face at column %d, "
				     "got %s face at %d")
			     (cdr expected) (car expected)
			     (cdr result) (car result)))))

			(set-buffer res-faces-buf)
			(while (progn
				 (skip-chars-forward " \t")
				 (not (eolp)))
			  (setq last-result (read res-faces-buf)))
			(forward-line 1)
			(set-buffer exp-faces-buf)
			(forward-line 1)))

		    (set-buffer testbuf)
		    (forward-line 1)
		    (setq expectedindent (cdr-safe expectedindent)
			  linenum (1+ linenum)))))

	      (when check-syntax
		(set-buffer exp-syntax-buf)
		(unless (eobp)
		  (setq error-found-p t)
		  (cc-test-log "%s:%d: Expected end of .res file"
			       filename linenum)))
	      (when check-faces
		(set-buffer exp-faces-buf)
		(unless (eobp)
		  (setq error-found-p t)
		  (cc-test-log "%s:%d: Expected end of .face file"
			       filename linenum)))

	      (when ignore-errs
		(setq test-error-found (not test-error-found))
		(when test-error-found
		  (cc-test-log
		   "%s:%d: Expected differences in syntax or fontification"
		   filename linenum)))
	      (if test-error-found (setq error-found-p t))))

	  (unless (or error-found-p (not collect-tests))
	    (setq cc-test-finished-tests
		  (cons filename cc-test-finished-tests)))

	  (when (and error-found-p (not no-error))
	    (set-buffer testbuf)
	    (buffer-enable-undo testbuf)
	    (set-buffer-modified-p nil)

	    (when exp-syntax-buf
	      (set-buffer res-syntax-buf)
	      (buffer-enable-undo res-syntax-buf)
	      (set-buffer-modified-p nil)
	      (set-buffer exp-syntax-buf)
	      (buffer-enable-undo exp-syntax-buf)
	      (set-buffer-modified-p nil))

	    (when exp-faces-buf
	      (set-buffer exp-faces-buf)
	      (buffer-enable-undo exp-faces-buf)
	      (set-buffer-modified-p nil))

	    (error "Regression found in file %s" filename))

	  (unless noninteractive
	    (message nil))

	  (set-buffer save-buf)
	  (goto-char save-point)
	  (when (and (not error-found-p) (interactive-p))
	    (kill-test-buffers))
	  (not error-found-p))))))

(defvar signal-hook-function)		; Doesn't exist in XEmacs.

(defun cc-test-signal (err data)
  (let ((standard-output (generate-new-buffer " *backtrace*"))
	(signal-hook-function nil))
    (backtrace)
    (save-current-buffer
      (set-buffer standard-output)
      (setq cc-test-last-backtrace (buffer-string)))
    (kill-buffer standard-output)
    (signal err data)))

(defun do-all-tests (&optional resetp)
  (interactive "P")
  (let ((old-c-echo-parsing-error (symbol-function 'c-echo-parsing-error))
	broken-files cc-test-comp-buf cc-test-comp-win
	(signal-hook-function (if cc-test-dump-backtraces
				  'cc-test-signal
				(if (boundp 'signal-hook-function)
				    (symbol-value 'signal-hook-function))))
	(enable-local-variables ':all))	; for Emacs 22's safety features.
    (unwind-protect
	(progn
	  (unless noninteractive
	    ;; Log to a buffer like M-x compile does.
	    (save-some-buffers)
	    (setq cc-test-comp-buf (get-buffer-create "*cc-test-log*"))
	    (save-excursion
	      (set-buffer cc-test-comp-buf)
	      (buffer-disable-undo cc-test-comp-buf)
	      (erase-buffer)
	      (buffer-enable-undo cc-test-comp-buf)
	      (set-buffer-modified-p nil)
	      (compilation-mode)
	      (toggle-read-only 0)	; `compilation-mode' has made it RO.
	      (setq cc-test-comp-win (display-buffer cc-test-comp-buf))
	      (set-window-start cc-test-comp-win (point-min))
	      (compilation-set-window-height cc-test-comp-win)))
	  (when (consp resetp)
	    (setq cc-test-finished-tests nil))
	  (cc-test-log "Testing CC Mode %s in %s" c-version (emacs-version))
	  (fset 'c-echo-parsing-error (lambda (&optional quiet)))
	  (mapcar (lambda (test)
		    (condition-case err
			(unless (do-one-test test t t)
			  (setq broken-files (cons test broken-files)))
		      (error
		       (cc-test-log "%s: Eval error: %s"
				    test (error-message-string err))
		       (when cc-test-last-backtrace
			 (cc-test-log "%s" cc-test-last-backtrace)
			 (setq cc-test-last-backtrace nil))
		       (setq broken-files (cons test broken-files)))
		      ))
		  (directory-files
		   cc-test-dir nil
		   "\\.\\(c\\|cc\\|java\\|pike\\|idl\\|m\\|awk\\)\\'")))
      (fset 'c-echo-parsing-error old-c-echo-parsing-error))
    (if noninteractive
	(send-string-to-terminal cc-test-clear-line-string))
    (kill-test-buffers)
    (when broken-files
      (cc-test-message "Broken file(s): %s"
		       (mapconcat 'identity (reverse broken-files) ", ")))
    (unless noninteractive
      (if broken-files
	  (first-error)
	(message "All tests successful")
	(delete-window cc-test-comp-win)
	(kill-buffer cc-test-comp-buf)
	(setq cc-test-finished-tests nil)))))

(defun facefile ()
  "Creates a .face file from the test file in the current buffer.
It records the faces put into the buffer by font-lock in the test file."

  ;; Note: fast-lock cache files can't be used for this since we need
  ;; to take special precautions to make the results comparable
  ;; between different (X)Emacsen.  This format is also more edit and
  ;; cvs friendly.

  (interactive)

  (when (and (featurep 'xemacs)
	     (<= emacs-major-version 20))
    ;; See special case in `do-one-test'.
    (error "Won't make .face files in XEmacs <= 20 since those versions "
	   "have broken syntactic fontification"))

  (let* ((testbuf (current-buffer))
	 (facefile (concat (file-name-sans-extension buffer-file-name)
			   ".face"))
	 (facebuf (find-file-noselect facefile))
	 error errpos)

    (save-excursion
      (save-selected-window
	(condition-case err
	    (progn
	      (switch-to-buffer-other-window facebuf)
	      (set-buffer facebuf)
	      (erase-buffer)
	      (set-buffer testbuf)

	      (cc-test-force-font-lock-buffer)
	      (goto-char (point-min))
	      (cc-test-record-faces testbuf facebuf t)

	      ;; Beautify the face file a little.
	      (set-buffer facebuf)
	      (goto-char (point-min))
	      (while (search-forward ")(" nil 'move)
		(replace-match ") (" t t))
	      (unless (bolp) (insert "\n")))

	  (error
	   (setq error err)
	   (set-buffer testbuf)
	   (setq errpos (point))))))
    (when error
      (when errpos (goto-char errpos))
      (signal (car error) (cdr error)))))

(defun resfile ()
  "Creates a .res file from the test file in the current buffer.
It records the syntactic analysis of each line in the test file."

  (interactive)
  (save-excursion
    (save-selected-window
      (let* ((testbuf (current-buffer))
	     (resfile (concat (file-name-sans-extension buffer-file-name)
			      ".res"))
	     (resbuf (find-file-noselect resfile)))
	(switch-to-buffer-other-window resbuf)
	(set-buffer resbuf)
	(erase-buffer)
	(cc-test-record-syntax testbuf resbuf nil nil)
	(cc-test-convert-to-rel-offsets resbuf testbuf)
	(set-buffer resbuf)
	(goto-char (point-min))))))

(defun shift-res-offsets (offset)
  ;; Shifts the offsets in the corresponding .res files by the
  ;; specified amount from the current point forward.
  (interactive "nShift offsets at or after point with: ")
  (let ((save-point (point))
	(resfile (concat (file-name-sans-extension buffer-file-name) ".res"))
	(count 0))
    (unless (file-exists-p resfile)
      (error "Cannot open result file %s" resfile))
    (find-file resfile)
    (goto-char (point-min))
    (while (re-search-forward "\\<[0-9]+\\>" nil t)
      (let ((pos (string-to-number (buffer-substring-no-properties
				    (match-beginning 0) (match-end 0)))))
	(when (>= pos save-point)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert-and-inherit (format "%d" (+ pos offset)))
	  (setq count (1+ count)))))
    (message "Shifted %d offsets" count)))
