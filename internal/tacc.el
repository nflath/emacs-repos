(defcustom tacc-basic-offset 3 "Amount to indent by")

(setq tacc-mode-syntax-table c++-mode-syntax-table)
(setq tacc-mode-font-lock-keywords
  (let ((keywords '("foreach" "or" "wait" "timeout" "default" "case" "switch" "for" "do" "while"
                    "else" "if" "continue" "break" "embedded" "coroutine" "frined" "sparse" "array" "stack"
                    "extensible" "queue" "using" "new" "static" "inout" "in" "out" "invasive" "extern" "inline"
                    "return" "overloading" "overriding" "overridable" "overlay" "ordered" "prealloc" "dense" "interface"
                    "CppInlineBlock" "CInlineBlock" "CppBlock" "CBlock" "void"
                    "type" "enum" "enumId" "CppInlineInclude" "CppInclude" "CInclude" "CInlineInclude"
                    "final" "immutable" "initially" "const" "mutable" "mutableThruConst" "local" "import"))
        (properties '("`[a-ZA-Z]*"))
        (types '("bool" "char" "float" "long" "int" "short"))
        (constants '("true" "false")))
    `((,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt types 'words) . font-lock-type-face)
      (,(regexp-opt constants 'words) . font-lock-constant-face)
      (,"`[a-zA-Z]*" . font-lock-builtin-face)
      (,"\\<[A-Z][a-zA-Z0-9:]*\\>" . font-lock-type-face)
      (,"\\<[a-z][a-zA-Z0-9:]*\\>[^(]" . font-lock-variable-name-face))))

(defun num-open (open close)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((end (point))
          (indent 0))
      (goto-char (point-min))
      (while (re-search-forward (concat "[" open close "]") end t)
        (backward-char)
        (let ((face (face-at-point)))
          (forward-char)
          (unless (or (eq face 'font-lock-comment-face)
                      (eq face 'font-lock-string-face))
            (if (= (char-before) (string-to-char open))
              (setq indent (1+ indent))
            (setq indent (1- indent))))))
      indent)))

(defun tacc-mode-indentation ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((end (point))
          (indent (num-open "{" "}")))
      (goto-char (point-min))
      (while (and (< (point) end)
                  (search-forward "namespace" end t))
        (search-forward "{")
        (condition-case nil 
            (progn
              (backward-char)
              (forward-sexp)
              (if (> point end)
                  (setq indent 1- indent)))
          (error (setq indent (1- indent)))))
      (goto-char end)
      (end-of-line)
      (if (= 0 (num-open "(" ")"))
          (let ((lineend (point)))
            (beginning-of-line)
            (if (re-search-forward "}" lineend t)
                (* tacc-basic-offset (1- indent))
              (* tacc-basic-offset indent)))
        (re-search-backward "(" (point-min) t)
        (+ 1 (current-column))))))

(defun delete-horizontal-space-forward ()
  (interactive)
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun tacc-indent-line (&rest args)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space-forward)
    (let ((indent (tacc-mode-indentation)))
      (if (> indent 0)
          (dotimes (x indent)
            (insert " "))))
    (beginning-of-line))
  (let ((saf (point)))
    (back-to-indentation)
    (if (> saf (point))
        (goto-char saf))))

(define-derived-mode tacc-mode nil "Tacc"
  "tacc"
  :syntax-table tacc-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(tacc-mode-font-lock-keywords nil nil nil nil))
  (set (make-local-variable 'indent-line-function) 'tacc-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "//"))
  

(define-key tacc-mode-map (kbd "TAB") 'tacc-indent-line)
(define-key tacc-mode-map (kbd "C-c o") 'ff-find-other-file)
(require 'find-file)
(add-to-list 'cc-other-file-alist '("\\.tac\\'" (".tin")))
(add-to-list 'cc-other-file-alist '("\\.tin\\'" (".tac")))

(defun tacc-electric (str)
  (insert str)
  (tacc-indent-line))

;(define-key tacc-mode-map (kbd "{") (lambda () (interactive) (tacc-electric "{")))

(provide 'tacc)