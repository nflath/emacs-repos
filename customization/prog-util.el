;;Use the common-lisp library.
(require 'cl)

;;; FixMe: Factor these two functions into a package: hook-utils?
(defun add-hook-to-all (hooks fn)
  "Add a function to a list of hooks."
  (mapcar (lambda (hook) (add-hook hook fn)) hooks))

(defun remove-hook-from-all (hooks fn)
  "Removes a function from a list of hooks."
  (mapcar (lambda (hook) (remove-hook hook fn)) hooks))

(defvar elisp-modes '(emacs-lisp-mode-hook
                      lisp-interaction-mode-hook
                      ielm-mode-hook
                      inferior-emacs-lisp-mode-hook)
  "List of modes that are used for programming in emacs-lisp.")

;; FixMe: Try to use prog-mode-hook here
(defvar programming-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    inferior-emacs-lisp-mode
    scheme-mode
    sql-mode
    lisp-mode
    c-mode
    python-mode
    c++-mode
    java-mode
    objc-mode
    haskell-mode
    clojure-mode)
    "List of programming modes.")

(defun line-matches (regexp)
    "Returns non-nil if the current line matches the given regexp, nil otherwise."
    (interactive "sRegex: ")
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (re-search-forward regexp end t))))

(defun after-last (regexp string)
  "Returns the part of the string after the last occurrence of regexp."
  (let ((index (string-match regexp string)))
    (if index
        (after-last regexp (substring string (match-end 0) (length string)))
      string)))

(defun string-match-any (regexp-list string &optional start)
  "Returns whether the given string, starting at position start,
matches any regexp in the list."
  (if regexp-list
      (let ((result (string-match (car regexp-list) string start)))
        (if result
            result
          (string-match-any (cdr regexp-list) string start)))))

(defun string-trim (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))
