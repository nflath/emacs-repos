;;Use the common-lisp library.
(require 'cl)

(defun add-hook-to-all (hooks fn)
  "Add a function to a list of hooks."
  (mapcar (lambda (hook) (add-hook hook fn)) hooks))

(defun remove-hook-from-all (hooks fn)
  "Removes a function from a list of hooks."
  (mapcar (lambda (hook) (remove-hook hook fn)) hooks))

(defvar elisp-modes '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    inferior-emacs-lisp-mode-hook)
  "List of modes that are used for programming in emacs-lisp.")

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
    clojure-mode
    )
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

(defun directory-files-recursive (dir )
  "Returns a list of files in the directory specified and all subdirectories."
  (apply #'append
   (mapcar
          (lambda (file)
            (when (not (string-match ".*\\.$" file))
              (if (file-directory-p file)
                  (directory-files-recursive file)
                (list file))))
          (directory-files dir t))))

(defun before-first (regexp string)
  "Returns the prefix of string that occurs directly before the start of the first match of 'regexp'."
  (let ((index (string-match regexp string)))
    (if index
        (substring string 0 (match-beginning 0))
      string)))

(defun surround-area (start end text)
  "Surrounds the given region with the given text."
  (goto-char end)
  (insert text)
  (goto-char start)
  (insert text))

(defun time-load-file (file)
  "Loads a file and returns a pair consisting of the filename and
the time in seconds it took to load."
  (let ((prev-time (float-time)))
    (load-file file)
    (cons file (- (float-time) prev-time))))

(defun load-directory (dir)
  "Loads every .el file in a directory."
  (mapcar 'time-load-file (directory-files dir t "\\.elc?$")))

(defun string-trim (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun exec-cmds (cmd-list error)
  "Execute all commands in the list until one fails, then print
an error with the command that failed."
  (if (not cmd-list) 0
    (if (= 0 (shell-command (car cmd-list)))
        (exec-cmds (cdr cmd-list) error)
      (message (concat error ": The command '%s' failed.") (car cmd-list)))))

