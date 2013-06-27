(require 'flymake)

;; If there is a tags file in a directory above this one, use it; otherwise
;; create one in this directory.  I don't want to be prompted for a TAGS file.
(defadvice find-tag (before create-tags activate)
  (make-variable-buffer-local 'tags-file-name)
  (let ((tf (locate-dominating-file default-directory "TAGS")))
    (unless tf
      (shell-command "etags *")
      (setq tf "TAGS"))
    (setq tags-file-name tf)))

;;Compiling always asks to save my buffers; I want it to instead do nothing, so I have save-some-buffers only do stuff if passed a filter.
(defadvice save-some-buffers (around save-buffers-ignore activate)
  (if (ad-get-arg 1)
      ad-do-it))

;I want compilation buffers to scroll to the bottom, since this is in general where errors are.
(setq compilation-scroll-output t)

(setq prog-mode nil)
(make-variable-buffer-local 'prog-mode)
(add-hook 'prog-mode-hook (lambda () (setq prog-mode t)))

;;These advices cause copy-pasted code to be properly indented.
(defadvice yank (after indent-region activate)
  (when prog-mode
    (let ((mark-even-if-inactive t))
      (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (when prog-mode
    (let ((mark-even-if-inactive transient-mark-mode))
      (indent-region (region-beginning) (region-end) nil))))

(defadvice kill-line (after fixup-whitespace activate)
  "Call fixup whitespace after killing line."
  (when (not (eq major-mode 'python-mode))
    (if (not (looking-at "$"))
        (fixup-whitespace))
    (if (not (looking-at "$"))
        (fixup-whitespace))
    (if (and (not (eq major-mode 'shell-mode))
             (not (looking-at "^$"))
             (not (eq indent-line-function 'indent-relative)))
        (funcall indent-line-function))))

(defun string-trim (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defadvice kill-line (after fixup-comments activate)
  "Don't leave comment characters after killing a line."
  (let* ((pt (point))
         (comment-at-start (progn (back-to-indentation) (looking-at (concat "[ \t]*" (regexp-opt (list (string-trim comment-start)))))))
         (only (eq (point) pt)))
    (goto-char pt)
    (let ((start (point)))
      (print comment-at-start)
      (when (and comment-start
                 only
                 comment-at-start
                 (looking-at (concat "[ \t]*" (regexp-opt (list (string-trim comment-start))))))
        (let ((len (- (match-end 0) (match-beginning 0))))
          (back-to-indentation)
          (when (and (looking-at (regexp-opt (list (string-trim comment-start))))
                     (not (= (point) start)))
            (goto-char start)
            (delete-char len)))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\|FixMe\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-add-watchwords)
