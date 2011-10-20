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

;;I want compilation buffers to scroll to the bottom, since this is in general where errors are.
(setq compilation-scroll-output t)

;;These advices cause copy-pasted code to be properly indented.
(defadvice yank (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode programming-major-modes)
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

(defadvice kill-line (after fixup-comments activate)
  "Don't leave comment characters after killing a line."
  (let* ((pt (point))
         (only (progn (back-to-indentation) (eq (point) pt))))
    (goto-char pt)
    (let ((start (point)))
      (when (and comment-start
                 only
                 (looking-at (concat "[ \t]*" (regexp-opt (list (string-trim comment-start))))))
        (let ((len (- (match-end 0) (match-beginning 0))))
          (back-to-indentation)
          (when (and (looking-at (regexp-opt (list (string-trim comment-start))))
                     (not (= (point) start)))
            (goto-char start)
            (delete-char len)))))))

(setq comment-auto-fill-only-comments t)

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
