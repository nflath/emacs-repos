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

;; FixMe: Make own package?
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\|FixMe\\)"
        1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'esk-add-watchwords)
