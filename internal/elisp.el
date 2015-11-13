;; Turn on eldoc in emacs-lisp modes and the *scratch* buffer.
;; Turn eldoc on
(setq eldoc-idle-delay 0)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(switch-to-buffer "*scratch*")
(turn-on-eldoc-mode)

;; If we edit a .el file that has a corresponding .elc file, we don't want to
;; keep the outdated .elc file.
(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (if (not (or (f-ancestor-of? emacs-repos-dir default-directory)
                         (file-equal-p emacs-repos-dir default-directory)))
                (flycheck-mode))))
