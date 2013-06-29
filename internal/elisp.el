;; Turn on eldoc in emacs-lisp modes and the *scratch* buffer.
;; Turn eldoc on
(setq eldoc-idle-delay 0)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook-to-all elisp-modes 'turn-on-eldoc-mode)
(switch-to-buffer "*scratch*")
(turn-on-eldoc-mode)

(defvar elisp-modes '(emacs-lisp-mode-hook
                      lisp-interaction-mode-hook
                      ielm-mode-hook
                      inferior-emacs-lisp-mode-hook)
  "List of modes that are used for programming in emacs-lisp.")

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

;; Add tab-completion to M-:
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; Set correct formatting for GNU Emacs elisp files
;; FixMe: Factor out and make it better
(defun gnu-emacs-elisp-formating ()
  "Sets up the correct style for officcial GNU Emacs elisp files."
  (setq indent-tabs-mode t)
  (setq tab-width 8))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (save-excursion
              (goto-char (point-min))
              (when (search-forward "This file is part of GNU Emacs." (point-max) t)
                (gnu-emacs-elisp-formating)))))
