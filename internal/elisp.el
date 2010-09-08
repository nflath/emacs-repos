;;Turn on eldoc in emacs-lisp modes and the *scratch* buffer.
(add-hook-to-all elisp-modes 'turn-on-eldoc-mode)
(switch-to-buffer "*scratch*")
(turn-on-eldoc-mode)

;;If we edit a .el file that has a corresponding .elc file, we don't want to keep the outdated .elc file.
(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

;;Add tab-completion to M-:.
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;;Make the scratch buffer unkillable and persistent
(defvar scratch-file (concat emacs-repos-dir "scratch.el")
  "Location the *scratch* buffer is saved to.")

(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions #'(lambda ()
                                             (if (eq (current-buffer) (get-buffer-create "*scratch*"))
                                                 (bury-buffer)
                                               (bury-buffer (get-buffer-create "*scratch*")))
                                             nil))
  (erase-buffer)
  (insert-file scratch-file)
  (setq buffer-file-name scratch-file)
  (setq default-directory emacs-repos-dir))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

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