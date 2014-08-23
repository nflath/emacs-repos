;; FixMe: Add filtering to ido
;; FixMe: Error message - Can't guess python-indent-offset, using defaults: 4
;; FixMe: Get better fontification for org-mode
;; FixMe: ( can cause org fontification to break?
;; FixMe: Python auto-indentation is broken
;; FixMe: Make ERC readable
;; FixMe: which-function in C++
;; FixMe: Allow @-1 in org-tables

(setq mac-command-modifier `meta)

;; FixMe: This should not be required
(require 'cc-mode)
(setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))

;; FixMe: This should not be required
(setq calc-multiplication-has-precedence t)

;; FixMe: Are these required?
(setq-default case-fold-search t)
(setq-default debug-on-error t)

;; FixMe: Should be somewhere else.
(when (file-exists-p "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

;;; Set the load path
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; Set a few paths in order to determine where everything is
(setq emacs-repos-dir "~/Dropbox/emacs-repos/")
(defvar org-directory "~/Dropbox/org/" "Location of org files")

;; Load private data (that I don't want in Github)
(load-file "~/Dropbox/private.el")

(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))

;;; Load other customizations
(setq load-dirs (concat emacs-repos-dir "/internal"))
(load-dirs)

;; Only reset keybindings after downloading everything
(load-file (concat emacs-repos-dir "keybindings.el"))

;;We're finished loading everything now
(maximize-frame)
(setq wg-session-load-on-start t)
(workgroups-mode 1)
(save-visited-files-mode t)
(server-start)


;; FixMe: Move these to appropriate places
(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))


 (defun dired-back-to-top ()
   (interactive)
   (beginning-of-buffer)
   (dired-next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

 (defun dired-jump-to-bottom ()
   (interactive)
   (end-of-buffer)
   (dired-next-line -1))

 (define-key dired-mode-map
   (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(global-set-key (kbd "C-M-j")
                (lambda ()
                  (interactive) (join-line -1)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)
