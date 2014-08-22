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
