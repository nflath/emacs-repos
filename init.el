;; FixMe: recursive-narrow only really works if narrow-to-defun is evaluated???
;; FixMe: Error message - Can't guess python-indent-offset, using defaults: 4
;; FixMe: ( can cause org fontification to break?
;; FixMe: Better searchingaqw
;; FixMe: Python auto-indentation is broken
;; FixMe: Make ERC readable
;; FixMe: which-function in C++
;; FixMe: investigate breadcrumb
;; FixMe: keybindings should all be (eval-after-load)ed
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

;;; Add git to our exec-path
;; FixMe: This should be unnecessary and already in our path
(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "~/bin")

;;; Set a few paths in order to determine where everything is
(setq emacs-repos-dir "~/Dropbox/emacs-repos/")
(defvar org-directory "~/Dropbox/org/" "Location of org files")

;;; Set the load path
;;; FixMe: Is this necessary?  Can we just move everything into one directory now
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Create various required directories
;; FixMe: Is this actually required?
(mapcar (lambda (dir) (mkdir dir t))
        (list (concat user-emacs-directory "elpa")
              (concat user-emacs-directory "log")))

;;; Load packages using package
;; FixMe: Is this required or can we just do the load-dirs
(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))

;;; Load other customizations
(setq load-dirs (concat emacs-repos-dir "/internal"))
(load-dirs)

;; FixMe: Rename this file
(load-file "~/Dropbox/logins.el")

;;We're finished loading everything now
(maximize-frame)
(setq wg-session-load-on-start t)
(workgroups-mode 1)
(save-visited-files-mode t)
(server-start)
