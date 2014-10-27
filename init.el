;;; init.el --- Entry point of my emacs initialization file

;;; Commentary:

;; FixMe: Eldoc running in python
;; FixMe: Why are colors not working in ansi-term?
;; FixMe: Automatically Checkdoc on save?
;; FixMe: Investigate aggresive-indent-mode
;; FixMe: Skeleton files
;; FixMe: Investigate snippets
;; FixMe: Add filtering to ido (or try out helm-mode)
;; FixMe: Python auto-indentation is broken - maybe smart-whitespace-comment-fixup.el?
;; FixMe: Make ERC readable
;; FixMe: CEDET
;; FixMe: Fix everything checkdoc/flycheck occurs
;; FixMe: have 't' in org-mode go straight from TODO->DONE, 'c' go from TODO->CANCELED (is this org-speed-commands?)
;; FixMe: Get better fontification for org-mode
;; FixMe: Allow @-1 in org-tables
;; FixMe: why does whitespace-mode not seem to work?
;; FixMe: Have flycheck messages override eldoc messages - [2014-10-22 Wed] Asked on emacs stack exchange
;; FixMe: Sometimes the agenda disappears after marking an item completed - [2014-10-24 Fri] Asked on emacs SE
;; FixMe: Have going to 'waiting' state take off the 'scheduled' timestamp - [2014-10-27 Mon] Asked on emacs SE
;; FixMe: flush-lines default to last entry

;; FixMe: just-one-space (currently bound to spotlight key)
;; FixMe: rectangle-mark-mode instead of CUA?
;; FixMe: prettify-symbols-mode
;; FixMe: Use advice-edd and advice-rermove instead of def advice

;; FixMe: File mode specification error: (error "Lisp nesting exceeds `max-lisp-eval-depth'")
;; FixMe: Loading /Users/nflath/Dropbox/emacs-repos/init.el (source)...done


;;; Code:
(defun custom-theme-load-confirm (&rest args)
  "Prevent theme from promting during load.  ARGS is ignored."
  t)

(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 3000)
(setq mac-command-modifier `meta)
(setq enable-local-eval t)
(setq enable-local-variables :all)

;; FixMe: This should not be required
(require 'cc-mode)
(setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))

;;; Set the load path
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; Set a few paths in order to determine where everything is
(defvar emacs-repos-dir "~/Dropbox/emacs-repos/")
(defvar org-directory "~/Dropbox/org/" "Location of org files.")

;; Load private data (that I don't want in Github)
(load-file "~/Dropbox/private.el")

(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))

;;; Load other customizations
(setq load-dirs (concat emacs-repos-dir "internal"))

;; Only reset keybindings after downloading everything
(load-file (concat emacs-repos-dir "keybindings.el"))

(setq wg-session-load-on-start t)
(workgroups-mode 1)
(save-visited-files-mode t)
(server-start)

(add-to-list 'auto-mode-alist '("\\.tin$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . c++-mode))
;; FixMe: Get tac mode here
(exec-path-from-shell-initialize)
(add-to-list 'exec-path "~/bin")

(provide 'init)
(maximize-frame)
;;; init.el ends here
