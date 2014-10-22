;;; init.el --- Entry point of my emacs initialization file

;;; Commentary:

;; FixMe: Eldoc running in python
;; FixMe: Why are colors not working in ansi-term?
;; FixMe: highlight-symbol-at-point customization
;; FixMe: Automatically Checkdoc on save?
;; FixMe: Skeleton files
;; FixMe: Investigate snippets
;; FixMe: Add elisp-slime-nav to jump-jumpers
;; FixMe: Add filtering to ido (or try out helm-mode)
;; FixMe: Python auto-indentation is broken - maybe smart-whitespace-comment-fixup.el?
;; FixMe: Make ERC readable
;; FixMe: CEDET
;; FixMe: Have flycheck messages override eldoc messages - [2014-10-22 Wed] Asked on emacs stack exchange
;; FixMe: Fix everything checkdoc/flycheck occurs
;; FixMe: have 't' in org-mode go straight from TODO->DONE, 'c' go from TODO->CANCELED (is this org-speed-commands?)
;; FixMe: Get better fontification for org-mode
;; FixMe: Have ~/ in ido still go to home dir
;; FixMe: Allow @-1 in org-tables
;; FixMe: Set up default browser to be firefox instead of chrome
;; FixMe: why does whitespace-mode not seem to work?

;; FixMe: Uniquify is enabled by default - can I remove this customization?
;; FixMe: investigate dired-hide-details-mode
;; FixMe: electric-indent-mode defaults to true
;; FixMe: toggle-frame-fullscreen and toggle-frame-maximized are now here
;; FixMe: just-one-space (currently bound to spotlight key)
;; FixMe: rectangle-mark-mode instead of CUA?
;; FixMe: prettify-symbols-mode
;; FixMe: Use advice-edd and advice-rermove instead of def advice

;; FixMe: File mode specification error: (error "Lisp nesting exceeds `max-lisp-eval-depth'")
;; FixMe: Loading /Users/nflath/Dropbox/emacs-repos/init.el (source)...done
;; FixMe: Loading Emacs Lisp code from ~/Dropbox/emacs-repos/internal
;; FixMe: Skipping /Users/nflath/Dropbox/emacs-repos/internal/c, it's already loaded.


;;; Code:
(defun custom-theme-load-confirm (&rest args)
  "Prevent theme from promting during load.  ARGS is ignored."
  t)


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
(load-dirs)

;; Only reset keybindings after downloading everything
(load-file (concat emacs-repos-dir "keybindings.el"))

;;We're finished loading everything now
(maximize-frame)
(setq wg-session-load-on-start t)
(workgroups-mode 1)
(save-visited-files-mode t)
(server-start)

(add-to-list 'auto-mode-alist '("\\.tin$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . c++-mode))
;; FixMe: Get tac mode here
(exec-path-from-shell-initialize)

(provide 'init)
;;; init.el ends here
