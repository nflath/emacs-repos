;;; init.el --- Entry point of my emacs initialization file
;;; Commentary:

;; FixMe: Eldoc running in python
;; FixMe: highlight-symbol-at-point customization
;; FixMe: Automatically Checkdoc on save?
;; FixMe: Skeleton files
;; FixMe: Investigate snippets
;; FixMe: Add elisp-slime-nav to jump-jumpers
;; FixMe: Add filtering to ido (or try out helm-mode)
;; FixMe: Python auto-indentation is broken - maybe smart-whitespace-comment-fixup.el?
;; FixMe: Make ERC readable
;; FixMe: CEDET
;; FixMe: Have flycheck messages override eldoc messages
;; FixMe: Fix everything checkdoc/flycheck occurs

;; FixMe: Get org-mode to hook into google calendar for events
;; FixMe: have 't' in org-mode go straight from TODO->DONE, 'c' go from TODO->CANCELED
;; FixMe: Get better fontification for org-mode
;; FixMe: ( can cause org fontification to break?
;; FixMe: Allow @-1 in org-tables

;; FixMe: Upgrade to Emacs 24
;; FixMe: Uniquify is enabled by default - can I remove this customization?
;; FixMe: investigate dired-hide-details-mod
;; FixMe: electric-indent-mode defaults to true
;; FixMe: eldoc in messages-buffer-mode
;; FixMe: (setq load-prefer-newer t)
;; FixMe: toggle-frame-fullscreen and toggle-frame-maximized are now here
;; FixMe: just-one-space (currently bound to spotlight key)
;; FixMe: rectangle-mark-mode instead of CUA?
;; FixMe: prettify-symbols-mode
;; FixMe: Use advice-aedd and advice-rermove instead of defadvice
;; FixMe: org-mode filter by location


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
(defvar load-dirs (concat emacs-repos-dir "/internal"))
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
