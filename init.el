;; FixMe: Eldoc running in python
;; FixMe: highlight-symbol-at-point customization
;; FixMe: Automatically Checkdoc
;; FixMe: Skeleton files
;; FixMe: snippets
;; FixMe: Jump-back
;; FixMe: Add filtering to ido
;; FixMe: Get better fontification for org-mode
;; FixMe: ( can cause org fontification to break?
;; FixMe: Python auto-indentation is broken - maybe smart-whitespace-comment-fixup.el?
;; FixMe: Make ERC readable
;; FixMe: Allow @-1 in org-tables
;; FixMe: CEDET
;; FixMe: Why is company-mode not popping up autocompletes in elisp mode?
;; FixMe: Have flycheck messages override eldoc messages
;; FixMe: Submit changes to jump-dls.el

;; FixMe: Upgrade to emacs 24
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


(defun custom-theme-load-confirm (hash) t)

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

(add-to-list 'auto-mode-alist '("\\.tin$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . c++-mode))
;; FixMe: Get tac mode here
(exec-path-from-shell-initialize)
