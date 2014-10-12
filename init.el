;; FixMe: Eldoc running in python
;; FixMe: highlight-symbol-at-point customization
;; FixMe: Automatically Checkdoc
;; FixMe: Skeleton files
;; FixMe: snippets
;; FixMe: Jump-dls auto-genrates tags and imenu
;; FixMe: Add filtering to ido
;; FixMe: Error message - Can't guess python-indent-offset, using defaults: 4
;; FixMe: Get better fontification for org-mode
;; FixMe: ( can cause org fontification to break?
;; FixMe: Python auto-indentation is broken - maybe smart-whitespace-comment-fixup.el?
;; FixMe: Make ERC readable
;; FixMe: Allow @-1 in org-tables
;; FixMe: CEDET
;; FixMe: Get jump-dls to prompt if no matches found
;; FixMe: Why is company-mode not popping up autocompletes in elisp mode?
;; FixMe: Have flycheck messages override eldoc messages
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
;; FixMe: Get tac-mode here

(add-to-list 'auto-mode-alist '("\\.tac$" . c++-mode))
(exec-path-from-shell-initialize)
