;;; init.el --- Entry point of my emacs initialization file

;;; Commentary:

;; FixMe: Eldoc running in python - [2014-11-01 Sat] Posted on stackexchange
;; FixMe: Have flycheck messages override eldoc messages - [2014-10-22 Wed] Asked on emacs stack exchange
;; FixMe: Why are colors not working in ansi-term?
;; FixMe: Automatically Checkdoc on save?
;; FixMe: Skeleton files
;; FixMe: Investigate erefactor
;; FixMe: Investigate redshank
;; FixMe: Investigate snippets
;; FixMe: Add filtering to ido (or try out helm-mode)
;; FixMe: Make ERC readable
;; FixMe: dirtrack-buffer-name-track-mode doesn't seem to be working
;; FixMe: CEDET
;; FixMe: have 't' in org-mode go straight from TODO->DONE, 'c' go from TODO->CANCELED (is this org-speed-commands?)
;; FixMe: Get better fontification for org-mode
;; FixMe: Allow @-1 in org-mode
;; FixMe: why does whitespace-mode not seem to work?
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

(maximize-frame)

(provide 'init)
;;; init.el ends here
