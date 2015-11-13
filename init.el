;;; init.el --- Entry point of my emacs initialization file

(setq debug-on-error t) ;; We want to debug errors.

(defvar emacs-repos-dir "~/Dropbox/emacs-repos/")
(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 3000)
(setq mac-command-modifier `meta)
(setq enable-local-eval t)
(setq enable-local-variables :all)

;;; Set the load path
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Load private data (that I don't want in Github)
(load-file "~/Dropbox/private.el")

(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))

;; Only reset keybindings after downloading everything
(load-file (concat emacs-repos-dir "keybindings.el"))

(setq read-quoted-char-radix 16)
(server-start)
(setq case-fold-search t)

(auto-insert-mode 1)
