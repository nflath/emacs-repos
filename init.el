(setq mac-command-modifier `meta)

(require 'cc-mode)
(setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))

(setq calc-multiplication-has-precedence t)
(setq-default case-fold-search t)
(setq-default debug-on-error t)

;; FixMe: investigate projectile
;; FixMe: Make ERC readable
;; FixMe: which-function in C++
;; FixMe: investigate breadcrumbs
;; FixMe: s should all be (eval-after-load)ed

(setq-default ispell-program-name "/usr/local/bin/aspell")


;;; Add git to our exec-path
(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "~/bin")

;;; Set a few paths in order to determine where everything is
(setq user-emacs-directory "~/.emacs.d/")
(setq emacs-repos-dir "~/Dropbox/emacs-repos/")
(defvar org-directory "~/Dropbox/org/" "Location of org files")

;;; Set the load path
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;Create various required directories
(mapcar (lambda (dir) (mkdir dir t))
        (list (concat user-emacs-directory "elpa")
              (concat user-emacs-directory "log")))

;;; Load packages using package
(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))


;;; Load other customizations
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                        '("internal" "external")))
(load-dirs)
(load-file "~/Dropbox/logins.el")

;;We're finished loading everything now
(shell-current-directory)

(maximize-frame)
(setq wg-session-load-on-start t)
(workgroups-mode 1)

(save-visited-files-mode t)
(server-start)
