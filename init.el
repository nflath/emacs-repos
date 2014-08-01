(require 'cc-mode)
(setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))

(setq-default case-fold-search t)

;;; Debug errors when they arise
(setq-default debug-on-error t)

(defun eval-function-in ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (call-interactively 'eval-last-sexp)))

(defadvice org-cycle (after org-reactivate-always activate)
  (if (org-at-table-p)
      (org-table-recalculate t)))

;; FixMe: Make ERC readable
;; FixMe: Make CANCELLED be a state in org-mode
;; FixMe: which-function in C++
;; FixMe: cache javadoc locally
;; FixMe: investigate breadcrumbs
;; FixMe: Keybindings should all be (eval-after-load)ed
;; FixMe: Have eldoc also print the values of variables

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

;;; Load packages using el-get
(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))


;;; Load other customizations
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                        '("internal" "external")))
(load-dirs)
(load-file "~/Dropbox/logins.el")

;;We're finished loading everything now
(server-start)
(setq save-visited-files-auto-restore t)
(save-visited-files-mode t)
(shell-current-directory)
(mv-shell-mode)

(maximize-frame)
