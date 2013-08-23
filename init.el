;; FixMe: cache javadoc locally
;; FixMe: global modes should be added to packages
;; FixMe: Have eldoc also print the values of variables
;; FixMe: eval-function-in and keybinding: evaluate the function you are currently inside
;; FixMe: investigate windmove
;; FixMe: Send email from emacs
;; FixMe: browse-URL on OS x doesn't work
;; FixMe: fullscreen command on OSX


(setq-default ispell-program-name "/usr/local/bin/aspell")

;;; Debug errors when they arise
(setq-default debug-on-error t)

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
(load-file (concat emacs-repos-dir "elget.el"))

;;; Load other customizations
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                        '("internal" "external")))
(load-dirs)

;;We're finished loading everything now
(server-start)
(maximize-frame)
(setq save-visited-files-auto-restore t)
(save-visited-files-mode t)
(shell-current-directory)
