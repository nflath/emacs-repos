;; FixMe: M-RET in org-mode doesn't seem to work
;; FixMe: cache javadoc locally
;; FixMe: nflath@optumsoft.com in jabber-chat
;; FixMe: ';;' at end of line do not match up when indenting
;; FixMe: Is there a way to fin out how many columns are visible?
;; FixMe: global modes should be added to packages
;; FixMe: Have ~/.emacs.d just symlink to ~/Dropbox/.emacs.d
;; FixMe: Have eldoc also print the values of variables
;; FixMe: eval-function-in and keybinding: evaluate the function you are currently inside
;; FixMe: investigate windmove
;; FixMe: Send email from emacs - including contacts from google?
;; FixMe: Install ispell on mac OS x
;; FixMe: browse-URL on OS x doesn't work
;; FixMe: fullscreen command on OSX
;; FixMe: Have google-chrome work be a program on command line

;; FixMe: submit pager-default-commands to marmalade, check into github, remove internal/pager.el, add to el-get
;; FixMe: Add missing functions to man-mode - need to push and update
;; FixMe: dired - no file on this line??



;;; Debug errors when they arise
(setq-default debug-on-error t)

;;; Add git to our exec-path
(add-to-list 'exec-path "/usr/local/git/bin")

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
                        '("customization" "internal" "external")))
(load-dirs)

;;We're finished loading everything now
(provide 'init-finished)
