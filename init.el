;; FixMe: Add missing functions to man-mode
;; FixMe: cache javadoc locally
;; FixMe: M-RET in org-mode doesn't seem to work
;; FixMe: C-s doesn't work in dired
;; FixMe: Work in jabber-chat
;; FixMe: M-x M-p doesn't work
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
;; FixMe: fullscreen command
;; FixMe: dired - no file on this line??
;; FixMe: Have google-chrome be a program on command line

(add-to-list 'exec-path "/usr/local/git/bin")

(defun hexrgb-canonicalize-defined-colors (&rest args) ) ;; FixMe: What is using this?
(defun dired-get-filename (&rest args) ()) ;; FixMe: What is using this?
(defun plist-to-alist (args) );; FixMe: What is using this?

(setq-default debug-on-error t)
(add-to-list 'load-path "~/Dropbox/.emacs.d/el-get/el-get")


(setq user-emacs-directory "~/Dropbox/.emacs.d")
(setq emacs-repos-dir "~/Dropbox/emacs-repos/")
(defvar org-directory "~/Dropbox/org/" "Location of org files")
(set-face-attribute 'default nil :height 90)
(setq warning-suppress-types nil)

(let ((default-directory "~/Dropbox/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;;Create required directories
(mapcar (lambda (dir) (mkdir dir t))
        '("~/Dropbox/.emacs.d/tmp"
          "~/Dropbox/.emacs.d/elisp"
          "~/Dropbox/.emacs.d/elpa"
          "~/Dropbox/.emacs.d/log"))

(let ((default-directory "~/Dropbox/.emacs.d"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; ;; Requires the following installed:
(load-file (concat emacs-repos-dir "elget.el"))
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                       '("customization" "internal" "external")))

(load-dirs)

;;We're finished loading everything now
(provide 'init-finished)

(if (get-buffer "scratch.el") (kill-buffer "scratch.el"))

(setq yas/root-directory "~/Dropbox/emacs-repos/snippets/")
(yas/load-directory yas/root-directory)
(yas/global-mode)

defun

(defun  ()
   "thisandthat."
   (interactive)
   (let (var1)
     (setq var1 some)

     )
   )

(xterm-mouse-mode)
