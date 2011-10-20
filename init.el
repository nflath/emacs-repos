(setq warning-suppress-types nil)

;;Create required directories
(mapcar (lambda (dir) (mkdir dir t))
        '("~/.emacs.d/tmp"
          "~/.emacs.d/elisp"
	  "~/.emacs.d/elpa"
          "~/.emacs.d/log"))

(let ((default-directory (concat emacs-repos-dir "supported/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/.emacs.d/elpa"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;Loads emacs configuration
(load-file (concat emacs-repos-dir "customization/prog-util.el"))
(load-file (concat emacs-repos-dir "customization/org.el"))
(load-directory (concat emacs-repos-dir "internal/"))
(load-directory (concat emacs-repos-dir "external/"))
(load-directory (concat emacs-repos-dir "optumsoft/"))

;;We're finished loading everything now
(provide 'init-finished)

(if (get-buffer "scratch.el")
    (kill-buffer "scratch.el"))
