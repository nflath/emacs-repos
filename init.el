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
(require 'load-dir)
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                       '("customization" "internal" "external" "optumsoft")))
(load-file (concat emacs-repos-dir "internal/package.el"))
(load-dirs)

;;We're finished loading everything now
(provide 'init-finished)

(if (get-buffer "scratch.el")
    (kill-buffer "scratch.el"))
