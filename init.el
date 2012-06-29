(setq warning-suppress-types nil)
(setq debug-on-error t)

;;Create required directories
(mapcar (lambda (dir) (mkdir dir t))
        '("~/.emacs.d/tmp"
          "~/.emacs.d/elisp"
          "~/.emacs.d/elpa"
          "~/.emacs.d/log"))

(let ((default-directory "~/.emacs.d/elpa"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;Loads emacs configuration
(load-file (concat emacs-repos-dir "internal/package.el"))
(require 'load-dir)
(setq load-dirs (mapcar (lambda (x) (concat emacs-repos-dir x "/"))
                       '("customization" "internal" "external")))
(load-dirs)

;;We're finished loading everything now
(provide 'init-finished)

(if (get-buffer "scratch.el")
    (kill-buffer "scratch.el"))
