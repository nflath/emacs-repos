(setq warning-suppress-types nil)
;;Create required directories
(mapcar (lambda (dir) (mkdir dir t))
        '("~/.emacs.d/tmp"
          "~/.emacs.d/elisp"
          "~/.emacs.d/log"))

(let ((default-directory (concat emacs-repos-dir "supported/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "red"))) t))

;;Loads emacs configuration
(load-file (concat emacs-repos-dir "customization/prog-util.el"))
(load-file (concat emacs-repos-dir "customization/org.el"))
(load-directory (concat emacs-repos-dir "internal/"))
(load-directory (concat emacs-repos-dir "external/"))

;;We're finished loading everything now
(provide 'init-finished)

(if (get-buffer "scratch.el")
    (kill-buffer "scratch.el"))

