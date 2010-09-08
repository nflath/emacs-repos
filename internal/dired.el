;;Dired is a mode for navigating and editing directories.  It shows a display like ls -l, and you can mark files and
;;delete them or perform other operations en masse.  Wdired allows editing the listing as text; the changes you make
;;will be saved when you execute save-buffer.  dired-x gives some extra commands that you can use.

(require 'wdired)
(require 'dired-x)
(setq wdired-allow-to-change-permissions 'advanced)
(setq dired-auto-revert-buffer t)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

(defun eval-files-dired (arg)
  "Evaluate dired-marked files."
  (interactive "P")
  (mapcar 'load-file
          (dired-get-marked-files nil arg)))
(define-key dired-mode-map (kbd "c") 'eval-files-dired)