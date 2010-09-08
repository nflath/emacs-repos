;;dired-isearch provides a replacement set of search commands that only search through file names.
(require 'dired-isearch)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "C-M-r") 'dired-isearch-backward-regexp)