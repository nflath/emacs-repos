(defun isearch-switch-to-regexp (direction)
  (setq isearch-regexp t)
  (setq isearch-word nil)
  (setq isearch-success t isearch-adjusted t)
  (isearch-repeat direction)
  (isearch-update))

(define-key isearch-mode-map (kbd "C-M-r") (lambda ()
                                             (interactive)
                                             (isearch-switch-to-regexp 'backward)))

(define-key isearch-mode-map (kbd "C-M-s") (lambda ()
                                             (interactive)
                                             (isearch-switch-to-regexp 'forward)))

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))