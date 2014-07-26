(defun isearch-switch-to-regexp (direction)
  (setq isearch-regexp t)
  (setq isearch-word nil)
  (setq isearch-success t isearch-adjusted t)
  (isearch-repeat direction)
  (isearch-update))

(defun isearch-switch-to-regexp-backward ()
  (interactive)
  (isearch-switch-to-regexp 'backward))

(defun isearch-switch-to-regexp-forward ()
  (interactive)
  (isearch-switch-to-regexp 'forward))

(defun isearch-switch-to-occur ()
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(setq-default isearch-case-fold-search t)
