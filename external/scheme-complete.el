;;; Auto-completion and eldoc for scheme
(define-key scheme-mode-map "\t" 'scheme-complete-or-indent)

(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))
