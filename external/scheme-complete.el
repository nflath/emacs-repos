(require 'scheme)
(require 'scheme-complete)
(define-key scheme-mode-map "\t" 'scheme-complete-or-indent)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))
