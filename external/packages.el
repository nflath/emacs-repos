;;; Customization for package.el packages that don't require a full file of their own

;;; Major modes
;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" .yaml-mode))

;; haml-mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq markdown-enable-math t)

;; graphviz-dot-mode


;;; Eldoc
;; c-eldoc
(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;eldoc-eval
(require 'eldoc-eval)
(setq eldoc-show-in-mode-line-delay 12)
