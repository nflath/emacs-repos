;;; Customization for package.el packages that don't require a full file of their own

;;; Major modes
;; FixMe: Are these necessary?
(add-to-list 'auto-mode-alist '("\\.yml" .yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq markdown-enable-math t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Eldoc
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;nxml
(setq nxml-slash-auto-complete-flag t)

;; utilities
(setq htmlize-html-major-mode 'html-mode)
