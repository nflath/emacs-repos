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





;;; Enhancements
;; wgrep
(require 'wgrep)
(setq wgrep-enable-key "q")

;; recursive-narrow
(require 'recursive-narrow)

;; mv-shell
(require 'mv-shell)
(mv-shell-mode 1)

;; gdb-shell
(require 'gdb-shell)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)

;; pager
(require 'pager)

;;guess-offset
(require 'guess-offset)

;;rainbow-mode
(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;; mic-paren
(require 'mic-paren)
(paren-activate)

;; highlight-parentheses
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; hungry-delete
(require 'hungry-delete)
(add-hook 'prog-mode-hook 'turn-on-hungry-delete-mode)
(add-hook 'org-mode-hook 'turn-on-hungry-delete-mode)

;; smart-operator
;; FIXME:  
;;(require 'smart-operator)
;;(add-hook 'prog-mode-hook 'smart-operator-mode)

;; diredisearch
(require 'dired-isearch)




;;; Utilities
;; htlmize
(require 'htmlize)
(setq htmlize-html-major-mode 'html-mode)

;; pastebin
(require 'pastebin)

;; p4
(require 'p4)
(setq p4-verbose nil)

;; marmalade

;; doc-mode
(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)

;; fuzzy-match
(require 'fuzzy-match)
