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
(require 'wgrep)
(setq wgrep-enable-key "q")

(require 'recursive-narrow)

(require 'mv-shell)
(mv-shell-mode 1)

(require 'gdb-shell)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)

(require 'pager)

(require 'guess-offset)

(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(require 'mic-paren)
(paren-activate)

(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

(require 'hungry-delete)
(add-hook 'prog-mode-hook 'turn-on-hungry-delete-mode)
(add-hook 'org-mode-hook 'turn-on-hungry-delete-mode)

(require 'save-visited-files)
(eval-after-load 'init-finished
  '(progn
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))

(require 'dired-isearch)

(require 'icomplete+)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
