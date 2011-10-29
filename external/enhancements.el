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
(add-hook 'prog-mode-hook (lambda () (smart-operator-mode)))
