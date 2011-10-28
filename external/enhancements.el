(setq wgrep-enable-key "q")
(mv-shell-mode 1)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(paren-activate)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(add-hook 'prog-mode-hook 'turn-on-hungry-delete-mode)
(add-hook 'org-mode-hook 'turn-on-hungry-delete-mode)
(eval-after-load 'init-finished
  '(progn
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
