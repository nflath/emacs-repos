(setq wgrep-enable-key "q")
(add-hook 'org-mode 'org-bullets-mode)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)
(paren-activate)

(eval-after-load 'init-finished
  '(progn
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))
