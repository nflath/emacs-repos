(setq wgrep-enable-key "q")
;; FixMe: Add this to mv-shell.el
(mv-shell-mode 1)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)
(paren-activate)

(eval-after-load 'init-finished
  '(progn
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))

(add-hook 'org-mode 'org-bullets-mode)
