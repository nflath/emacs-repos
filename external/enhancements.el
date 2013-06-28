(setq wgrep-enable-key "q")
;; FixMe: Add this to mv-shell.el
(mv-shell-mode 1)
o
(eval-after-load 'init-finished
  '(progn
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))
