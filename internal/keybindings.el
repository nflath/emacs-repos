;;; Sets custom keybindings

(global-set-key (kbd "C-x y") 'copy-whole-line)
(global-set-key [f1] 'help-anything)
(global-set-key (kbd "C-c O") 'swap-windows)
(global-set-key (kbd "C-x g") 'grep-with-defaults)
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-c b m") 'move-buffer-file)
(global-set-key (kbd "C-a") 'nflath-cycle-bol)
(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
