;;; Sets custom keybindings

;; global
(global-set-key (kbd "M-s") 'ido-goto-symbol)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x y") 'copy-whole-line)
(global-set-key [f1] 'help-anything)
(global-set-key (kbd "C-c O") 'swap-windows)
(global-set-key (kbd "C-x g") 'grep-with-defaults)
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-c b m") 'move-buffer-file)
(global-set-key (kbd "C-a") 'nflath-cycle-bol)
(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
(global-set-key (kbd "C-x s" ) 'ido-shell)
(global-set-key (kbd "C-c s") 'shell-current-directory)
(global-set-key (kbd "C-c m") 'insert-email)
(global-set-key (kbd "C-c f") 'flyspell-add-word)

;; c-mode-base
(define-key c-mode-base-map [remap newline-and-indent] 'continue-string-if-necessary)
(define-key c-mode-base-map (kbd "C-c c f") 'c-fwdinclude)

;; c-mode
(define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)

;; c++-mode
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

;;dired-mode
(define-key dired-mode-map (kbd "c") 'dired-do-load)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
