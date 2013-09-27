
;; global
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
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-o") 'split-line)
(global-set-key (kbd"C-x \\") 'align-regexp)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c w") (lambda () (interactive) (diff-buffer-with-file (current-buffer))))
(global-set-key (kbd "C-c i") 'bh/insert-inactive-timestamp)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "C-x n n") 'recursive-narrow-to-region)
(global-set-key (kbd "C-x n w") 'recursive-widen)
(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key "\C-x~" 'macro-math-eval-and-round-region)
(global-set-key "\C-x=" 'macro-math-eval-region)
(global-set-key [(control meta o)] 'loccur)
(global-set-key [(control shift o)] 'loccur-previous-match)

;; c-mode-base
(define-key c-mode-base-map [remap newline-and-indent] 'continue-string-if-necessary)
(define-key c-mode-base-map (kbd "C-c c f") 'c-fwdinclude)

;; c-mode
(define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)

;; c++-mode
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

;; dired-mode
(define-key dired-mode-map (kbd "c") 'dired-do-load)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;; emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; visual-line-mode
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)
(define-key visual-line-mode-map (kbd "C-e") 'end-of-visual-line)

;; org-mode
(define-key org-mode-map (kbd "C-<RET>" ) 'org-insert-heading-respect-content)
(define-key global-map "\C-cr" 'org-remember)
(define-key org-remember-mode-map (kbd "C-x C-s") 'org-remember-finalize)

;; dired-mode
;; FixMe: Create packaage dired-isearch-default-keybindings
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "C-M-r") 'dired-isearch-backward-regexp)

;; java-mode
(define-key java-mode-map (kbd "C-x j") 'javadoc-lookup)

(define-key jabber-chat-mode-map (kbd "M-p") 'jabber-chat-input-cycle-input)

(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)
