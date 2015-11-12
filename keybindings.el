(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x y") 'copy-whole-line)
(global-set-key [f1] 'help-anything)
(global-set-key (kbd "C-c O") 'swap-windows)
(global-set-key (kbd "C-x g") 'grep-with-defaults)
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-c b m") 'move-buffer-file)
(global-set-key (kbd "C-a") 'nflath-cycle-bol)
(global-set-key (kbd "C-SPC") 'cua-set-mark)
(global-set-key (kbd "C-x s" ) 'ido-shell)
(global-set-key (kbd "C-c s") 'shell-current-directory)
(global-set-key (kbd "C-c m") 'insert-email)
(global-set-key (kbd "C-c f") 'flyspell-add-word)
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'comment-indent-new-line)
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
(global-set-key (kbd "C-M-SPC") 'just-one-space)

;; emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-x i") 'eval-function-in)
(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)

;; visual-line-mode
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)
(define-key visual-line-mode-map (kbd "C-e") 'end-of-visual-line)

;; org-mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(define-key org-mode-map (kbd "C-<RET>" ) 'org-insert-heading-respect-content)
(define-key global-map "\C-cr" 'my-org-capture-dont-ask)
(define-key org-capture-mode-map (kbd "C-x C-s") 'org-capture-finalize)
(define-key global-map (kbd "C-c ,") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-heading-respect-content)

(define-key jabber-chat-mode-map (kbd "M-p") 'jabber-chat-input-cycle-input)


;; Add tab-completion to M-:
(define-key read-expression-map (kbd "M-/") 'lisp-complete-symbol)

(global-set-key  (kbd "C-x C-b")        'ibuffer-other-window)
(global-set-key (kbd "M-s") 'imenu)

(global-set-key (kbd "C-M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(global-set-key "\M-c" 'endless/capitalize)
(global-set-key "\M-l" 'endless/downcase)
(global-set-key "\M-u" 'endless/upcase)
