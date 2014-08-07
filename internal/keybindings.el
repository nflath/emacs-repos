;; FixMe: Move these to the correct file instead of in one
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

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)

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
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

;; dired-mode
(define-key dired-mode-map (kbd "c") 'dired-do-load)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;; emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "M-.") 'jump-symbol-at-point)
(define-key emacs-lisp-mode-map (kbd "C-x i") 'eval-function-in)

;; visual-line-mode
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)
(define-key visual-line-mode-map (kbd "C-e") 'end-of-visual-line)

;; org-mode
(define-key org-mode-map (kbd "C-<RET>" ) 'org-insert-heading-respect-content)
(define-key global-map "\C-cr" 'my-org-capture-dont-ask)
(define-key org-capture-mode-map (kbd "C-x C-s") 'org-capture-finalize)
(define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-heading-respect-content)

;; dired-mode
(setq dired-isearch-filenames t)

;; java-mode
(define-key java-mode-map (kbd "C-x j") 'javadoc-lookup)

(define-key jabber-chat-mode-map (kbd "M-p") 'jabber-chat-input-cycle-input)

(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)

;; Add tab-completion to M-:
(define-key read-expression-map (kbd "M-/") 'lisp-complete-symbol)

(define-key python-mode-map (kbd "C-;") 'my-insert-self)

(define-key isearch-mode-map (kbd "C-M-r") 'isearch-switch-to-regexp-backward)
(define-key isearch-mode-map (kbd "C-M-r") 'isearch-switch-to-regexp-forward)
(define-key isearch-mode-map (kbd "C-o") `isearch-switch-to-occur)

(global-set-key  (kbd "C-x C-b")        'ibuffer-other-window)

(global-set-key (kbd "M-s") 'imenu)

(windmove-default-keybindings 'M)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-'") 'ace-jump-line-mode)
(global-set-key (kbd "M-'") 'ace-jump-word-mode)


(global-set-key (kbd "<C-M-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-right>")  'buf-move-right)
