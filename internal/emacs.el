(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(display-battery-mode t)
(setq eval-expression-print-length nil)
(display-time-mode t)
(setq frame-title-format (concat invocation-name "@" system-name ": %b %+%+ %f"))
(when window-system (global-unset-key "\C-z"))
(toggle-truncate-lines 1)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)
(setq-default fill-column 120)
(setq line-move-visual nil)
(windmove-default-keybindings)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(setq scroll-preserve-screen-position t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq apropos-do-all t)
(setq message-log-max t)
(setq pop-up-windows nil)
(setq visible-bell t)
(setq-default case-fold-search t)
(setq kill-read-only-ok t)
(setq x-select-enable-clipboard t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq disabled-command-function nil)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tabify-regexp "^[ \t]+")
(setq diff-switches "-u")
(global-reveal-mode nil)
(eval-after-load 'init-finished
  '(server-start))

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;History features
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq savehist-file "~/.emacs.d/.savehist")
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring)    ;; ... my search entries
      )
(savehist-mode 1)

;;Improve the buffer menu
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-elide-long-columns t)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)
(global-set-key  (kbd "C-x C-b")        'ibuffer-other-window)

;;Rename duplicate buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;Display tooltips in echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0)
(help-at-pt-set-timer)

;;Auto-revert PDFs and images
(add-hook 'doc-view-mode-hook 'turn-on-auto-revert-mode)

;;Set the major mode on new buffers according to the buffer name.
(setq-default major-mode (lambda ()
                           (let ((buffer-file-name (replace-regexp-in-string "\\.tmp$" ""
                                                                             (or buffer-file-name (buffer-name)))))
                             (set-auto-mode)
                             )))

;;Some better keybindings
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-o") 'split-line)
;;(require 'log-edit-mode)
;;(define-key log-edit-mode-map (kbd "C-x C-s") 'log-edit-done)
(define-key global-map [(control meta o)] 'loccur)
(define-key global-map [(control shift o)] 'loccur-previous-match)
(global-set-key (kbd"C-x \\") 'align-regexp)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c w") (lambda () (interactive) (diff-buffer-with-file (current-buffer))))
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)
(define-key visual-line-mode-map (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-x f") 'ido-find-file-other-window)

;;Backup to only use one directory
(mkdir "~/.emacs.d/data/emacs-backups/" t)
(setq backup-directory-alist `(("." . "~/.emacs.d/emacs-backups/")))
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 100)
(setq kept-old-versions 100)
(setq backup-by-copying t)
(add-hook 'before-save-hook
          '(lambda ()
             (setq buffer-backed-up nil)))

;;CUA-mode rectangles
(setq cua-enable-cua-keys nil)
(cua-mode t)

;;Turn on auto-save
(setq auto-save-default t)
(setq auto-save-list-file-name "~/.emacs.d/.saves")

;;Initialize a frame
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen) on an X display."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun w32-maximize-frame ()
  "Maximize the current frame on a Windows machine."
  (interactive)
  (w32-send-sys-command 61488))

(defun maximize-frame ()
  "Maximizes the Emacs frame."
  (interactive)
  (if window-system
      (if (fboundp 'x-send-client-message)
          (x11-maximize-frame)
        (w32-maximize-frame))))
(eval-after-load 'init-finished
  '(maximize-frame))

;;subword movement
(require 'subword)
(global-subword-mode t)

;;Better M-x
(icomplete-mode 1)
(setq icomplete-compute-delay 0)

;;Ido is a mode for easier selection of items in the minibuffer.  It's main uses
;;are enhancing find-file and switch-buffer.  Ido provides 'flex matching', as
;;well as searching recent directories if no matches are found.  It will also
;;default to the file at point if it exists.
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(defadvice ido-find-file (around dired-dont-guess activate)
  (let ((ido-use-filename-at-point nil))
    ad-do-it))
(setq ido-max-prospects 0)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-mode t)
(ido-everywhere t)
(setq ido-default-buffer-method 'selected-window)

(setq only-global-abbrevs nil)
(require 'abbrev)
(setq-default abbrev-mode t)

(winner-mode t)

;;Tramp allows remote access to files by using find-file with
;;/ssh:user@host:path/to/file.  It is too slow for regular use, but it can still
;;sometimes be useful.  We want it to use ssh by default to make it slightly
;;more bearable.
(require 'tramp)
(setq tramp-default-method "ssh")

(defun sudo-edit (&optional arg)
  "Find a file and open it as root."
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  "Edit the current file as root."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char pos)))
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

(add-to-list 'completion-ignored-extensions ".class")
(add-to-list 'completion-ignored-extensions ".exe")
(add-to-list 'completion-ignored-extensions ".o")
(add-to-list 'completion-ignored-extensions ".dvi")
(add-to-list 'completion-ignored-extensions ".ps")

;; Avoid those horrible "File %s changed on disk.  Reread from disk?" messages.
(setq revert-without-query '(".*"))
;; And same for "buffer %s has changed on disk.  Really edit?"
(defun ask-user-about-supersession-threat (filename)
  (message "Reverting file %s..." filename)
  (revert-buffer t t)
  (message "Reverting file %s... done" filename))

(setq whitespace-style '(lines-tail))
(setq whitespace-line-column 85)
(add-hook-to-all programming-major-mode-hooks '(lambda () (whitespace-mode 1)))
(add-hook-to-all programming-major-mode-hooks '(lambda () (setq fill-column 79)))
(setq tab-always-indent 'complete)

(setq split-height-threshold 80)
(setq split-width-threshold 160)

(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

(global-auto-revert-mode t)

(require 'windmove)
(windmove-default-keybindings 'super)
(set-default 'imenu-auto-rescan t)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)

(package-initialize)
(package-refresh-contents)
(setq nxml-slash-auto-complete-flag t)

(setq packages-to-install '(auctex
                            auto-complete
                            auto-complete-etags
                            auto-indent-mode
                            c-eldoc
                            calfw-gcal
                            clojure-mode
                            clojure-test-mode
                            cmake-mode
                            color-theme
                            ctags cygwin-mount
                            durendal
                            ecb
                            eldoc-eval
                            elisp-cache
                            elisp-slime-nav
                            find-file-in-project
                            fuzzy-match
                            graphviz-dot-mode
                            haskell-mode
                            haml-mode
                            highlight-parentheses
                            icomplete+
                            ipython
                            js2-mode
                            load-dir
                            magit
                            magit-simple-keys
                            magithub
                            markdown-mode
                            marmalade
                            mic-paren
                            org
                            org-email
                            org-magit
                            pep8
                            powershell
                            pyflakes
                            pylint
                            pytest
                            python
                            python-mode
                            python-pep8
                            python-pylint
                            rainbow-mode
                            rainbow-delimiters
                            scratch
                            setup-cygwin
                            slime
                            slime-clj
                            switch-window
                            slime-fuzzy
                            slime-repl
                            smart-operator
                            smart-tab
                            swank-cdt
                            w32-browser
                            wgrep
                            yasnippet
                            dired-isearch
                            emms
                            facebook
                            gdb-shell
                            htmlize
                            hungry-delete
                            log4j-mode
                            mv-shell
                            nxml-mode
                            twitter
                            w3
                            macro-math
                            javadoc-help
                            ioccur
                            save-visited-files
                            scheme-complete
                            winpoint
                            yaml-mode))

(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p)))

(setq revert-without-query '(".*"))
;; And same for "buffer %s has changed on disk.  Really edit?"
(defun ask-user-about-supersession-threat (filename)
  (message "Reverting file %s..." filename)
  (revert-buffer t t)
  (message "Reverting file %s... done" filename))

(setq c-basic-offset 3)

(setq auto-mode-alist
      (append '(("\\.c$"                      . c-mode)
                ("\\.cc$"                     . c++-mode)
                ("\\.C$"                      . c++-mode)
                ("\\.CC$"                     . c++-mode)
                ("\\.h$"                      . c-mode)
                ("\\.hh$"                     . c++-mode)
                ("\\.H$"                      . c-mode)
                ("\\.HH$"                     . c++-mode)
                ("\\.cpp$"                    . c++-mode)
                ("\\.CPP$"                    . c++-mode)
                ("\\.hpp$"                    . c++-mode)
                ("\\.HPP$"                    . c++-mode)
                ("\\.\\([pP][Llm]\\|al\\)$"   . perl-mode)
                ("\\`/var/tmp/"               . text-mode)
                ("\\.tac$"                    . tacc-mode)
                ("\\.tin$"                    . c++-mode)
                ("\\.itin$"                    . c++-mode)
                )
              auto-mode-alist))
