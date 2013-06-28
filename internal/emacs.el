(setq mac-command-modifier `meta)
(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode t)
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
(setq resize-mini-windows nil)
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;History features
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq savehist-file "~/.emacs.d/.savehist")
(setq savehist-additional-variables
      '(search ring regexp-search-ring))
(savehist-mode 1)

;;Improve the buffer menu
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-elide-long-columns t)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)
(global-set-key  (kbd "C-x C-b")        'ibuffer-other-window)

;; Rename duplicate buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Display tooltips in echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0)
(help-at-pt-set-timer)

;; Auto-revert PDFs and images
(add-hook 'doc-view-mode-hook 'turn-on-auto-revert-mode)

;; Set the major mode on new buffers according to the buffer name.
(setq-default major-mode
              (lambda ()
                (let ((buffer-file-name
                       (replace-regexp-in-string
                        "\\.tmp$" ""
                        (or buffer-file-name (buffer-name)))))
                  (set-auto-mode)
                  )))

;; Backup should only use one directory instead of sending
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

;; CUA-mode rectangles
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Turn on auto-save
(setq auto-save-default t)
(setq auto-save-list-file-name "~/.emacs.d/.saves")

;; Maximize the frame
(eval-after-load 'init-finished
  '(maximize-frame))

;; subword movement
(require 'subword)
(global-subword-mode t)

;; Better M-x
(icomplete-mode 1)
(setq icomplete-compute-delay 0)

;; Ido is a mode for easier selection of items in the minibuffer.  It's main uses
;; are enhancing find-file and switch-buffer.  Ido provides 'flex matching', as
;; well as searching recent directories if no matches are found.  It will also
;; default to the file at point if it exists.
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

;; Turn abbrev mode on
(setq only-global-abbrevs nil)
(require 'abbrev)
(setq-default abbrev-mode t)

;; Record changes in window configuration
(winner-mode t)

;; Tramp allows remote access to files by using find-file with
;; /ssh:user@host:path/to/file.  It is too slow for regular use, but it can still
;; sometimes be useful.  We want it to use ssh by default to make it slightly
;; more bearable.
(require 'tramp)
(setq tramp-default-method "ssh")

;; Use tab to complete if possible
(setq tab-always-indent 'complete)

;; Ignore files when completing
(add-to-list 'completion-ignored-extensions ".class")
(add-to-list 'completion-ignored-extensions ".exe")
(add-to-list 'completion-ignored-extensions ".o")
(add-to-list 'completion-ignored-extensions ".dvi")
(add-to-list 'completion-ignored-extensions ".ps")

;; Have lines be max 80 chars
(setq whitespace-style '(lines-tail))
(setq whitespace-line-column 85)
(add-hook 'prog-mode-hook '(lambda () (whitespace-mode 1)))
(add-hook 'prog-mode-hook '(lambda () (setq fill-column 79)))

;; When to split windows
(setq split-height-threshold 80)
(setq split-width-threshold 160)

(electric-indent-mode t)

;; Always revert
(global-auto-revert-mode t)

;; Windmove - use keys to move windows
(require 'windmove)
(windmove-default-keybindings 'super)

;; Set Imenu to always rescan
(set-default 'imenu-auto-rescan t)

;; Get rid of annoying prompts
(setq revert-without-query '(".*"))
(defun ask-user-about-supersession-threat (filename)
  (message "Reverting file %s..." filename)
  (revert-buffer t t)
  (message "Reverting file %s... done" filename))

;; This causes VC to not longer destroy your window configuration.
(setq vc-delete-logbuf-window t)

;;git is balls slow on windows, causing slow file opens
(if (eq window-system 'w32)
    (setq vc-handled-backends nil))

;; Wrap lines by default
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Use Chrome as the browser
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "google-chrome")

(set-face-attribute 'default nil :height 90)

(xterm-mouse-mode)

;; Always grab locks
(defun ask-user-about-lock (FILE OPPONENT) t)
