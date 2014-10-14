(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode t)

(setq eval-expression-print-length nil) ;; FixMe: have this not be true in eldoc and print vars

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

(add-hook 'before-save-hook (lambda () (if (not (eq major-mode 'org-mode)) (delete-trailing-whitespace))))

;;History features
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

(setq savehist-file "~/.emacs.d/.savehist")
(setq savehist-additional-variables
      '(search ring regexp-search-ring))
(savehist-mode 1)

;;Improve the buffer menu
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-elide-long-columns t)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)

;; Rename duplicate buffers
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
(mkdir "~/.emacs.d/emacs-backups/" t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/emacs-backups/")))
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 1000)
(setq kept-old-versions 1000)
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(add-hook 'before-save-hook
          '(lambda ()
             (setq buffer-backed-up nil)))

(defadvice find-backup-file-name (around add-timestamp activate)
  (let ((filename ad-do-it))
    (setq ad-return-value (list (concat (car filename) (format-time-string "%Y-%m-%d %T"))))))

;; CUA-mode rectangles
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Turn on auto-save
(setq auto-save-default t)
(setq auto-save-list-file-name "~/.emacs.d/.saves")

;; subword movement
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

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

;; Turn abbrev mode on
(setq only-global-abbrevs nil)
(setq-default abbrev-mode t)

;; Record changes in window configuration
(winner-mode t)

;; Tramp allows remote access to files by using find-file with
;; /ssh:user@host:path/to/file.  It is too slow for regular use, but it can still
;; sometimes be useful.  We want it to use ssh by default to make it slightly
;; more bearable.
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
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Windmove - use keys to move windows
(windmove-default-keybindings 'super)

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

(defun browse-url-default-macosx-browser-prepend-http (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let
      ((url (if (string-prefix-p "http" url) url (concat "http://" url))))
    (message (concat "url: "  url))
    (start-process (concat "open " url) nil "open" url)))

(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-browser-function 'browse-url-default-macosx-browser-prepend-http)

(setq browse-url-generic-program "google-chrome")
(set-face-attribute 'default nil :height 90)
(xterm-mouse-mode)

;; Always grab locks
(defun ask-user-about-lock (FILE OPPONENT) t)


(add-hook 'find-file-hook (lambda () (make-directory default-directory t) ))


(defun after-find-file (&optional error warn noauto
                                  _after-find-file-from-revert-buffer
                                  nomodes)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables.
Optional args ERROR, WARN, and NOAUTO: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
NOAUTO means don't mess with auto-save mode.
Fourth arg AFTER-FIND-FILE-FROM-REVERT-BUFFER is ignored
\(see `revert-buffer-in-progress-p' for similar functionality).
Fifth arg NOMODES non-nil means don't alter the file's modes.
Finishes by calling the functions in `find-file-hook'
unless NOMODES is non-nil."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
           (msg
            (cond
             ((not warn) nil)
             ((and error (file-attributes buffer-file-name))
              (setq buffer-read-only t)
              (if (and (file-symlink-p buffer-file-name)
                       (not (file-exists-p
                             (file-chase-links buffer-file-name))))
                  "Symbolic link that points to nonexistent file"
                "File exists, but cannot be read"))
             ((not buffer-read-only)
              (if (and warn
                       ;; No need to warn if buffer is auto-saved
                       ;; under the name of the visited file.
                       (not (and buffer-file-name
                                 auto-save-visited-file-name))
                       (file-newer-than-file-p (or buffer-auto-save-file-name
                                                   (make-auto-save-file-name))
                                               buffer-file-name))
                  (format "%s has auto save data; consider M-x recover-this-file"
                          (file-name-nondirectory buffer-file-name))
                (setq not-serious t)
                (if error "(New file)" nil)))
             ((not error)
              (setq not-serious t)
              "Note: file is write protected")
             ((file-attributes (directory-file-name default-directory))
              "File not found and directory write-protected")
             ((file-exists-p (file-name-directory buffer-file-name))
              (setq buffer-read-only nil))
             (t
              (setq buffer-read-only nil)
              ))))
      (when msg
        (message "%s" msg)
        (or not-serious (sit-for 1 t))))
    (when (and auto-save-default (not noauto))
      (auto-save-mode 1)))
  ;; Make people do a little extra work (C-x C-q)
  ;; before altering a backup file.
  (when (backup-file-name-p buffer-file-name)
    (setq buffer-read-only t))
  ;; When a file is marked read-only,
  ;; make the buffer read-only even if root is looking at it.
  (when (and (file-modes (buffer-file-name))
             (zerop (logand (file-modes (buffer-file-name)) #o222)))
    (setq buffer-read-only t))
  (unless nomodes
    (when (and view-read-only view-mode)
      (view-mode-disable))
    (normal-mode t)
    ;; If requested, add a newline at the end of the file.
    (and (memq require-final-newline '(visit visit-save))
         (> (point-max) (point-min))
         (/= (char-after (1- (point-max))) ?\n)
         (not (and (eq selective-display t)
                   (= (char-after (1- (point-max))) ?\r)))
         (not buffer-read-only)
         (save-excursion
           (goto-char (point-max))
           (ignore-errors (insert "\n"))))
    (when (and buffer-read-only
               view-read-only
               (not (eq (get major-mode 'mode-class) 'special)))
      (view-mode-enter))
    (run-hooks 'find-file-hook)))
