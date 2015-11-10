;;; init.el --- Entry point of my emacs initialization file

(setq debug-on-error t) ;; We want to debug errors.

;;; I-Z packages

;; Bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; List of packages to make sure are installed
(defvar my-packages
  `(
    ;; Packages used for initialization purposes
    ;;load-dir
    oauth2

    ;; Emacs UI improvements
    magit
    color-theme
    zenburn-theme
    mic-paren
    dirtrack-buffer-name-track-mode
    rainbow-delimiters
    highlight-symbol
    highlight-parentheses
    icomplete+
    scratch-persist
    occur-default-current-word
    frame-fns
    frame-cmds
    workgroups2
    save-visited-files
    ibuffer
    uniquify
    smart-mode-line
    browse-kill-ring
    key-chord

    ;; Emacs navigation improvements
    winpoint
    smartscan
    winpoint-ignore-dired
    smooth-scrolling
    jump-dls
    helm
    expand-region
    ace-jump-mode
    pager
    pager-default-keybindings
    saveplace
    subword
    windmove
    imenu
    elisp-slime-nav-mode
    isearch-switch

    ;; Emacs editing improvements
    ;;hungry-delete
    recursive-narrow
    smart-whitespace-comment-fixup
    abbrev
    move-line
    company
    company-c-headers
    company-go

    ;; General programming utilities
    flycheck
    java-file-create
    guess-offset
    org-table-comment
    auto-indent-mode
    flymake
    fixme-mode

    ;; Dired enhancements
    dired
    wdired
    dired-x
    dired-aux
    dired-nav-enhance

    ;; Shell-mode enhancements
    comint-better-defaults
    mv-shell
    ansi-color
    dirtrack

    ;; Eldoc improvements
    c-eldoc

    ;; Miscellanious major modes
    haml-mode
    markdown-mode
    ;js2-mode
    ssh-config-mode
    graphviz-dot-mode
    go-mode
    scheme
    cc-mode
    python

    ;; General utility functions
    org-publish-agenda
    buffer-file-utils
    exec-path-from-shell
    sudo-edit
    dedicated
    unbound
    loccur
    htmlize
    macro-math
    occur-x
    man-commands
    memory-usage
    ioccur
    go-play
    imgur
    google-contacts
    marmalade-upload
    calc

    ;; Communication
    jabber
    erc
    smtpmail
    tramp

    ;; Python-mode enhancements
    pyflakes
    pytest
    python-pep8
    python-pylint

    ;; Elisp programming libraries
    thingatpt+
    dirtree
    hook-utils
    cl

    ;; Org-mode
    org
    org-habit
    appt

    ;; Version control enhacements
    gitconfig-mode
    gitignore-mode
    git-commit

    ;; HTML in emacs...
    w3

    ;; Windows enhancements
    w32-browser

    ;; Documentation
    javadoc-lookup

    ;; Emacs usage information
    keywiz
    )
  "List of packages to install via package.el."
  )


(defvar emacs-repos-dir "~/Dropbox/emacs-repos/")
(defun custom-theme-load-confirm (&rest args)
  "Prevent theme from promting during load.  ARGS is ignored."
  t)

(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 3000)
(setq mac-command-modifier `meta)
(setq enable-local-eval t)
(setq enable-local-variables :all)

(require 'cc-mode)
(setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))

;;; Set the load path
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(setq failed-requires ())
(defun try-require (sym)
  (condition-case nil
      (require sym)
    (error (setq failed-requires (append failed-requires (list sym))))))
(setq failed-requires ())


;;; Set a few paths in order to determine where everything is
(defvar org-directory "~/Dropbox/org/" "Location of org files.")

;; Load private data (that I don't want in Github)
(load-file "~/Dropbox/private.el")

(load-file (concat emacs-repos-dir "org.el"))
(load-file (concat emacs-repos-dir "elget.el"))

(use-package load-dir
  :init
  (setq load-dirs (concat emacs-repos-dir "internal"))
  :config (load-dirs))

(use-package hungry-delete
  :config
    (global-hungry-delete-mode))

(use-package js2-mode
  :mode "\\.js$"
  :config
    (setq js2-bounce-indent-p t)
    (setq js2-highlight-level 3))

(use-package ace-jump-mode
  :bind
    (("C-'" . ace-jump-line-mode)
     ("M-'" . ace-jump-word-mode)
     ("C-." . ace-jump-mode)))

(use-package buffer-move
  :bind
    (("<C-M-down>" . buf-move-down)
     ("<C-M-up>"   . buf-move-up)
     ( "<C-M-left>" . buf-move-left)
     ("<C-M-right>" . buf-move-right)))

(use-package rainbow-delimeters-mode
  :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

(use-package isearch-mode
  :config
    (setq case-fold-search t))

(use-package flyspell
  :config
    (defadvice ispell-command-loop (before ispell-reverse-miss-list activate)
      "reverse the first argument to ispell-command-loop"
      (ad-set-arg 0 (reverse (ad-get-argument 0))))

    (when (= 0 (shell-command ispell-program-name))
      (add-hook 'text-mode-hook 'turn-on-flyspell-mode)
      (add-hook 'org-mode-hook 'turn-on-flyspell-mode)
      (add-hook 'latex-mode-hook 'turn-on-flyspell-mode)
      (add-hook 'LaTeX-mode-hook 'turn-on-flyspell-mode)
      (add-hook 'plain-tex-mode-hook 'turn-on-flyspell-mode))

    (defun flyspell-add-word (word)
      "Adds word to personal dictionary"
      (interactive (list (read-string (concat "Add word to personal dictionary <" (current-word) ">: "))))
      (when (string-equal word "") (setq word (current-word)))
      (ispell-send-string (concat "*" word "\n"))
      (ispell-send-string "#\n"))

    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package dired
  :config
  (setq dired-auto-revert-buffer t))

(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions 'advanced)
  :after dired)

(use-package dired-x
  :config
  (setq dired-omit-files
        (concat
         dired-omit-files
         "\\\\|"
         (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                 (seq bol "." (not (any "."))) ;; dot-files
                 (seq "~" eol)                 ;; backup-files
                 (seq bol "CVS" eol)           ;; CVS dirs
                 (seq ".class" eol)            ;; Compiled java files
                 (seq ".pyc" eol)              ;; Compiled python files
                 (regexp "TAGS")
                 ))))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  :after dired)

;; Only reset keybindings after downloading everything
(load-file (concat emacs-repos-dir "keybindings.el"))

(setq wg-session-load-on-start t)
(workgroups-mode 1)
(save-visited-files-mode t)
(server-start)

(add-to-list 'auto-mode-alist '("\\.tin$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . c++-mode))
;; FixMe: Get tac mode here

(exec-path-from-shell-initialize)
(add-to-list 'exec-path "~/bin")

(maximize-frame)

(provide 'init)
;;; init.el ends here
(setq read-quoted-char-radix 16)
