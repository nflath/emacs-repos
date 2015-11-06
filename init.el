;;; init.el --- Entry point of my emacs initialization file

;;; Commentary:

;; FixMe: Only replace calendar.org if there are diffs
;; FixMe: Why are colors not working in ansi-term?
;; FixMe: why does whitespace-mode not seem to work?
;; FixMe: Investigate snippets
;; FixMe: Eldoc running in python - [2014-11-01 Sat] Posted on stackexchange
;; FixMe: Have flycheck messages override eldoc messages - [2014-10-22 Wed] Asked on emacs stack exchange
;; FixMe: Add filtering to ido (or try out helm-mode)
;; FixMe: have 't' in org-mode go straight from TODO->DONE, 'c' go from TODO->CANCELED (is this org-speed-commands?)
;; FixMe: Loading /Users/nflath/Dropbox/emacs-repos/init.el (source)...done
;; FixMe: Indentation after :config should increment
;; FixMe: Switch to use-package for everything
;; FixMe: Make the 'HIDDEN' bold
(setq debug-on-error t)

;; Bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
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
;; (setq load-dirs (concat emacs-repos-dir "internal"))
;; (condition-case nil
;;     (progn
;;       (require 'load-dir)
;;       (load-dirs)
;;       (message "Loaded directories")
;;       )
;;   (error nil))


(load-file (concat emacs-repos-dir "elget.el"))




(require `use-package)
(use-package load-dir
  :init (setq load-dirs (concat emacs-repos-dir "internal"))
  :config (load-dirs))

(use-package hungry-delete
  :config (global-hungry-delete-mode))

(use-package js2-mode
  :mode "\\.js$"
  :config
    (setq js2-bounce-indent-p t)
    (setq js2-highlight-level 3))

(use-package rainbow-delimeters-mode
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

;;; Load other customizations

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

(face-spec-set 'org-level-2 '((t (:foreground "#DC8CC3"))))

(provide 'init)
;;; init.el ends here
(setq read-quoted-char-radix 16)
