;;; Installs all the packages I rely on

;;; Ensure that all ELPA repositories are available
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-refresh-contents)

;;; List of packages to make sure are installed
(setq my-packages
      `(
        ;; Packages used for initialization purposes
        load-dir
        oauth2

        ;; Emacs UI improvements
        color-theme
        zenburn-theme
        mic-paren
        rainbow-delimiters
        highlight-parentheses
        icomplete+
        scratch-persist
        occur-default-current-word
        frame-fns
        frame-cmds
        workgroups2
        save-visited-files

        ;; Emacs navigation improvements
        winpoint
        winpoint-ignore-dired
        smooth-scrolling
        jump-dls
        helm
        eproject         ;; FixMe: configure
        expand-region
        ace-jump-mode
        pager
        pager-default-keybindings

        ;; Emacs editing improvements
        hungry-delete
        recursive-narrow
        kill-ring-ido  ;; FixMe: keybindings
        smart-whitespace-comment-fixup

        ;; Shell-mode enhancements
        comint-better-defaults
        mv-shell

        ;; Eldoc improvements
        c-eldoc

        ;; Miscellanious major modes
        haml-mode
        markdown-mode
        nxml
        js2-mode
        ssh-config-mode
        graphviz-dot-mode ; FixMe: Add provide to file
        go-mode

        ;; General utility functions
        buffer-move ;; FixMe: keybindings
        dedicated   ;; FixMe: Configure/remember
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

        ;; Communication
        jabber
        erc

        ;; Python-mode enhancements
        pyflakes
        pytest
        python-pep8
        python-pylint

        ;; M-x grep enhancements
        wgrep

        ;; General programming utilities
        java-file-create
        guess-offset
        org-table-comment
        auto-indent-mode

        ;; Elisp programming enhancements
        elisp-slime-nav ;; FixMe: Add to jump-dls?

        ;; Elisp programming libraries
        thingatpt+
        dirtree
        hook-utils

        ;; Org-mode
        org
        org-habit

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

        ;; Emacs-internal packages FixMe: reorganize
        cl
        scheme
        appt
        cc-mode
        wdired
        dired-x
        dired-aux
        saveplace
        ibuffer
        uniquify
        subword
        abbrev
        tramp
        windmove
        smtpmail
        flymake
        python
        ansi-color
        dirtrack
        imenu
        ))
;;; Download and require all packages

 (defun my-filter (condp lst)
   (delq nil
         (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(setq failed-requires ())
(defun try-require (sym)
  (condition-case nil
      (require sym)
    (error sym)))

(setq failed-installs ())
(defun try-package-install (sym)
  (condition-case nil
      (progn
        (if ((not package-installed-p sym)
             (package-install sym)))
        (try-require sym))
    (error sym)))

(mapcar 'try-package-install my-packages)

;; FixMe: These should be default
(global-rainbow-delimiters-mode)
(global-hungry-delete-mode)

;; FixMe: This should be default
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; FixMe: This should be default
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)

;; FixMe: This should be on by default
(auto-indent-global-mode)

(dolist (hook  '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; To guess variables when a major mode is loaded, add `guess-style-guess-all'
;; to that mode's hook like this:
(add-hook 'c-mode-common-hook 'guess-style-guess-all)

(setq js2-bounce-indent-p t)
(setq js2-highlight-level 3)
(setq markdown-enable-math t)
(setq nxml-slash-auto-complete-flag t)
(setq wgrep-enable-key "q")

;; FixMe: This should be default
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; FixMe: This should be default
(setq htmlize-html-major-mode 'html-mode)

;; FixMe: This should be on by default
(paren-activate)
