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
        expand-region
        ace-jump-mode
        pager
        pager-default-keybindings

        ;; Emacs editing improvements
        hungry-delete
        recursive-narrow
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
        graphviz-dot-mode
        go-mode

        ;; General utility functions
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

;; FixMe: Can these functions be anywhere else?
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
             (package-install sym))))
    (error sym)))

(mapcar 'try-package-install my-packages)
(mapcar 'try-require my-packages)

;; FixMe: These should be default
(global-rainbow-delimiters-mode)
(global-hungry-delete-mode)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
(auto-indent-global-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq htmlize-html-major-mode 'html-mode)
(paren-activate)

;; FixMe: Move to a different file?
(setq js2-bounce-indent-p t)
(setq js2-highlight-level 3)
(setq markdown-enable-math t)
(setq wgrep-enable-key "q")
