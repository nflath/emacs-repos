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
        saveplace
        subword
        windmove
        imenu
        elisp-slime-nav-mode
        isearch-switch

        ;; Emacs editing improvements
        hungry-delete
        recursive-narrow
        smart-whitespace-comment-fixup
        abbrev
        move-line
        company
        company-c-headers
        company-go

        ;; General programming utilities
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
        js2-mode
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
        ))
;;; Download and require all packages
(setq failed-requires ())
(defun try-require (sym)
  (condition-case nil
      (require sym)
    (error (setq failed-requires (append failed-requires (list sym))))))

(setq failed-installs ())
(defun try-package-install (sym)
  (condition-case nil
      (progn
        (if ((not package-installed-p sym)
             (package-install sym))))
    (error (setq failed-installs (append failed-installs (list sym))))))

(mapcar 'try-package-install my-packages)
(mapcar 'try-require my-packages)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)
(global-hungry-delete-mode)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
(auto-indent-global-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq htmlize-html-major-mode 'html-mode)
(paren-activate)
(setq js2-bounce-indent-p t)
(setq js2-highlight-level 3)
(setq markdown-enable-math t)
(setq wgrep-enable-key "q")
(sml/setup)
(setq browse-kill-ring-highlight-current-entry t)
(add-hook 'after-init-hook 'global-company-mode)

(require 'anzu)
(global-anzu-mode +1)
(global-rainbow-delimiters-mode nil)
