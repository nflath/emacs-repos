;;; Configuration for el-get
;;; Installs all the packages I rely on

;;; Ensure that all ELPA repositories are available
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))


;;; Make sure that we have *some* version of el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; Make sure that recipes are build for emacswiki and elpa (only done if necessary
(add-to-list 'el-get-recipe-path "~/Dropbox/emacs-repos/recipes")
(if (not (file-exists-p "~/Dropbox/.emacs.d/el-get/recipes/elpa"))
    (el-get-elpa-build-local-recipes))
(if (not (file-exists-p "~/Dropbox/.emacs.d/el-get/recipes/emacswiki"))
    (el-get-emacswiki-build-local-recipes))

;;; List of packages to make sure are installed
(setq my:el-get-packages
      `(
        ;; Packages used for initialization purposes
        el-get
        package
        load-dir
        sys

        ;; Emacs UI improvements
        color-theme
        zenburn
        mic-paren
        rainbow-delimiters
        highlight-parentheses
        icomplete+
        pretty-mode-plus    ;; FixMe: Add matches in org.el
        highlight-80+       ;; FixMe: Have highlight change immediately when fill-column changes
        scratch-persist
        occur-default-current-word

        ;; Emacs navigation improvements
        winpoint
        smooth-scrolling
        sourcepair    ;; FixMe: set keybindings
        pc-keys
        highlight-symbol ;; FixMe: set keybindings
        idomenu          ;; FixMe: set keybindings
        jump-dls         ;; FixMe: set keybindings - should override tag/idomenu?
                         ;; FixMe: should provide jump-dls
        eproject         ;; FixMe: configure
        expand-region    ;; FixMe: keybindings
        ace-jump-mode    ;; FixMe: keybindings
        breadcrumb       ;; FixMe: Configure, keybindings
                         ;; FixMe: isearch should auto-place
        pager
        pager-default-keybindings

        ;; Emacs editing improvements
        hungry-delete
        recursive-narrow
        kill-ring-ido ;; FixMe: keybindings
        ireplace      ;; FixMe: Train to use
        smart-operator ;;FixMe: Code clearnup, have '+SPACE' not add another space
        autopair
        duplicate-line ;; FixMe: kybindings

        ;; Dired enhancements
        dired-isearch
        wdired
        dired-sort

        ;; Shell-mode enhancements
        gdb-shell
        mv-shell

        ;; Eldoc improvements
        c-eldoc    ;; FixMe: Not enabled by default

        ;; Miscellanious major modes
        haml-mode
        markdown-mode
        nxml-mode
        js2-mode
        ssh-config-mode
        graphviz-dot-mode
        go-mode

        ;; ;; General utility functions
        buffer-move ;; FixMe: keybindings
        dedicated    ;; FixMe: Configure/remember
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
        flip-tables ;; FixMe: Also have insert in buffer
        generate-autoloads
        google-contacts

        ;; Sceme programming enhancements
        scheme-complete  ;; FixMe: hook into autocomplete

        ;; Communication
        jabber ;; FixMe: disable auto paren insertion
        jabber-chatx
        erc
        erc-highlight-nicknames

        ;; Python-mode enhancements
        pyflakes
        pytest
        python-pep8
        python-pylint

        ;; M-x grep enhancements
        wgrep ;; FixMe: Keybindings

        ;; General programming utilities
        c-toggle-dot-pointer ;; FixMe: Keybindings
        guess-offset
        org-table-comment
        auto-indent-mode
        guess-style

        ;; ;; Elisp programming enhancements
        elisp-slime-nav ;; Add to jump-dls?

        ;; ;; Elisp programming libraries
        thingatpt+
        dirtree
        hook-utils

        ;; Org-mode
        org

        ;; Version control enhacements
        git-emacs
        github-browse-file
        gitconfig-mode
        gitignore-mode
        git-commit-mode

        ;; HTML in emacs...
        w3

        ;; Windows enhancements
        w32-browser

        ;; M-x compile enhancements
        compile-dwim

        ;; Documentation
        javadoc-help

        ;; Emacs usage information
        keyfreq ;; FixMe: Schedule a periodic revioew
        keywiz ;; FixMe: Schedule to use

        Save-visited-files ;; FixMe: Save-visited-files interfering with save-visited-files
        ))

;;; Download and require all packages
(el-get `sync my:el-get-packages)

(mapcar (lambda (p) (if (not (member p '(jump-dls
                                  graphviz-dot-mode
                                  generate-autoloads
                                  duplicate-line
                                  git-commit-mode
                                  sicp
                                  Save-visited-files
                                  marmalade)))
                   (require p))) my:el-get-packages)
(require 'save-visited-files)

(global-rainbow-delimiters-mode)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(define-globalized-minor-mode global-highlight-80+-mode
  highlight-80+-mode
  (lambda ()
    (highlight-80+-mode t)))
(global-highlight-80+-mode t)


(define-globalized-minor-mode global-hungry-delete-mode
  hungry-delete-mode
  (lambda ()
    (hungry-delete-mode t)))
(global-hungry-delete-mode)

(global-pretty-mode)
(zenburn)

(defvaralias 'highlight-80+-columns 'fill-column)

(setq highlight-symbol-idle-delay 0)
(highlight-symbol-mode 1)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode 1)))

(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
(auto-indent-global-mode)

(dolist (hook  '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; To guess variables when a major mode is loaded, add `guess-style-guess-all'
;; to that mode's hook like this:
(add-hook 'c-mode-common-hook 'guess-style-guess-all)
(autopair-global-mode)

(setq jabber-account-list
      '(("flat0103@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))
        ("nflath@optumsoft.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
