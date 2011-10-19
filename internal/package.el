;;; Controls the emacs package manager - sets it to look at more repositories
;;; than just the official GNU repository and installs all packages I want.

(require 'package)

;; Use ELPA and marmalade as well as official GNU Elpa
(setq package-archives (append
                        package-archives
                        '(("marmalade" . "http://marmalade-repo.org/packages/")
                          ("ELPA" . "http://tromey.com/elpa/"))))
(package-initialize)
(package-refresh-contents)

;; List of packages to install
(setq packages-to-install '(auctex
                            auto-complete
                            auto-complete-etags
                            auto-indent-mode
                            c-eldoc
                            cache
                            calfw-gcal
                            clojure-mode
                            clojure-test-mode
                            cmake-mode
                            color-theme
                            command-frequency
                            ctags
                            cygwin-mount
                            dired-isearch
                            doc-mode
                            durendal
                            ecb
                            eldoc-eval
                            elisp-cache
                            elisp-slime-nav
                            emms
                            facebook
                            find-file-in-project
                            fuzzy-match
                            gccsense
                            gdb-shell
                            graphviz-dot-mode
                            guess-offset
                            haml-mode
                            haskell-mode
                            highlight-parentheses
                            htmlize
                            hungry-delete
                            icomplete+
                            ioccur
                            ipython
                            javadoc-help
                            js2-mode
                            load-dir
                            loccur
                            log4j-mode
                            macro-math
                            magit
                            magit-simple-keys
                            magithub
                            markdown-mode
                            marmalade
                            member-function
                            mic-paren
                            mv-shell
                            nxml-mode
                            org
                            org-email
                            org-magit
                            p4
                            pager
                            pastebin
                            pep8
                            powershell
                            pyflakes
                            pylint
                            pytest
                            python
                            python-mode
                            python-pep8
                            python-pylint
                            rainbow-delimiters
                            rainbow-mode
                            recursive-narrow
                            save-visited-files
                            scheme-complete
                            scratch
                            setup-cygwin
                            slime
                            slime-clj
                            slime-fuzzy
                            slime-repl
                            smart-operator
                            smart-tab
                            swank-cdt
                            switch-window
                            twitter
                            w3
                            w32-browser
                            wgrep
                            winpoint
                            yaml-mode
                            yasnippet))

;; Install packages if they have not been installed
(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p)))