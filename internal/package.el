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
                            c-eldoc 
                            cache 
                            clojure-mode 
                            clojure-test-mode 
                            color-theme 
                            dired-isearch 
                            doc-mode 
                            durendal 
                            eldoc-eval 
                            elisp-slime-nav 
                            fuzzy-match 
                            gdb-shell 
                            guess-offset 
                            haml-mode 
                            haskell-mode
                            highlight-parentheses 
                            htmlize 
                            hungry-delete 
                            icomplete+ 
                            javadoc-help 
                            js2-mode
                            load-dir 
                            loccur 
                            macro-math 
                            magit 
                            magithub 
                            markdown-mode 
                            marmalade 
                            mic-paren 
                            mv-shell 
                            nxml-mode
                            org 
                            org-magit
                            p4 
                            pager 
                            pastebin 
                            pyflakes 
                            pytest 
                            python-pep8 
                            python-pylint 
                            rainbow-mode 
                            recursive-narrow 
                            save-visited-files 
                            scheme-complete 
                            slime 
                            slime-fuzzy 
                            slime-repl 
                            smart-operator 
                            swank-cdt 
                            w3 
                            w32-browser 
                            wgrep 
                            winpoint 
                            yaml-mode))

;; Install packages if they have not been installed
(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p))
  (condition-case nil (require p) (error t)))
