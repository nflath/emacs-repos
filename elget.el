;;; Configuration for el-get
;;; Installs all the packages I rely on

;;; Ensure that all ELPA repositories are available
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Note: Also need to hack package.rcp to have this if I reinstall.

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
        oauth2


        ;; Emacs UI improvements
        color-theme
        zenburn
        mic-paren
        rainbow-delimiters
        highlight-parentheses
        icomplete+
        scratch-persist
        occur-default-current-word
        frame-fns
        frame-cmds

        ;; Emacs navigation improvements
        ; winpoint ;;FixMe: This is breaking startup
        winpoint-ignore-dired
        smooth-scrolling
        pc-keys
        idomenu
        jump-dls         ;; FixMe: set keybindings - should override tag/idomenu?
        helm
        eproject         ;; FixMe: configure
        expand-region    ;; FixMe: keybindings
        ace-jump-mode    ;; FixMe: keybindings
        breadcrumb       ;; FixMe: Configure, keybindings ;; FixMe: isearch should auto-place

        ; pager ;; FixMe: Breaking installation
        pager-default-keybindings

        ;; Emacs editing improvements
        hungry-delete
        recursive-narrow
        kill-ring-ido  ;; FixMe: keybindings
        ireplace       ;; FixMe: Train to use
        duplicate-line ;; FixMe: kybindings
        smart-whitespace-comment-fixup

        ;; Dired enhancements
        dired-isearch
        wdired
        dired-sort

        ;; Shell-mode enhancements
        comint-better-defaults
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
        generate-autoloads
        google-contacts

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
        wgrep

        ;; General programming utilities
        c-toggle-dot-pointer ;; FixMe: Keybindings
        java-file-create
        guess-offset
        org-table-comment
        auto-indent-mode
        guess-style


        ;; Elisp programming enhancements
        elisp-slime-nav ;; Add to jump-dls?

        ;; Elisp programming libraries
        thingatpt+
        dirtree
        hook-utils

        ;; Org-mode
        org

        ;; Version control enhacements
        git-emacs
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
        keywiz
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
(require 'winpoint)
(require 'pager)
(require 'save-visited-files)

(define-globalized-minor-mode global-highlight-80+-mode
  highlight-80+-mode
  (lambda ()
    (highlight-80+-mode t)))

(global-rainbow-delimiters-mode)
(global-highlight-80+-mode t)
(global-hungry-delete-mode)
(zenburn)

(defvaralias 'highlight-80+-columns 'fill-column)

(setq highlight-symbol-idle-delay 0)
(highlight-symbol-mode 1)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode 1)))

(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
(auto-indent-global-mode)

(dolist (hook  '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; To guess variables when a major mode is loaded, add `guess-style-guess-all'
;; to that mode's hook like this:
(add-hook 'c-mode-common-hook 'guess-style-guess-all)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-bounce-indent-p t)
(setq js2-highlight-level 3)
(setq markdown-enable-math t)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(setq htmlize-html-major-mode 'html-mode)
(setq nxml-slash-auto-complete-flag t)

(setq wgrep-enable-key "q")
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)
(paren-activate)
(mv-shell-mode)

(javadoc-set-predefined-urls '("http://download.oracle.com/javase/1.5.0/docs/api/"))
(jdh-process-predefined-urls *jdh-predefined-urls*)
(condition-case nil
    (jdh-refresh-url "http://download.oracle.com/javase/1.5.0/docs/api/")
  (error nil))



; FixMe: Add this stuff to idomenu

(defun imenu-old (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  ;; Convert a string to an alist element.
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (when index-item
    (push-mark nil t)
    (let* ((is-special-item (listp (cdr index-item)))
           (function
            (if is-special-item
                (nth 2 index-item) imenu-default-goto-function))
           (position (if is-special-item
                         (cadr index-item) (cdr index-item)))
           (rest (if is-special-item (cddr index-item))))
      (apply function (car index-item) position rest))
    (run-hooks 'imenu-after-jump-hook)))

;;;###autoload
(defun idomenu ()
  "Switch to a buffer-local tag from Imenu via Ido."
  (interactive)
  ;; ido initialization
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
  (add-hook 'choose-completion-string-functions 'ido-choose-completion-string)
  (add-hook 'kill-emacs-hook 'ido-kill-emacs-hook)
  ;; set up ido completion list
  (let ((index-alist (cdr (imenu--make-index-alist))))
    (if (equal index-alist '(nil))
        (message "No imenu tags in buffer")
      (imenu-old (idomenu--read (idomenu--trim-alist index-alist) nil t)))))

(defun imenu (&rest args)
  (interactive)
  (idomenu))
