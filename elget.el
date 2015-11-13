;;; elget.el - Installs all the packages I rely on

;;; Bootstrap use-package and Ensure that all ELPA repositories are available
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)

;;; List of packages to make sure are installed
(defvar my-packages
  `(
    ;; Packages used for initialization purposes
    load-dir
    oauth2
    use-package

    ;; Emacs UI improvements
    diminish
    color-theme
    zenburn
    miniedit
    magit
    wgrep
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
    winner

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
    recursive-narrow
    smart-whitespace-comment-fixup
    abbrev
    move-line
    company
    company-c-headers
    company-go
    flyspell
    org
    org-capture
    org-mobile

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
    shell
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
    python-mode
    pyflakes
    pytest
    python-pep8
    python-pylint

    ;; Elisp programming libraries
    lisp-mode
    thingatpt+
    dirtree
    hook-utils
    cl

    ;; Org-mode
    org
    org-habit
    appt

    ;; Version control enhancements
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

;;; Download and require all packages

(setq failed-installs ())
(defun try-package-install (sym)
  (condition-case nil
      (progn
        (if (not (package-installed-p sym))
             (package-install sym)))
    (error (setq failed-installs (append failed-installs (list sym))))))

(setq failed-requires ())
(defun try-require (sym)
  (condition-case nil
      (require sym)
    (error (setq failed-requires (append failed-requires (list sym))))))
(setq failed-requires ())

(mapc 'try-package-install my-packages)
(mapc 'try-require my-packages)

;;; Use-package definitions; all of these packages should also be in the above
;;; list, but these also have configuration I want to only happen if they are
;;; installed.

(use-package mic-paren
  :config
  (paren-activate))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package load-dir
  :init
  (setq load-dirs (concat emacs-repos-dir "internal"))
  :config (load-dirs))

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

(use-package javadoc-lookup
  :init
  (define-key java-mode-map (kbd "C-x j") 'javadoc-lookup))

(use-package smartscan
  :config
  (add-hook 'prog-mode-hook 'smartscan-mode)
  (add-hook 'org-mode-hook 'smartscan-mode))

(use-package occur-x
  :config
  (add-hook 'occur-mode-hook 'turn-on-occur-x-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

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

(use-package python
  :config
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)


  (defun my-insert-self ()
    "Insert self. at the beginning of the current word."
    (interactive)
    (save-excursion
      (search-backward-regexp
       "[
 \t,(-]\\|^")
      (if (not (looking-at "^"))
          (forward-char))
      (insert "self.")))
  (define-key python-mode-map (kbd "C-;") 'my-insert-self)

  (setq python-indent-guess-indent-offset nil))

(use-package loccur
  :bind (([(control meta o)] . loccur)
         ([(control shift o)] . loccur-previous-match)))

(use-package macro-math
  :bind (("\C-x~" . macro-math-eval-and-round-region)
         ("\C-x=" . macro-math-eval-region)))

(use-package cc-mode
  :mode (("\\.tin$" . c++-mode)
         ("\\.tac$" . c++-mode)
         ("\\.h$" . c++-mode))

  :config
  (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c-mode-base-map [remap newline-and-indent] 'continue-string-if-necessary)

  (setq c-standard-font-lock-fontify-region-function (default-value 'font-lock-fontify-region-function))  ;; FixMe: why is this necessary?

  (defun continue-string-if-necessary ()
    "If in a string, closes it and starts it again on the next
  line; otherwise just calls newling-and-indent."
    (interactive)
    (if (or
         (eq (get-text-property (point) 'face) font-lock-string-face)
         (eq (c-in-literal) 'string))
        (progn
          (insert "\"")
          (newline-and-indent)
          (insert "+ \""))
      (newline-and-indent)))

  (defun set-compile-command ()
    "Sets the compile command to a sensible default(compile the
current file) if no makefile is found."
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (if buffer-file-name
               (let ((file (file-name-nondirectory buffer-file-name)))
                 (format "%s -c -o %s.o %s %s %s"
                         (or (getenv "CC") "g++")
                         (file-name-sans-extension file)
                         (or (getenv "CPPFLAGS") "-DDEBUG=9")
                         (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                         file))))))

  (add-hook 'c-mode-hook 'set-compile-command)
  (add-hook 'c++-mode-hook 'set-compile-command)

  (defun h-file-create ()
    "Create a new h file.  Insert a infdef/define/endif block"
    (if (or (equal (substring (buffer-file-name (current-buffer)) -2 ) ".h")
            (equal (substring (buffer-file-name (current-buffer)) -4 ) ".hpp"))
        (let* ((buffer-name (buffer-name (current-buffer)))
               (class-name (substring buffer-name 0 (string-match "\\." buffer-name))))
          (when (equal "" (buffer-string))
            (insert "#ifndef "
                    (upcase class-name)
                    "_H\n#define "
                    (upcase class-name)

                    "_H\n\n\n\n#endif")
            (search-backward "define")
            (next-line 2)))))

  (defun c-include (include)
    "Includes a header file in the current file."
    (interactive (list (read-string (concat "Include file <" (current-word) ">: "))))
    (when (string-equal include "") (setq include (current-word)))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "#include" nil t)
          (progn (beginning-of-line)
                 (insert (concat "#include " include "\n")))
        (search-forward "\n\n")
        (insert "#include " include "\n\n"))))

  (defun c-forward-declare (class)
    "Insert a forward declaration for class"
    (interactive (list (read-string (concat "Forward declare n<" (current-word) ">: "))))
    (when (string-equal class "") (setq class (current-word)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "class [a-zA-Z]*;" nil t)
          (insert (concat "\nclass " (capitalize class) ";"))
        (search-forward "\n\n")
        (insert "class " (capitalize class) ";\n\n"))))


  (add-hook 'c++-mode-hook 'h-file-create)
  (add-hook 'c-mode-hook 'h-file-create)

  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions 'advanced)
  (setq dired-isearch-filenames t)

  :after dired)

(use-package workgroups2
  :diminish workgroups-mode
  :init
  (setq wg-session-load-on-start t)
  :config
  (workgroups-mode 1))

(use-package save-visited-files
  :config
  (add-hook 'after-init-hook (save-visited-files-mode t)))

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t))

(use-package dirtrack-buffer-name-track-mode
  :config
  (dirtrack-buffer-name-track-mode))

(use-package htmlize
  :config
  (setq htmlize-html-major-mode 'html-mode))

(use-package highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package auto-indent-mode
  :diminish auto-indent-mode
  :config
  (auto-indent-global-mode))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package wgrep
  :config
  (setq wgrep-enable-key "q"))

(use-package jump-dls
  :config
  (setq jump-build-index t)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'jump-symbol-at-point))

(use-package magit
  :bind (( "C-x v s" . magit-status)))

(use-package ssh-config-mode
  :mode (".ssh/config\\'"
         "sshd?_config\\'")
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package markdown-mode
  :config
  (setq markdown-enable-math t))

(use-package winner
  :config
  (winner-mode 1))

(use-package frame-cmds
  :config
  (maximize-frame))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "~/bin"))

(use-package shell
  :config
  (defun shell-dirtrack-mode-off ()
    (interactive)
    (shell-dirtrack-mode -1))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'shell-dirtrack-mode-off))

(use-package dirtrack
  :config
  (if (eq window-system 'w32)
      (setq-default dirtrack-list '("\\(c:[^ ]*\\)>" 1))
    (setq-default dirtrack-list '("[a-zA-Z]*@\\([^$ \t\r\n]*\\)\\$" 1)))

  (defun dirtrack-mode-on ()
    (interactive)
    (dirtrack-mode 1))
  (add-hook 'shell-mode-hook 'dirtrack-mode-on)

  (defvar max-prompt-len 40 "Maximum length of your prompt string.")

  ;; Dirtrack forces the prompt to contain the full working directory, but this
  ;; sometimes causes the prompt to be too long.  The following code will cause
  ;; your prompt to only contain the last 40 characters of the current directory.
  (defun dirtrack-buffer-name-track-shorten-prompt (input)
    "Shortens any prompts displayed to max-prompt-len chars."
    (let* ((prompt (progn (if (string-match (car dirtrack-list) input)
                              (match-string 0 input))))
           (len (if prompt (length prompt) 0)))
      (if (and (> len max-prompt-len)
               (<= (- len max-prompt-len 1) (length default-directory)))
          (replace-regexp-in-string
           (replace-regexp-in-string "\\$" "\\$"  prompt nil t)
           (concat "nflath@/"
                   (substring default-directory (- len max-prompt-len 1)) "$ " )
           input nil t)
        input)))
  (add-hook 'comint-preoutput-filter-functions 'dirtrack-buffer-name-track-shorten-prompt)

  :after shell)

(use-package windmove
  :config
  (windmove-default-keybindings 'M))

(use-package expand-region
  :bind (("C-=" . r/expand-region)))

(use-package hook-utils
  :config
  (defvar elisp-modes '(emacs-lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook
                        inferior-emacs-lisp-mode-hook)
    "List of modes that are used for programming in emacs-lisp.")

  (hook-utils-add-hook-to-all elisp-modes 'turn-on-eldoc-mode))

(use-package zenburn
  :config
  (zenburn))
(use-package miniedit
  :config
  (miniedit-install))
(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'whitespace-mode)
  (diminish 'subword-mode))
