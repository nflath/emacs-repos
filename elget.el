;;; Installs all the packages I rely on

;;; Ensure that all ELPA repositories are available
;;; Code
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;;; Download and require all packages

(setq failed-installs ())
(defun try-package-install (sym)
  (condition-case nil
      (progn
        (if (not (package-installed-p sym))
             (package-install sym)))
    (error (setq failed-installs (append failed-installs (list sym))))))

(mapc 'try-package-install my-packages)
(mapc 'try-require my-packages)

;(require 'elisp-slime-nav)
;(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
(add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
(auto-indent-global-mode)
(setq htmlize-html-major-mode 'html-mode)
(paren-activate)



(setq markdown-enable-math t)
(setq wgrep-enable-key "q")
(sml/setup)
(setq browse-kill-ring-highlight-current-entry t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (if (not (or (f-ancestor-of? emacs-repos-dir default-directory)
                         (file-equal-p emacs-repos-dir default-directory)))
                (flycheck-mode))))



         (add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
;(require 'anzu)
;(global-anzu-mode +1)

(add-hook 'prog-mode-hook 'smartscan-mode)
(add-hook 'org-mode-hook 'smartscan-mode)
(setq jump-build-index t)

(setq highlight-symbol-idle-delay 0)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require 'dirtrack-buffer-name-track-mode)
(dirtrack-buffer-name-track-mode)
