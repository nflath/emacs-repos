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

(auto-indent-global-mode)
(paren-activate)

(setq browse-kill-ring-highlight-current-entry t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (if (not (or (f-ancestor-of? emacs-repos-dir default-directory)
                         (file-equal-p emacs-repos-dir default-directory)))
                (flycheck-mode))))
