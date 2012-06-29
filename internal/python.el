(require 'python)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

(defun my-insert-self ()
  "Insert self. at the beginning of the current word."
  (interactive)
  (save-excursion
    (search-backward-regexp
     "[ \t,(-]\\|^")
    (if (not (looking-at "^"))
        (forward-char))
    (insert "self.")))

(define-key	python-mode-map	(kbd "C-;")	'my-insert-self)

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-process-echoes nil)))

;; Customizing
(when (= 0 (shell-command "pyflakes"))
  (setq flymake-enable-pyflakes t)
  (setq flymake-enable-pylint nil)
  (setq flymake-enable-pep8 nil))

(setq python-remove-cwd-from-path nil)
(add-hook 'python-mode-hook (lambda () (setq imenu-create-index-function 'python-imenu-create-index)))
