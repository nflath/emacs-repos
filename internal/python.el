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
  (setq flymake-enable-pep8 nil)

  (eval-after-load 'python
    '(progn
       ;; Init functions!
       (defun flymake-pyflakes-init ()
         (flymake-command-setup "pyflakes"))

       (defun flymake-pep8-init ()
         (flymake-command-setup "pep8"))

       (defun flymake-pylint-init ()
         (flymake-command-setup "python" (list (concat emacs-repos-dir "/pylint-mod.py"))))

       (defun flymake-disable-python-checkers ()
         "Disable all python checkers"
         (dolist (flymake-checker-init '(flymake-pyflakes-init flymake-pep8-init flymake-pylint-init))
           (remove '("\\.py\\'" flymake-checker-init) 'flymake-allowed-file-name-masks)))

       (defun flymake-add-checker (command)
         "Add the checker specified by the COMMAND list"
         (add-to-list 'flymake-allowed-file-name-masks
                      (list "\\.py\\'" command)))

       ;; Not on all modes, please
       (add-hook 'python-mode-hook 'flymake-find-file-hook)

       (when flymake-enable-pyflakes
         (flymake-add-checker 'flymake-pyflakes-init))

       (when flymake-enable-pylint
         (flymake-add-checker 'flymake-pylint-init))

       (when flymake-enable-pep8
         (flymake-add-checker 'flymake-pep8-init)))))

(setq python-remove-cwd-from-path nil)
