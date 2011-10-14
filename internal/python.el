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

(when (= 0 (shell-command "python --version"))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-process-echoes nil)))
  ;(run-python)
  (setq comint-process-echoes nil))

;; Customizing
(when (= 0 (shell-command "pyflakes"))
  (setq flymake-enable-pyflakes t)
  (setq flymake-enable-pylint nil)
  (setq flymake-enable-pep8 nil)

  ;; Python or python mode?
  (eval-after-load 'python
    '(progn

       ;;==================================================
       ;; Virtualenv Commands
       ;;==================================================


       ;;==================================================
       ;; Flymake for python configuration
       ;;===================================================

       ;; TODO: There is some duplication, that can be removed using macros
       ;; TODO: Implement flymake-remove-checker

       ;; Instructions to add a new checker based on command:
       ;;
       ;; 1) Write an init function, the flymake-command-setup performs some
       ;;    checks and at the end of the option list the filename to process:
       ;;
       ;;   (defun flymake-newchecker-init ()
       ;;      (flymake-command-setup "command" (list "option1" "option2")))
       ;;
       ;; 2) Use the flymake-add-checker function
       ;;
       ;;    (flymake-add-checker flymake-newchecker-init)

       (require 'tramp)
       ;; Utilities that increase legibility and reduce code duplication
       (defun current-file-remotep ()
         "Tell if the file is remote"
         (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

       (defun flymake-create-copy-file ()
         "Create a copy local file"
         (let* ((temp-file (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace)))
           (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))

       (defun flymake-command-setup (command &optional options)
         "Setup the command to be used with flymake, the command
will be called in this way: COMMAND OPTIONS FILE The FILE varible
is passed after the options."
         ;; Make sure it's not a remote buffer or flymake would not work
         (when (not (current-file-remotep))
           (list command
                 (append options (list (flymake-create-copy-file))))))

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

(defun optumsoft-indent-python-mode-hook ()
  (setq py-indent-offset 3)
  (optumsoft-python-add-copyright))

(defun optumsoft-python-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/env python\n")
          (insert (format "# Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "# OptumSoft, Inc. Confidential and Proprietary.\n\n"))
        (set-buffer-modified-p mod))))

(add-hook 'python-mode-hook 'optumsoft-indent-python-mode-hook)

(setq python-remove-cwd-from-path nil)