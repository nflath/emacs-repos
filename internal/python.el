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

(defun python-self-insert-command ()
  "Appends __ to the current word if it started with __."
  (interactive)
  (save-excursion
    (let ((cur (point)))
      (search-backward-regexp "[\t .(-]\\|^" 0 t)
      (if (not (looking-at "^"))
          (forward-char 1))
      (if (looking-at "\\s\__")
          (setq my-temp-var t)
        (setq my-temp-var nil))))
  (save-excursion
    (when (> (point) 2)
      (backward-char 2)
      (if (looking-at "__")
          (setq my-temp-var nil))))
  (if my-temp-var (insert "__"))
  (self-insert-command 1))

(define-key	python-mode-map (kbd ".")	'python-self-insert-command)
(define-key	python-mode-map (kbd "SPC")	'python-self-insert-command)
(define-key	python-mode-map (kbd "(")	'python-self-insert-command)

(when (= 0 (shell-command "python --version"))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-process-echoes nil)))
  ;(run-python)
  (setq comint-process-echoes nil))
