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

(setq python-remove-cwd-from-path nil)
(setq python-indent-guess-indent-offset nil)
