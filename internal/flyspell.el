(add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'latex-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'plain-tex-mode-hook '(lambda () (flyspell-mode 1)))

(defadvice ispell-command-loop (before ispell-reverse-miss-list activate)
  "reverse the first argument to ispell-command-loop"
  (ad-set-arg 0 (reverse (ad-get-arg 0))))

;;Flyspell often doesn't have technical words or abbreviations, so this will add a command that allows you to define
;;these more easily.
(defun flyspell-add-word (word)
  "Adds word to personal dictionary"
  (interactive (list (read-string (concat "Add word to personal dictionary <" (current-word) ">: "))))
  (when (string-equal word "") (setq word (current-word)))
  (ispell-send-string (concat "*" word "\n"))
  (ispell-send-string "#\n"))
(global-set-key (kbd "C-c f") 'flyspell-add-word)









