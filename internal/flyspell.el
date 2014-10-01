(when (file-exists-p "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

;; Configures flyspell, a spell checker
(when (= 0 (shell-command ispell-program-name))
  (add-hook 'text-mode-hook 'turn-on-flyspell-mode)
  (add-hook 'org-mode-hook 'turn-on-flyspell-mode)
  (add-hook 'latex-mode-hook 'turn-on-flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell-mode)
  (add-hook 'plain-tex-mode-hook 'turn-on-flyspell-mode))

(defadvice ispell-command-loop (before ispell-reverse-miss-list activate)
  "reverse the first argument to ispell-command-loop"
  (ad-set-arg 0 (reverse (ad-get-arg 0))))

;; Flyspell often doesn't have technical words or abbreviations, so this will
;; add a command that allows you to define these more easily.
(defun flyspell-add-word (word)
  "Adds word to personal dictionary"
  (interactive (list (read-string (concat "Add word to personal dictionary <" (current-word) ">: "))))
  (when (string-equal word "") (setq word (current-word)))
  (ispell-send-string (concat "*" word "\n"))
  (ispell-send-string "#\n"))


(add-hook 'prog-mode-hook 'flyspell-prog-mode)
