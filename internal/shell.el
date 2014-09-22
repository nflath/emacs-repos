;;; Customizations for shell-mode

;; Shell-mode doesn't colorize your output by default; instead, it inserts the
;; control codes into your buffer.  This turns on colorization.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq-default dirtrack-list '("[a-zA-Z]*@\\([^$ \t\r\n]*\\)\\$ " 1))

;; FixMe: This should be in .bashrc
(when (eq window-system 'w32)
  (setq-default dirtrack-list '("\\(c:[^ ]*\\)>" 1)))

(defun shell-mode-start-dirtrack ()
  "Changes to the correct prompt"
  (shell-dirtrack-mode -1)
  (comint-send-string (current-buffer) "export PS1=\"nflath@\\w$ \"\n")
  (dirtrack-mode 1))

(add-hook 'shell-mode-hook 'shell-mode-start-dirtrack)

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
