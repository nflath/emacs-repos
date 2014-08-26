;;; Customizations for shell-mode

;; Shell-mode doesn't colorize your output by default; instead, it inserts the
;; control codes into your buffer.  This turns on colorization.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; FixMe: Export to own package
;;Dirtrack is a mode that keeps your shell buffers' directories in sync with
;;the shell.  It does this by looking for the current directory in your prompt.
;;To use it, you need to set your prompt to match the regex, or vice versa.
(setq-default dirtrack-list '("[a-zA-Z]*@\\([^$ \t\r\n]*\\)\\$ " 1))
(defun shell-mode-start-dirtrack ()
  "Changes to the correct prompt"
  (shell-dirtrack-mode -1)
  (comint-send-string (current-buffer) "export PS1=\"nflath@\\w$ \"\n")
  (dirtrack-mode 1))
(add-hook 'shell-mode-hook 'shell-mode-start-dirtrack)

;;It is quite convenient to have your shell buffers contain the name of the
;;directory they are visiting.  This requires changing the buffer name whenever
;;dirtrack changes the working directory.
(defun dirtrack-directory-change-hook-fn ()
  "Changes the name of the buffer to reflect the directory."
  (let* ((base-buffer-name (concat "shell-" (expand-file-name default-directory) "-shell"))
         (i 1)
         (full-buffer-name (concat base-buffer-name "<1>")))
    (if (get-buffer base-buffer-name)
        (progn
          (while (get-buffer full-buffer-name)
            (setq i (1+ i))
            (setq full-buffer-name (concat base-buffer-name "<" (number-to-string i) ">")))
          (rename-buffer full-buffer-name))
      (rename-buffer base-buffer-name))))
(add-hook 'dirtrack-directory-change-hook 'dirtrack-directory-change-hook-fn)

;;Dirtrack forces the prompt to contain the full working directory, but this
;;sometimes causes the prompt to be too long.  The following code will cause
;;your prompt to only contain the last 40 characters of the current directory.
(defvar max-prompt-len 40 "Maximum length of your prompt string")
(defun shorten-prompt (input)
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
(add-hook 'comint-preoutput-filter-functions 'shorten-prompt)

(when (eq window-system 'w32)
  (setq-default dirtrack-list '("\\(c:[^ ]*\\)>" 1)))
