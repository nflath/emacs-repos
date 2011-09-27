(if (= 0 (shell-command "zsh"))
    (setq explicit-shell-file-name "/bin/zsh"))

;;Functions that make it much easier to open a shell.  Also, create a shell in the default emacs directory.
(defun shell-current-directory ()
  "Opens a shell in the current directory"
  (interactive)
  (let ((new-buffer-name (concat "shell-" (expand-file-name default-directory) "-shell" )))
    (if (get-buffer new-buffer-name) (switch-to-buffer-other-window new-buffer-name)
      (shell new-buffer-name))))
(eval-after-load 'init-finished '(shell-current-directory))
(global-set-key (kbd "C-c s") 'shell-current-directory)

(defun ido-shell ()
  "Prompts for a directory and then opens a shell in it."
  (interactive)
  (let ((dirname (expand-file-name (ido-read-directory-name "Shell in directory: "))))
    (shell dirname)
    (comint-send-string (current-buffer) (concat "cd " dirname "\n"))))
(global-set-key (kbd "C-x s" ) 'ido-shell)

;;Shell-mode doesn't colorize your output by default; instead, it inserts the control codes into your buffer.  This
;;turns on colorization.
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;Dirtrack is a mode that keeps your shell buffers' directories in sync with the shell.  It does this by looking for the
;;current directory in your prompt.  To use it, you need to set your prompt to match the regex, or vice versa.
(setq-default dirtrack-list '("[a-zA-Z]*@\\([^$ \t\r\n]*\\)\\$ " 1))
(defun shell-mode-start-dirtrack ()
  "Changes to the correct prompt"
  (shell-dirtrack-mode -1)
  (if (= 0 (shell-command "zsh"))
      (comint-send-string (current-buffer) "export PS1=\"nflath@\%d$ \"\n")
    (comint-send-string (current-buffer) "export PS1=\"nflath@\\w$ \"\n"))
  (dirtrack-mode 1))
(add-hook 'shell-mode-hook 'shell-mode-start-dirtrack)

;;It is quite convenient to have your shell buffers contain the name of the directory they are visiting.  This requires
;;changing the buffer name whenever dirtrack changes the working directory.
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

;;Dirtrack forces the prompt to contain the full working directory, but this sometimes causes the prompt to be too long.
;;The following code will cause your prompt to only contain the last 40 characters of the current directory.
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

(require 'dirtrack)

;;When I'm on windows, I want to use *eshell* instead of the built-in shell.  However, a few of my functions call shell directly, so I alias *shell* to *eshell* if I'm using windows
(when (eq window-system 'w32)
  (require 'eshell)
  (defalias 'shell 'eshell))
