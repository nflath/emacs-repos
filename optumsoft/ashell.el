;;; ashell builds on and modifies shell-mode with a few arastra-local improvements
;;;   meta-z <char> finds or creates a shell buffer suffixed with char
;;;   control-z goes to the most recent shell buffer
;;;   control-l does a slightly nice recentering function
;;;   control-] sends an immediate control-] to the underlying process
;;;   Output filters that strip control-m and determine cwd from shell prompt

(require 'shell)

(defun ashell (arg)
  (interactive "c")
  (if (or (< arg 32) (> arg 126))
      (error "Please use printable character for shell name"))
  (let* ((buffer (get-buffer (format "*shell*<%c>" arg))))
    (if buffer
	(switch-to-buffer buffer)
      (progn
	(shell)
        (setq shell-dirtrackp nil)
        (make-local-variable 'comint-input-sender)
        ;(setq comint-input-sender (function shellx-simple-send))
	(rename-buffer (format "*shell*<%c>" arg))))))

(defun ashell-last-shell-buffer ()
  (interactive)
  (let* ((bl (buffer-list)) 
	 found)
    (while bl 
      (if (string-match "\*shell\*" (buffer-name (car bl)))
	  (progn (switch-to-buffer (car bl)) 
		 (setq bl nil)
		 (setq found t))
	(setq bl (cdr bl))))
    (if (not found)
      (ashell ?1))))

(defun ashell-send-string (string)
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) string))

(defun ashell-send-control-right-bracket ()
  (interactive)
  (ashell-send-string ""))

(defun ashell-send-control-d ()
  (interactive)
  (ashell-send-string ""))

(defun ashell-recenter (arg)
  (interactive "P")
  (if (not (eobp))
      (recenter arg)
    (recenter (- (window-height (selected-window)) 2))))

;; It is such a pain trying to support K different versions of emacs,
;; each more complicated than the last.  In xemacs 21.5, there are
;; these random "device-specific" keymaps that override global-map,
;; and make ^Z do weird things.  Search and destroy.
(mapcar (lambda (sym) (if (boundp sym) 
                          (define-key (eval sym) '[(control ?z)] nil)))
        '(global-window-system-map global-tty-map))

(remove-hook 'comint-output-filter-functions
             'comint-watch-for-password-prompt)

;; Determine current directory from shell prompt
;; (adapted from http://list-archive.xemacs.org/xemacs-beta/200507/msg00215.html)
(add-hook 'comint-output-filter-functions 'ashell-get-cwd-from-prompt)
(defvar ashell-cwd nil)
(defvar ashell-prompt-regexp
  (format "^\\(.+\\) @%s[#%%>] " (car (split-string (system-name) "\\."))))
(defun ashell-get-cwd-from-prompt (&optional string)
  (interactive)
  (save-excursion
    (goto-char (if (interactive-p) comint-last-input-end comint-last-output-start))
    (while (re-search-forward ashell-prompt-regexp nil t)
      (let ((new-cwd (match-string 1)))
        (when (not (equal ashell-cwd new-cwd))
          (setq ashell-cwd new-cwd)
          (setq new-cwd (file-name-as-directory new-cwd))
          (if find-file-visit-truename
              (setq new-cwd (file-truename new-cwd)))
          (setq new-cwd (abbreviate-file-name (expand-file-name new-cwd)))
          (message "cwd is %s" new-cwd)
          (setq default-directory new-cwd))))))

;; Make Ctrl-M (CR) do the right thing
(add-hook 'comint-output-filter-functions 'ashell-do-ctrl-m)
(defun ashell-do-ctrl-m (&optional string)
  (interactive)
  (save-excursion
    (goto-char (if (= comint-last-output-start 0) 0 (- comint-last-output-start 1)))
    (while (search-forward "\r" (point-max) t)
      (cond ((and (eobp) (= (char-before) ?\r)) t)
            ((looking-at "\n") (delete-backward-char 1))
            (t (delete-backward-char (- (point) (point-at-bol))))))))

;; Make M-p and M-n work as god intended, which is that if you type a prefix
;; of a prior command and press M-p, you get the prior command that matches,
;; rather than just throwing out what you typed and giving you the most
;; recent prior command.  For example:
;;
;;   % TRACE=pexpect/0-4,CliTest/0-4 make check TESTS=test/ReloadTest.py
;;   ... lots of output
;;   % echo hello
;;   % echo lots of other stuff
;;   % make all
;;   % rm /tmp/junkfile
;;   % TR<M-p>
;;
;; When you press M-p in the last line above, instead of recalling the
;; most recent command (the rm command), comint will recall the TRACE=...
;; command.  This is very handy.  -kduda 2009-02-12

(add-hook 'comint-mode-hook 
          (lambda nil
            (define-key (current-local-map) "\ep" 
              'comint-previous-matching-input-from-input)
            (define-key (current-local-map) "\en" 
              'comint-next-matching-input-from-input)))

;; control-A does not work in some cases because of the bizarre way
;; that comint implements control-A by default.  By default, it does
;; this:
;;
;;         (constrain-to-field (line-beginning-position) (line-end-position))
;;
;; which seems to guarantee that if there seem to be multiple prompts, 
;; comint does the wrong thing and brings you to the end of the last prompt,
;; instead of the end of the first prompt.  This next hook makes control-A
;; use a prompt regexp instead of using the "field" stuff.
;;
;; I took a wild swing at fixing this, but it broke other cases (such
;; as Acons or python-interactive or gdb or pdb) so I need to
;; investigate more deeply.  -kduda 2009-05-18

