;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.

(format-time-string "%Y-%m-%d" (date-to-day "2011-01-24"))

(current-time)

(defun only-outline ()
  (interactive)
  (occur "^\\*")
  (switch-to-buffer "*Occur*")
  (rename-buffer (buffer-name) + ".org"))

(defun shell-and-cd (&rest args)
  (interactive)
  (let ((dir default-directory))
    (switch-to-buffer "*shell*")
    (comint-send-string (current-buffer) (concat "cd " dir))))

(add-to-list 'load-path "c:/Users/nflath/emacs-repos/supported/calfw")
(require 'calfw-org)

(setq foo (shell-command-to-string "~/a.out&"))

(add-to-list 'load-path "~/Downloads/calfw")


(setq jabber-account-list
      '(("flat0103@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
defun
