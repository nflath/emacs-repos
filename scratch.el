 ;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.

(url-retrieve "https://graph.facebook.com/me/events?access_token=2227470867|2.WATor_z92fHz8QjpTnr_nA__.3600.1295888400-122615117|n-bErIWnK_X639Jt5p1qs8-6v2I"
#'callback)

(format-time-string "%Y-%m-%d" (date-to-day "2011-01-24"))


(current-time)

(defun only-outline ()
  (interactive)
  (occur "^\\*")
  (switch-to-buffer "*Occur*")
  (rename-buffer (buffer-name) + ".org")
  )
(setq foo (shell-command-to-string "~/a.out&"))

(remove-if '(lambda (x)
              (and nil (or (string-equal location x)
                           (not (file-exists-p x))
                           (eq nil x))))
           (mapcar 'buffer-file-name (buffer-list)))

(mapcar '(lambda (x) (insert x "\n"))
        (remove-if '(lambda (x)
                      (if x 
                      (or (string-equal "foo" x)
                          (not (file-exists-p x))
                          (eq nil x))))
                   (mapcar 'buffer-file-name (buffer-list)))
))


(mapcar '(lambda (x) (if x (insert x "\n"))
        (remove-if '(lambda (x)
                      (if x (or (string-equal "foo" x)
                                (not (file-exists-p x))
                                (eq nil x))))
                   (mapcar 'buffer-file-name (buffer-list)))))

(setq a (remove-if '(lambda (x)
                      (if x (or (string-equal "foo" x)
                                (not (file-exists-p x))
                                (eq nil x))))
                   (mapcar 'buffer-file-name (buffer-list))))