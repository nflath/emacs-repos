;; Use Gmail to send email when ~/.authinfo exists
(require 'starttls)
(setq starttls-use-gnutls t)
(setq smtpmail-stream-type 'ssl)

(when (file-exists-p "~/.authinfo")
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 465 nil nil))
        smtpmail-auth-credentials (expand-file-name "~/.authinfo")
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-debug-info t))


(defun insert-email ()
  "Uses email-alist to read a name and insert the correct email addresses at point."
  (interactive)
  (insert (cadr (assoc (ido-completing-read
                        "Name: "
                        (mapcar #'car email-alist))
                       email-alist))))
