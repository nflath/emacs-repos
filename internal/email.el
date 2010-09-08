;;Use Gmail to send email when ~/.authinfo exists
(when (file-exists-p "~/.authinfo")
  (require 'smtpmail)
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        (expand-file-name "~/.authinfo")
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

;;Send emails easily
(defvar email-alist
  '(( "Max Burstyn"              "dmbursty@gmail.com")
    ( "Yubin Kim"                "shdwfeather@gmail.com")
    ( "Derek Thor Thurn"         "thor@thurn.ca")
    ( "Gobi Raveendran"          "superworm85@gmail.com")
    ( "Design Project fydp uydp" "fydp_ngym@googlegroups.com")
    ( "SE1 SE463"                "dmbursty@gmail.com, shdwfeather@gmail.com, thor@thurn.ca")
    ( "Networks"                 "curtis.steeves@gmail.com, tyler.szabo@gmail.com, thor@thurn.ca")
    ( "Linode"                   "dmbursty@gmail.com, shdwfeather@gmail.com, tyler.szabo@gmail.com, thor@thurn.ca")
    ( "Alan Mackenzie"           "acm@muc.de")
    )
  "alist of names to email adresses")

(defun insert-email ()
  "Uses email-alist to read a name and insert the correct email addresses at point."
  (interactive)
  (insert (cadr (assoc (ido-completing-read
                        "Name: "
                        (mapcar #'car email-alist))
                       email-alist))))

(global-set-key (kbd "C-c m") 'insert-email)