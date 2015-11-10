;; Use Gmail to send email when ~/.authinfo exists
(defun insert-email ()
  "Uses email-alist to read a name and insert the correct email addresses at point."
  (interactive)
  (insert (cadr (assoc (ido-completing-read
                        "Name: "
                        (mapcar #'car email-alist))
                       email-alist))))
