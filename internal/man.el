;; I usually try to do M-x man-mail, or whatever command I'm trying, before
;; going to M-x max and entering the command.  The following actually installs
;; all of the commands you can man as interactive command.

(let ((man-page-list nil))
  (mapc (lambda (dir)
          (message dir)
          (mapcar
           (lambda (file) (add-to-list 'man-page-list
                                       (before-first "\\." (after-last "/" file))))
           (directory-files-recursive (concat man-dir dir))))
        (remove-if-not (lambda (elt) (string-match "man" elt))
                       (directory-files man-dir)))
  (mapcar (lambda (elt)
            (eval
             `(defun ,(intern (concat "man-" elt)) ()
                (interactive)
                (man ,elt))))
          man-page-list))
