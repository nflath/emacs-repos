(defun a4-edit ()
  (interactive)
  (shell-command (concat "ssh obs11 'a4 edit "
                           (replace-regexp-in-string "/home/nflath/obs11"
                                           "/local/nflath"
                                           (buffer-file-name))
                           "'")))