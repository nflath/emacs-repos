(defun a4-edit ()
  (interactive)
  (message (shell-command-to-string (concat "ssh obs11 'cd /local/nflath/baiyu; a4 chroot; a4 edit "
                           (replace-regexp-in-string "/home/nflath/obs11"
                                           "/local/nflath"
                                           (buffer-file-name))
                           "'"))))

(defun a4-revert ()
  (interactive)
  (message (shell-command-to-string (concat "ssh obs11 'cd /local/nflath/baiyu; a4 chroot; a4 revert "
                                            (replace-regexp-in-string "/home/nflath/obs11"
                                                                      "/local/nflath"
                                                                      (buffer-file-name))
                                            "'"))))