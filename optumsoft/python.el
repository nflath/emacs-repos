;; Customizations for python for OptumSoft

(defun optumsoft-indent-python-mode-hook ()
  (setq py-indent-offset 3)
  (optumsoft-python-add-copyright))

(defun optumsoft-python-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/env python\n")
          (insert (format "# Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "# OptumSoft, Inc. Confidential and Proprietary.\n\n"))
        (set-buffer-modified-p mod))))

(add-hook 'python-mode-hook 'optumsoft-indent-python-mode-hook)
