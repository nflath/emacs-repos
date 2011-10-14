
;; The default cc-mode style needs to be modified slightly to match Arastra
;; conventions:

(c-add-style "Arastra" '("user" 
                         (c-basic-offset . 3)
                         (c-offsets-alist
                          (innamespace . 0)
                          (access-label . -2)
                          (arglist-intro . 6)
                          (case-label . 1)
                          (statement-case-intro . 2)
                          (statement-case-open . 2)
                          (member-init-intro . 6)
                          )) nil)

(defun arastra-add-copyright (comment)
  (insert (format "%s Copyright (c) %s Arista Networks, Inc.  All rights reserved.\n"
                  comment (nth 5 (decode-time (current-time)))))
  (insert (format "%s Arista Networks, Inc. Confidential and Proprietary.\n\n" comment)))
  
(defun arastra-c-mode-common-hook ()
  (c-set-style "Arastra")
  ;; Automatically insert copyright notice.
  (if (< (buffer-size) 10) 
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (arastra-add-copyright "//")
          (if (string-match "\\([a-zA-Z0-9_]+\\)[/\\\\]\\([a-zA-Z0-9_]+\\)\\.h$" (buffer-file-name))
              (let* ((dir-name (substring (buffer-file-name) (match-beginning 1) (match-end 1)))
                     (file-name (substring (buffer-file-name) (match-beginning 2) (match-end 2)))
                     (symbol (upcase (format "%s_%s_H" dir-name file-name))))
                (insert (format "#ifndef %s\n#define %s\n\n\n#endif // %s\n" symbol symbol symbol)))))
        (set-buffer-modified-p mod))))

(add-hook 'c-mode-common-hook 'arastra-c-mode-common-hook)

(custom-set-variables
 '(indent-tabs-mode nil))


(defun arastra-perl-mode-hook ()
  (setq perl-indent-level 3)
  (setq perl-continued-statement-offset 3)
  (arastra-perl-add-copyright))

(defun arastra-perl-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/perl -w\n#\n")
          (arastra-add-copyright "#")
          (insert "use strict;\n\n"))
        (set-buffer-modified-p mod))))

(defun arastra-cperl-mode-hook ()
  (setq cperl-indent-level 3)
  (setq cperl-continued-statement-offset 3)
  (arastra-perl-add-copyright))

(add-hook 'perl-mode-hook 'arastra-perl-mode-hook)
(add-hook 'cperl-mode-hook 'arastra-cperl-mode-hook)

                  
(defun arastra-indent-python-mode-hook ()
  (if (featurep 'xemacs)
    (setq py-indent-offset 3)
  (setq python-indent 3))
  (arastra-python-add-copyright))

(defun arastra-python-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/env python\n")
          (arastra-add-copyright "#"))
        (set-buffer-modified-p mod))))

(add-hook 'python-mode-hook 'arastra-indent-python-mode-hook)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "linux")
  (setq indent-tabs-mode t))
