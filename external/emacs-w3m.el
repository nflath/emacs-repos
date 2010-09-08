(when (= 0 (shell-command "w3m"))
  (require 'w3m)
    (require 'w3m-session)
    (require 'w3m-cookie)
    (setq w3m-session-load-last-sessions t)
    (setq w3m-session-file ".emacs.d/.w3m-session")
    (setq w3m-use-cookies t)

    (defun w3m-browse-current-buffer ()
      "Look at the current buffer as rendered by w3m."
      (interactive)
      (let ((filename (concat (make-temp-file "w3m-") ".html")))
        (unwind-protect
            (progn
              (write-region (point-min) (point-max) filename)
              (w3m-find-file filename))
          (delete-file filename))))

    ;;Better w3m buffer names
    (add-hook 'w3m-display-hook (lambda (url) (rename-buffer url t)))
    ;;Better w3m formatting
    (add-hook 'w3m-display-hook (lambda (url)
                                  (let ((buffer-read-only nil))
                                    (delete-trailing-whitespace))))

    ;;Only use w3m if we aren't on a system that can run firefox
    (unless window-system
      (setq browse-url-browser-function 'w3m-browse-url)
      (setq browse-url-new-window-flag t)))
