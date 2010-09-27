;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.

(setq browse-url-chrome-arguments nil)
(setq browse-url-chrome-program "google-chrome")

;;;###autoload
(defun browse-url-chrome (url &optional new-window)
  "Ask the Chrome WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chrome-arguments' are also passed to
Chrome.

When called interactively, if variable
`browse-url-new-window-flag' is non-nil, load the document in a
new Chrome window, otherwise use a random existing one.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

If `browse-url-chrome-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument
NEW-WINDOW is used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is
ignored.  Chrome for Windows does not support the \"-remote\"
command line parameter.  Therefore, the
`browse-url-new-window-flag' and `browse-url-chrome-new-window-is-tab'
are ignored as well.  Chrome on Windows will always open the requested
URL in a new window."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process
          (apply 'start-process
                 (concat "chrome " url) nil
                 browse-url-chrome-program
                 (append
                  browse-url-chrome-arguments
                  (if (or (featurep 'dos-w32)
                          (string-match "win32" system-configuration))
                      (list url)
                    (list "-remote"
                          url))))))

    (set-process-sentinel process
                          `(lambda (process change)
                             (browse-url-chrome-sentinel process ,url)))))

(defun browse-url-chrome-sentinel (process url)
  "Handle a change to the process communicating with Chrome."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
        ;; Chrome is not running - start it
        (message "Starting Chrome...")
        (apply 'start-process (concat "chrome " url) nil
               browse-url-chrome-program
               (append browse-url-chrome-startup-arguments (list url))))))

(setq browse-url-browser-function 'browse-url-chrome)

(defun browse-current-file ()
  (interactive)
  (browse-url (concat "file://" buffer-file-name)))

(require 'google-weather)
(require 'org-google-weather)

(remove-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'before-save-hook
          (lambda ()
            (when (and (locate-dominating-file buffer-file-name ".git")
                       (eq major-mode 'emacs-lisp-mode))
              (shell-command "export SSH_AUTH_SOCK=0; git commit -am \"Auto commit from emacs\"; git push origin master"))
            t))

(define-key c-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)
(define-key c++-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)

(defun num-code-lines ()
  (interactive)
  (beginning-of-line)
  (let ((count 0))
    (while (not (eobp))
      (back-to-indentation)
      (if (or (eq (face-at-point) 'font-lock-comment-face)
              (eq (face-at-point) 'font-lock-comment-delimiter-face)
              (eq (face-at-point) 'font-lock-doc-face))
          (setq count (1+ count)))
      (forward-line))
    (print count)))

