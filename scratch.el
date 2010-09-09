;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.

(setq str "abc")

(defun org-increment-string (str)
  (let ((chars (string-to-list str))
        (n (1- (length str)))
        result y)

    (while (and (>= n 0))
      (let ((char (nth n chars)))
        (setq result (append (list (org-increment-char char)) result))
        (setq y (1- n))
        (setq n (if (not (or (= char ?Z) (= char ?z)))
                    -1
                  (1- n)))))
    (if (> y 0 )
        (while (>= y 0)
          (setq result (append (list (nth n chars)) result))
          (setq y (1- y)))
      (setq result (append (list ?a) result)))
    (concat result)))

(defun org-increment-char (char)
  (cond
   ((= char ?Z) ?A)
   ((= char ?z) ?a)
   (t (1+ char))))

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
(require 'google-maps)

(require 'org-google-weather)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'before-save-hook
          (lambda ()
            (when (and (locate-dominating-file buffer-file-name ".git")
                       (eq major-mode 'emacs-lisp-mode))
              (shell-command "export SSH_AUTH_SOCK=0; git commit -am \"Auto commit from emacs\"; git push"))
            t))


(define-key c-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)
(define-key c++-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)
