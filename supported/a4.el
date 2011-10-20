
;;; add replace-regexp-in-string for Xemacs.  The behavior is
;;; identical in the cases that we're using it.
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp newtext string)
    (replace-in-string string regexp newtext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Perforce-emacs integration
;;; 

(defun a4-run (argv &optional output-filter)
  (interactive 
   (list 
    (split-string (read-from-minibuffer "Run like: " (cons (concat "a4  " (buffer-file-name)) 4))
                  " ")))
  (with-temp-buffer
    (let* ((result (apply 'call-process (append (list (car argv) nil t nil) (cdr argv))))
           (re "\n$")
           (str (buffer-substring (point-min) (point-max)))
           (ofilt (or output-filter (lambda (x) x)))
           (output (apply ofilt (list (replace-regexp-in-string re "" str)))))
      (if (eq result 0)
          (progn
            (message "%s" output)
            t)
        (message "a4 error: %s" output)
        nil))))

(defun a4-nuke-p4-crud (msg) 
  (replace-regexp-in-string "^\\+ p4 .*\n" "" msg))

(defun a4-edit (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (if (a4-run (list "a4" "edit" (file-name-nondirectory file)) 'a4-nuke-p4-crud)
        (setq buffer-read-only nil))))

(defun a4-revert (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (if (a4-run (list "a4" "revert" (file-name-nondirectory file)) 'a4-nuke-p4-crud)
        (revert-buffer))))

(defun a4-add (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (a4-run (list "a4" "add" (file-name-nondirectory file)) 'a4-nuke-p4-crud)))

(defun a4-diff (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (shell-command (concat "a4 diff " (file-name-nondirectory file)))))


(defun a4-submit ()
  (interactive)
  (a4-run (list "a4" "submit")) 'a4-nuke-p4-crud)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Build system integration
;;;

(defvar arastra-root (expand-file-name "~/ar/"))
(defvar arastra-virgin-environment (copy-alist process-environment))
(defvar arastra-package-list "")
(defvar arastra-package-list-history '(""))
(defvar arastra-gdb-command "sugdb")
(defvar arastra-kgdb-command "sukgdb")
(defvar arastra-make-target "all")
(defvar arastra-make-target-history '("all"))
(defvar arastra-last-compile-command "a4 make sanity")
(defvar arastra-last-package "")

(defun arastra-prompt-for-root ()
  (setq arastra-root (expand-file-name 
                      (read-file-name "a4 client root: " arastra-root nil t "")))
  (or (string-match "/$" arastra-root)
      (setq arastra-root (concat arastra-root "/"))))

(defun arastra-make (&optional arg)
  (interactive "P")
  (if arg 
      (progn
        (arastra-prompt-for-root)
        (setq arastra-package-list (read-string "List of packages to build: " 
                                                (car arastra-package-list-history)
                                                '(arastra-package-list-history . 1)))
        (setq arastra-make-target (read-string "Final make target: "
                                               (car arastra-make-target-history)
                                               '(arastra-make-target-history . 1)))
        (let* ((pkglist (split-string arastra-package-list "[ ,]+"))
               (last (car (last pkglist)))
               (cmd (apply 'concat 
                           (cons "true" 
                                 (mapcar (lambda (x) (format " && a4 make -p %s %s" x
                                                             (if (equal x last) 
                                                                 arastra-make-target
                                                               "all install")))
                                         pkglist)))))
          (setq arastra-last-package last)
          (arastra-compile cmd)))
    (arastra-compile arastra-last-compile-command)))

(defun arastra-compile (cmd)
  (setq arastra-last-compile-command cmd)
  (let ((default-directory arastra-root))
    (compile cmd))
  (let* ((b (get-buffer "*compilation*"))
         (w (and b (get-buffer-window b))))
    (if w
        (progn
          (select-window w)
          (set-buffer b)
          (goto-char (point-max))
          (insert ">")
          (goto-char (point-max))))))

(defun arastra-make-package (pkg)
  (interactive "sPackage to make: ")
  (arastra-compile (format "a4 make -p %s" pkg)))

(defun arastra-make-sanity (&optional arg)
  (interactive "P")
  (if arg
      (arastra-prompt-for-root))
  (arastra-compile "a4 make sanity"))

(defvar arastra-gdb-script nil)
(defvar arastra-gdb-script-history nil)

(defvar arastra-gdb-script-args nil)
(defvar arastra-gdb-script-args-history nil)

(defun arastra-gdb (&optional arg)
  (interactive "P")
  (let* ((srcdir (format "%ssrc/%s/" arastra-root arastra-last-package))
         (blddir (format "%sbld/%s/" arastra-root arastra-last-package))
         (default-directory arastra-root)
         (testdir (concat srcdir "test/"))
         (gdb-command-name arastra-gdb-command))
    (when arg
      (setq arastra-gdb-script (read-file-name "Run gdb on test script: " 
                                               testdir nil t nil
                                               'arastra-gdb-script-history))
      (if arastra-gdb-script
          (setq arastra-gdb-script-args 
                (read-string "Script arguments: " 
                             (car arastra-gdb-script-args-history)
                             '(arastra-gdb-script-args-history . 1) nil))))

    (if (file-is-python-script arastra-gdb-script)
        (progn
          (gdb (search-path-for-file (parse-colon-path (getenv "PATH"))
                                     "python"))
          (let ((proc (get-buffer-process (current-buffer))))
            (process-send-string proc (format "cd %s\n" blddir))
            (setq default-directory blddir)
            (if (file-exists-p ".gdbinit")
                (process-send-string proc "source .gdbinit\n"))
            (if arastra-gdb-script
                (process-send-string proc (format "set args %s %s\n" 
                                                  arastra-gdb-script
                                                  arastra-gdb-script-args)))))
      (gdb arastra-gdb-script))
    (let ((proc (get-buffer-process (current-buffer))))
      (process-send-string proc "set env CATCH_THROW 1\n")
      ;; The above interacts with tacc/libfwk/Exception.cpp to enable exception
      ;; debugging.
      (if (file-exists-p (concat srcdir ".gdbinit"))
          (process-send-string proc (concat "source " srcdir ".gdbinit\n"))))))



(defvar arastra-kgdb-script nil)
(defvar arastra-kgdb-script-history nil)

(defvar arastra-kgdb-module nil)
(defvar arastra-kgdb-module-history nil)

(defun arastra-kgdb (&optional arg)
  (interactive "P")
  (let ((gdb-command-name arastra-kgdb-command))
    (when arg
      (arastra-prompt-for-root)
      (setq arastra-kgdb-script (read-string "Virtual Machine Address: "
                                             (car arastra-kgdb-script-history)
                                             '(arastra-kgdb-script-history . 1) nil))
      (if arastra-kgdb-script
          (setq arastra-kgdb-module 
                (read-file-name "Module File (optional): " 
                                arastra-root "" t (car arastra-kgdb-module-history)
                                arastra-kgdb-module-history))))

    (gdb (format "%s%s" arastra-root arastra-kgdb-script) arastra-kgdb-module )))



(defun file-is-python-script (filename)
  (string-match "\\.py$" filename))

(defun arastra-find-include ()
  (interactive)
  (let* ((include
          (or
           (save-excursion
             (beginning-of-line)
             (if (looking-at ".*include *[\"<]\\([^\">]+\\)")
                 (buffer-substring (match-beginning 1) (match-end 1))))
           (save-excursion
             (skip-chars-backward "^\n <\"")
             (if (looking-at "[^\n <\"]+")
                 (buffer-substring (match-beginning 0) (match-end 0))))
           (error "Can't find filename near point")))
         (path (list 
                default-directory
                (concat arastra-root "aroot/usr/include/")
                "/usr/include/"
                "/usr/include/c++/3.4.2/"
                ;; It would be nice to be smarter about this next few.
                ;; For example, we could run "gcc -v" and look for "include".
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/g++-v3/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/g++-v3/i686-pc-linux-gnu/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/g++-v3/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/g++-v3/i686-pc-linux-gnu/"
                ))
         (file (or (search-path-for-file path include)
                   (error "Could not locate %s" include))))
    (find-file file)))

(defun search-path-for-file (path file)
  (catch 'found
    (while path
      (let ((full (concat (car path) file)))
        (if (file-exists-p full)
            (throw 'found full)))
      (setq path (cdr path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Default keybindings
;;;

(provide 'a4)
