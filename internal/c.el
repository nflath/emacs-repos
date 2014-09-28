;; Customizations for cc-mode

(defun h-file-create ()
  "Create a new h file.  Insert a infdef/define/endif block"
  (if (or (equal (substring (buffer-file-name (current-buffer)) -2 ) ".h")
          (equal (substring (buffer-file-name (current-buffer)) -4 ) ".hpp"))
      (let* ((buffer-name (buffer-name (current-buffer)))
             (class-name (substring buffer-name 0 (string-match "\\." buffer-name))))
        (when (equal "" (buffer-string))
          (insert "#ifndef "
                  (upcase class-name)
                  "_H\n#define "
                  (upcase class-name)

                  "_H\n\n\n\n#endif")
          (search-backward "define")
          (next-line 2)))))
(add-hook 'c++-mode-hook 'h-file-create)
(add-hook 'c-mode-hook 'h-file-create)

(defun set-compile-command ()
  "Sets the compile command to a sensible default if no makefile is found."
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (if buffer-file-name
             (let ((file (file-name-nondirectory buffer-file-name)))
               (format "%s -c -o %s.o %s %s %s"
                       (or (getenv "CC") "g++")
                       (file-name-sans-extension file)
                       (or (getenv "CPPFLAGS") "-DDEBUG=9")
                       (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                       file))))))

(add-hook 'c-mode-hook 'set-compile-command)
(add-hook 'c++-mode-hook 'set-compile-command)

(defun c-include (include)
  "Includes a header file in the current file."
  (interactive (list (read-string (concat "Include file <" (current-word) ">: "))))
  (when (string-equal include "") (setq include (current-word)))
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "#include" nil t)
        (progn (beginning-of-line)
               (insert (concat "#include " include "\n")))
      (search-forward "\n\n")
      (insert "#include " include "\n\n"))))

(defun c-forward-declare (class)
  "Insert a forward declaration for class"
  (interactive (list (read-string (concat "Forward declare n<" (current-word) ">: "))))
  (when (string-equal class "") (setq class (current-word)))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "class [a-zA-Z]*;" nil t)
        (insert (concat "\nclass " (capitalize class) ";"))
      (search-forward "\n\n")
      (insert "class " (capitalize class) ";\n\n"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (which-function-mode t)))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
