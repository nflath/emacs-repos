(require 'cc-mode)
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
                  "_H\n\nclass "
                  (capitalize class-name)
                  " {\npublic:\n\n\nprivate:\n\n\n};"
                  "\n\n#endif")
          (search-backward "public:\n")
          (next-line)))))

(add-hook 'c++-mode-hook 'h-file-create)

(defun set-compile-command ()
  "Sets the compile command to a sensible default if no makefile is found."
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -c -o %s.o %s %s %s"
                   (or (getenv "CC") "g++")
                   (file-name-sans-extension file)
                   (or (getenv "CPPFLAGS") "-DDEBUG=9")
                   (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                   file)))))
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
               (insert (concat "#include \"" (downcase include) ".hpp\"\n")))
      (search-forward "\n\n")
      (insert "#include \"" (downcase include) ".hpp\"\n\n"))))

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

(defun c-fwdinclude (arg)
  "Either forward declare or include a class/file, depending on
  whether you are in a header or implementation file."
  (interactive (list (read-string (concat "Import <" (current-word) ">: "))))
  (when (string-equal arg "") (setq arg (current-word)))
  (let ((buffer-name (buffer-name (current-buffer))))
    (if (string-match-any (list "\.h" "\.hpp") buffer-name )
        (c-forward-declare arg)
      (c-include arg))))

(define-key c-mode-base-map (kbd "C-c c f") 'c-fwdinclude)

(define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
(add-hook 'c-mode-common-hook (lambda () (c-toggle-syntactic-indentation 1)))
(add-hook 'c-mode-common-hook (lambda () (c-toggle-electric-state 1)))
(add-hook 'c-mode-common-hook (lambda () (c-toggle-hungry-state 1)))