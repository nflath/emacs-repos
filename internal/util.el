;;; Collection of utility functions for interactive use 

(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill. This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables. This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (let ((kill-read-only-ok t))
    (ad-deactivate 'kill-line)
    (toggle-read-only 1)
    (kill-line arg)
    (toggle-read-only -1)
    (ad-activate 'kill-line)))

(defun copy-whole-line (&optional arg)
  "Copies the current line"
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (copy-line arg)))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun help-anything ()
  "If function given tries to `describe-function' if variable
uses 'describe-variable', otherwise uses `manual-entry' to display
manpage of a `current-word'."
  (interactive)
  (let ((var (variable-at-point))
        (fn (function-called-at-point)))
    (cond
     ((symbolp var) (describe-variable var))
     (fn (describe-function fn))
     ((and (eq major-mode 'java-mode) (fboundp java-describe-class))
      (java-describe-class (current-word)))
     (t (man (current-word))))))

(defun google (query)
  "googles a query"
  (interactive "sQuery:")
  (browse-url (concat "http://www.google.com/search?q=" query)))

(defun wc nil
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun wc-region ()
  "Count words in region"
  (interactive)
  (shell-command-on-region (mark) (point) "wc -w"))

(defun iwb ()
  "Indents the entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun uwb ()
  "Untabifies the whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun twb ()
  "Tabifies the whole buffer"
  (interactive)
  (tabify (point-min) (point-max)))

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))

(defun format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun increment-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (format-bin answer field-width)))))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)     t))))

(defun prev-window (&optional arg)
  "Go to the previous window displayed."
  (interactive)
  (other-window (- arg)))

(defun turn-on-auto-revert-mode ()
  "Always turns on auto revert mode, instead of toggling."
  (interactive)
  (auto-revert-mode 1))

(defun turn-on-visual-line-mode ()
  "Always turns on visual line mode, instead of toggling."
  (interactive)
  (visual-line-mode 1))

(defun turn-on-flyspell-mode ()
  "Always turns on visual line mode, instead of toggling."
  (interactive)
  (flyspell-mode 1))

(defun grep-with-defaults (word)
  "Grep the whole directory for something defaults to term at cursor position"
  (interactive (list (read-string (concat "Grep For: <" (current-word) ">: "))))
  (when (string-equal word "") (setq word (current-word)))
  (grep (concat "egrep -s -i -n -r \"" word "\" * " )))

(defun insert-current-time (&optional arg)
  "Insert current time string at current position"
  (interactive "p")
  (insert (current-time-string)))

(defun nflath-cycle-bol (&optional arg)
  "If at the first non-whitespace character of a line, go to the
beginning of the current line.  Otherwise, goto the first non-whitespace
character of the current line."
  (interactive)
  (cond
   ((bolp) (back-to-indentation))
   ((save-excursion
      (let ((pt (point)))
        (back-to-indentation)
        (eq pt (point)))) (beginning-of-line))
   (t (back-to-indentation))))

(defun browse-current-file ()
  "Opens the current file in a web browser."
  (interactive)
  (browse-url (concat "file://" buffer-file-name)))

(defun wc-buffer-elisp ()
  "Counts the words in the buffer using elisp"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((i 0))
      (while (not (eobp))
        (forward-word)
        (setq i (1+ i)))
      (print i))))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun sum-column()
  "Sums a column of numbers starting at point"
  (interactive)
  (save-excursion
    (if (and (not (= (current-column) 0))
             (re-search-backward "[ \t]" 0 t ))
        (forward-char))
    (let ((retn 0)
          (old-column (current-column))
          (old-next-line-add-newlines))
      (setq next-line-add-newlines nil)
      (while (not
              (looking-at "^[ \t]*$"))
        (move-to-column old-column t)
        (if (and (looking-at "-?[0123456789]+")
                 (eq (current-column) old-column))
            (setq retn (+ retn (string-to-number (current-word)))))
        (next-line)
        (beginning-of-line))
      (next-line)
      (next-line)
      (move-end-of-line 0)
      (insert (make-string (- old-column (current-column)) 32))
      (insert (number-to-string retn))
      (setq next-line-add-newlines old-next-line-add-newlines)
      retn)))

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (read-shell-command "Open current file with: ")
                    " "
                    buffer-file-name))))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun add (amt) 
  "Increment the number (in base 10 representation) at point."
  (interactive "nAmount to add: ")
  (if (looking-at "[-0-9.]+")
      (progn
        (let ((num (car (read-from-string (buffer-substring (match-beginning 0) (match-end 0))))))
          (delete-region (match-beginning 0) (match-end 0))
          (setq num (+ num amt))
          (insert (format (if (< (abs (- num (round num))) 0.001) "%.0f" "%.2f") num))))))

(defun ++ ()
  "Increment the number (in base 10 representation) at point."
  (interactive)
  (add 1))

(defun mul (amt) 
  "Multiply the number (in base 10 representation) at point."
  (interactive "nAmount to multiply by: ")
  (if (looking-at "[-0-9.]+")
      (progn
        (let ((num (car (read-from-string (buffer-substring (match-beginning 0) (match-end 0))))))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (format "%f" (* num amt)))))))

(defun shell-current-directory ()
  "Opens a shell in the current directory"
  (interactive)
  (let ((new-buffer-name (concat "shell-" (expand-file-name default-directory) "-shell" )))
    (if (get-buffer new-buffer-name) (switch-to-buffer-other-window new-buffer-name)
      (shell new-buffer-name))))

(defun ido-shell ()
  "Prompts for a directory and then opens a shell in it."
  (interactive)
  (let ((dirname (expand-file-name (ido-read-directory-name "Shell in directory: "))))
    (shell dirname)
    (comint-send-string (current-buffer) (concat "cd " dirname "\n"))))

(defun continue-string-if-necessary ()
  "If in a string, closes it and starts it again on the next
  line; otherwise just calls newling-and-indent."
  (interactive)
  (if (or
       (eq (get-text-property (point) 'face) font-lock-string-face)
       (eq (c-in-literal) 'string))
      (progn
        (insert "\"")
        (newline-and-indent)
        (insert "+ \""))
    (newline-and-indent)))

(require 'imenu)
(require 'cl)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist)
      (if (not symbol-names)
          (ido-goto-symbol)
        (let* ((symbol-at-point (symbol-name (symbol-at-point)))
               (selected-symbol (ido-completing-read
                                 "Symbol? "
                                 (if (member symbol-at-point symbol-names)
                                     (cons symbol-at-point (remove-if
                                                            (lambda (x) (string-equal x symbol-at-point))
                                                            symbol-names))
                                   symbol-names)))
               (position (cdr (assoc selected-symbol name-and-pos))))
          (if (overlayp  position)
              (goto-char (overlay-start position))
            (goto-char position)))))))

(defun sudo-edit (&optional arg)
  "Find a file and open it as root."
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  "Edit the current file as root."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char pos)))

(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen) on an X display."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun w32-maximize-frame ()
  "Maximize the current frame on a Windows machine."
  (interactive)
  (w32-send-sys-command 61488))

(defun maximize-frame ()
  "Maximizes the Emacs frame."
  (interactive)
  (if window-system
      (if (fboundp 'x-send-client-message)
          (x11-maximize-frame)
        (w32-maximize-frame))))
