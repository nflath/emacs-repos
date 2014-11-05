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

(defun prev-window (&optional arg)
  "Go to the previous window displayed."
  (interactive)
  (let ((arg (if arg arg 1)))
    (other-window (- arg))))

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
          (last nil)
          (old-column (current-column))
          (old-next-line-add-newlines))
      (setq next-line-add-newlines nil)

      (while (and (not (looking-at "^[ \t]*$"))
                  (not (eq (point) last)))
        (setq last (point))
        (move-to-column old-column t)
        (if (and (looking-at "-?[0123456789]+\\.?[0123456789]*")
                 (eq (current-column) old-column))
            (setq retn (+ retn (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
        (forward-line)
        (beginning-of-line))
      (when (eq (point) last) (end-of-line) (insert "\n"))
      (end-of-line)
      (let ((begn (point)))
        (forward-line)
        (when (= (point) begn)
          (message "Hi")
          (insert "\n")
          (forward-line)))
      (end-of-line)
      (let ((begn (point)))
        (forward-line)
        (when (= (point) begn)
          (message "Hi2")
          (insert "\n")
          (forward-line)))
      (move-end-of-line 0)
      (if (< (current-column) old-column)
          (insert (make-string (- old-column (current-column)) 32)))
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

(defun try-require (pkg)
  (condition-case  nil
      (require pkg)
    (error nil)))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun eval-function-in ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (call-interactively 'eval-last-sexp)))

(defun string-match-any (regexp-list string &optional start)
  "Returns whether the given string, starting at position start,
matches any regexp in the list."
  (if regexp-list
      (let ((result (string-match (car regexp-list) string start)))
        (if result
            result
          (string-match-any (cdr regexp-list) string start)))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defmacro time (block)
  `(let (start end)
    (setq start (current-time))
    ,block
    (setq end (current-time))
    (print (time-subtract end start))))

(macroexpand '(time (print "hi")))

(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))
