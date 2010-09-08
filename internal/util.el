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
(global-set-key (kbd "C-x y") 'copy-whole-line)

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

(defun my-help ()
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
(global-set-key [f1] 'my-help)

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

(defun my-increment-number-decimal (&optional arg)
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

(defun my-increment-number-hexadecimal (&optional arg)
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

(defun my-format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun my-increment-number-binary (&optional arg)
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
          (replace-match (my-format-bin answer field-width)))))))

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
(global-set-key (kbd "C-c O") 'swap-windows)

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
(global-set-key (kbd "C-c b m") 'move-buffer-file)

(defun prev-window (&optional arg)
  "Go to the previous window displayed."
  (interactive)
  (other-window (- arg)))
(global-set-key (kbd "C-x p") 'prev-window)

(defun turn-on-auto-revert-mode ()
  "Always turns on auto revert mode, instead of toggling."
  (interactive)
  (auto-revert-mode 1))

(defun turn-on-visual-line-mode ()
  "Always turns on visual line mode, instead of toggling."
  (interactive)
  (visual-line-mode 1))

(defun my-grep (word)
  "Grep the whole directory for something defaults to term at cursor position"
  (interactive (list (read-string (concat "Grep For: <" (current-word) ">: "))))
  (when (string-equal word "") (setq word (current-word)))
  (grep (concat "egrep -s -i -n -r \"" word "\" * " )))
(global-set-key (kbd "C-x g") 'my-grep)

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
(global-set-key (kbd "C-a") 'nflath-cycle-bol)