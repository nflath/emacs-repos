;; Allow use of alphabetical lists
(setq org-alphabetical-lists t)

;; Agenda customizations
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files (list org-directory))
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(defadvice org-cycle (after org-reactivate-always activate)
  (if (org-at-table-p)
      (org-table-recalculate t)))

;; Time logging
;(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist 'history)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))
(add-hook 'org-clock-in-prepare-hook 'my-org-mode-ask-effort)

;; org-remember - quickly jot down thoughts
(require 'org-capture)
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "TODO.org") "Tasks")
         "* TODO %?\n  %i\n")))


(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-default-notes-file (concat org-directory "TODO.org"))

(defun my-org-capture-dont-ask ()
  (interactive)
   (org-capture 1 "t"))


;; General org customizations
(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-completion-use-ido t)
(setq org-insert-heading-respect-content t)
(setq org-irc-link-to-logs)
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-speed-commands-user nil)
(setq org-startup-align-all-tables t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-use-speed-commands t)
(setq org-archive-location (concat org-directory "archive/%s_archive::"))
(setq org-hide-leading-stars t)

;; TODO customizations
(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time)
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)") ))

(setq org-todo-keyword-faces
      '(("CANCELED"  . (:foreground "blue" :weight bold :strike-through t))))

(defun org-current-section-number (&optional pos)
  "Returns the subsection number at pos"
  (save-excursion
    (if pos (goto-char pos))
    (let ((retn 1))
      (ignore-errors
        (while t
          (outline-backward-same-level 1)
          (incf retn)))
      retn)))

(defun org-full-sections (&optional pos)
  "Returns a list coresponding to the full section number at pos"
  (save-excursion
    (if pos (goto-char pos))
    (let* ((retn 1)
           (curnum (org-current-section-number))
           (retlist (list curnum)))
      (condition-case nil
          (while t
            (progn
              (outline-up-heading 1)
              (setq retlist (append (list (org-current-section-number)) retlist))))
        (error retlist))
      retlist)))

(defun org-full-sections-string (&optional pos)
  "Returns a string corresponding to the section at pos"
  (substring (reduce (lambda (x y) (concat x "." (number-to-string y)))
                     (org-full-sections)
                     :initial-value "") 1))

(defun line-matches (regexp)
  "Returns non-nil if the current line matches the given regexp, nil otherwise."
  (interactive "sRegex: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (re-search-forward regexp end t))))

(defun org-publish-agenda ()
  "Writes out the agenda and all agenda files as HTML."
  (interactive)
  (save-window-excursion
    (mapcar (lambda (file)
              (find-file file)
              (org-export-as-html 3)
              (kill-buffer))
            (org-agenda-files))
    (org-agenda 0 "a")
    (org-agenda-month-view)
    (let ((html-buffer (htmlize-buffer (get-buffer "*Org Agenda*")))
          (agenda-buffer (get-buffer "*Org Agenda*")))
      (switch-to-buffer html-buffer)
      (goto-char (point-min))
      (search-forward "<body>")
      (let ((line-start (line-number-at-pos)))
        (while (< (point) (point-max))
          (beginning-of-line)
          (cond
           ((line-matches "org-agenda-structure") (forward-line))
           ((line-matches "org-agenda-dat") (forward-line))
           ((line-matches "org-time-grid") (forward-line))
           ((line-matches " *\\([^:]+\\):")
            (let ((calendar (after-last " "(match-string 1))))
              (let ((agenda-line-no (1- (- (line-number-at-pos) line-start))))
                (switch-to-buffer agenda-buffer)
                (goto-line agenda-line-no)
                (let* ((marker (or (get-text-property (point) 'org-marker)
                                 (org-agenda-error)))
                       (buffer (marker-buffer marker))
                       (pos (marker-position marker)))
                  (switch-to-buffer buffer)
                  (goto-char pos)
                  (setq sec-string (concat "sec-" (org-full-sections-string)))
                  (switch-to-buffer html-buffer)))
              (insert (concat "<a href=\"" calendar ".html#" sec-string "\">"))
              (end-of-line)
              (insert "</a>")
              (forward-line)))
           (t (forward-line)))))
      (write-file (concat (car org-agenda-files) "agenda.html"))
      (let ((default-directory (car org-agenda-files)))
        (shell-command "mv *.html publish/" nil)
        (kill-buffer "agenda.html")))))

;; General hooks for org and agenda
(add-hook 'org-mode-hook (lambda () (auto-revert-mode t)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
(remove-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-agenda-mode-hook '(lambda () (cd (car org-agenda-files))))

;; Turn on appointment checking
(setq appt-time-msg-list nil)
(appt-activate 1)
(setq appt-display-format 'window)

(defadvice  org-agenda-redo (after org-agenda-redo-add-appts activate)
  "Pressing `r' on the agenda will also add appointments."
  (progn
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

(defun my-org-pending-remove-scheduled ()
  (when (string= (org-get-todo-state) "PENDING")
    (org-remove-timestamp-with-keyword org-scheduled-string)))
(add-hook 'org-after-todo-state-change-hook 'my-org-pending-remove-scheduled)

;; graph habits
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

;; Start with nothing expanded
(add-hook 'org-mode-hook 'org-hide-block-all)

;; Prettify entities
(setq org-entities-user
      '(("intersection" "\\cap" t "&cap;" "[intersection]" "[intersection]" "∩")
        ("union" "\\cup" t "&cup;" "[union]" "[union]" "∪")
        ("Z" "\\mathbb{Z}" t "&#8477" "[Set of Integers]" "[Set of Integers]" "ℤ")
        ("R" "\\mathbb{R}" t "&#8484" "[Set of Reals]" "[Set of Reals]" "ℝ")
        ("integers" "\\mathbb{Z}" t "&#8477" "[Set of Integers]" "[Set of Integers]" "ℤ")
        ("reals" "\\mathbb{R}" t "&#8484" "[Set of Reals]" "[Set of Reals]" "ℝ")
        ("nullset" "\\emptyset" t "&Phi;" "Phi" "Phi" "∅")
        ("null" "\\emptyset" t "&Phi;" "Phi" "Phi" "∅")))
(setq org-pretty-entities t)

(setq org-fontify-done-headline t)

;; Just some switches
(defun work-org-mode ()
  (interactive)
  (setq org-directory "~/Dropbox/work/")
  (setq org-agenda-files (list org-directory))
  (load-file (concat emacs-repos-dir "org.el")))

(defun home-org-mode ()
  (interactive)
  (setq org-directory "~/Dropbox/org/")
  (setq org-agenda-files (list org-directory))
  (load-file (concat emacs-repos-dir "org.el")))


(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))

        ;; other commands here
        ))))

;; Make the agenda
(org-agenda-list)
(setq org-list-allow-alphabetical t)

;;; FixMe: This is submitted to the mailing list but not committed
(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas.
With prefix arg ALL, do this for all lines in the table.
With the prefix argument ALL is `(16)' \
\(a double \\[universal-prefix] \\[universal-prefix] prefix), or if
it is the symbol `iterate', recompute the table until it no longer changes.
If NOALIGN is not nil, do not re-align the table after the computations
are done.  This is typically used internally to save time, if it is
known that the table will be realigned a little later anyway."
  (interactive "P")
  (or (memq this-command org-recalc-commands)
      (setq org-recalc-commands (cons this-command org-recalc-commands)))
  (unless (org-at-table-p) (user-error "Not at a table"))
  (if (or (eq all 'iterate) (equal all '(16)))
      (org-table-iterate)
    (org-table-get-specials)
    (let* ((eqlist (sort (org-table-get-stored-formulas)
                         (lambda (a b) (string< (car a) (car b)))))
           (eqlist1 (copy-sequence eqlist))
           (inhibit-redisplay (not debug-on-error))
           (line-re org-table-dataline-regexp)
           (thisline (org-current-line))
           (thiscol (org-table-current-column))
           seen-fields lhs1
           beg end entry eqlnum eqlname eqlname1 eql (cnt 0) eq a name name1)
      ;; Insert constants in all formulas
      (when eqlist
        (setq eqlist
              (mapcar (lambda (x)
                        (if (string-match "^@-?I+" (car x))
                            (user-error "Can't assign to hline relative reference"))
                        (when (string-match "\\`$[<>]" (car x))
                          (setq lhs1 (car x))
                          (setq x (cons (substring
                                         (org-table-formula-handle-first/last-rc
                                          (car x)) 1)
                                        (cdr x)))
                          (if (assoc (car x) eqlist1)
                              (user-error "\"%s=\" formula tries to overwrite existing formula for column %s"
                                          lhs1 (car x))))
                        (cons
                         (org-table-formula-handle-first/last-rc (car x))
                         (org-table-formula-substitute-names
                          (org-table-formula-handle-first/last-rc (cdr x)))))
                      eqlist))
        ;; Split the equation list
        (while (setq eq (pop eqlist))
          (if (<= (string-to-char (car eq)) ?9)
              (push eq eqlnum)
            (push eq eqlname)))
        (setq eqlnum (nreverse eqlnum) eqlname (nreverse eqlname))
        ;; Expand ranges in lhs of formulas
        (setq eqlname (org-table-expand-lhs-ranges eqlname))

        ;; Get the correct line range to process
        (if all
            (progn
              (setq end (move-marker (make-marker) (1+ (org-table-end))))
              (goto-char (setq beg (org-table-begin)))
              (if (re-search-forward org-table-calculate-mark-regexp end t)
                  ;; This is a table with marked lines, compute selected lines
                  (setq line-re org-table-recalculate-regexp)
                ;; Move forward to the first non-header line
                (if (and (re-search-forward org-table-dataline-regexp end t)
                         (re-search-forward org-table-hline-regexp end t)
                         (re-search-forward org-table-dataline-regexp end t))
                    (setq beg (match-beginning 0))
                  nil))) ;; just leave beg where it is
          (setq beg (point-at-bol)
                end (move-marker (make-marker) (1+ (point-at-eol)))))
        (goto-char beg)

        ;; First find the named fields, and mark them untouchable.
        ;; Also check if several field/range formulas try to set the same field.
        (remove-text-properties beg end '(org-untouchable t))
        (while (setq eq (pop eqlname))
          (setq name (car eq)
                a (assoc name org-table-named-field-locations))
          (setq name1 name)
          (if a (setq name1 (format "@%d$%d" (org-table-line-to-dline (nth 1 a))
                                    (nth 2 a))))
          (when (member name1 seen-fields)
            (user-error "Several field/range formulas try to set %s" name1))
          (push name1 seen-fields)

          (and (not a)
               (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" name)
               (setq a (list name
                             (condition-case nil
                                 (aref org-table-dlines
                                       (string-to-number (match-string 1 name)))
                               (error (user-error "Invalid row number in %s"
                                                  name)))
                             (string-to-number (match-string 2 name)))))
          (when (and a (or all (equal (nth 1 a) thisline)))
            (org-goto-line (nth 1 a))
            (org-table-goto-column (nth 2 a))
            (push (append a (list (cdr eq))) eqlname1)
            (org-table-put-field-property :org-untouchable t)))
        (setq eqlname1 (nreverse eqlname1))

        ;; Now evaluate the column formulas, but skip fields covered by
        ;; field formulas
        (goto-char beg)
        (while (re-search-forward line-re end t)
          (unless (string-match "^ *[_^!$/] *$" (org-table-get-field 1))
            ;; Unprotected line, recalculate
            (setq org-last-recalc-line (org-current-line))
            (setq eql eqlnum)
            (while (setq entry (pop eql))
              (org-goto-line org-last-recalc-line)
              (org-table-goto-column (string-to-number (car entry)) nil 'force)
              (unless (get-text-property (point) :org-untouchable)
                (org-table-eval-formula nil (cdr entry)
                                        'noalign 'nocst 'nostore 'noanalysis)))))

        ;; Now evaluate the field formulas
        (while (setq eq (pop eqlname1))
          (org-goto-line (nth 1 eq))
          (org-table-goto-column (nth 2 eq))
          (org-table-eval-formula nil (nth 3 eq) 'noalign 'nocst
                                  'nostore 'noanalysis))

        (org-goto-line thisline)
        (org-table-goto-column thiscol)
        (remove-text-properties (point-min) (point-max) '(org-untouchable t))
        (or noalign (and org-table-may-need-update (org-table-align)))

        ;; back to initial position
        (org-goto-line thisline)
        (org-table-goto-column thiscol)
        (or noalign (and org-table-may-need-update (org-table-align)))))))
;;; FixMe: End submission
