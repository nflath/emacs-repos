;; Allow use of alphabetical lists
(setq org-alphabetical-lists t)

;; Cleanup-alarms script for ical

;; Agenda customizations
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files (list org-directory))
(setq org-agenda-files (list "~/Dropbox/org" "~/Dropbox/org/info" "~/Dropbox/org/trips"))
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; Auto-recalculate table lines
(defadvice org-cycle (after org-reactivate-always activate)
  (if (org-at-table-p)
      (org-table-recalculate t)))

;; Time logging
(org-clock-persistence-insinuate)
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
         "* TODO %?")))

(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-default-notes-file (concat org-directory "TODO.org"))


(defun my-org-capture-dont-ask ()
  (interactive)
  (org-capture 1 "t"))


(defadvice org-capture-finalize (after flush-blanks activate)
  (save-current-buffer
    (find-file (concat org-directory "TODO.org"))
    (beginning-of-buffer)
    (flush-lines "^\\s-*$")))

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
(setq org-archive-location (concat org-directory "Archive.org::"))
(setq org-hide-leading-stars t)

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "LATER(l)" "WAITING(w/!)" "|" "DONE(d!)" "CANCELED(c)")))

;; General hooks for org and agenda
(add-hook 'org-mode-hook (lambda () (auto-revert-mode t)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
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
           (starttime (float-time))
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

(defun cleanup-alarms-for-gmail ()
  (interactive)
  (save-window-excursion
    (find-file "~/Dropbox/org/calendar.org")
    (goto-char (point-min))
    (while (search-forward "* Alarm notification" nil t)
      (beginning-of-line)
      (forward-char 2)
      (let* ((bolp (point))
             (eolp (progn (end-of-line) (point)))
             (bosp (progn (search-forward "SUMMARY:") (point)))
             (eosp (progn (end-of-line) (point)))
             (summary (buffer-substring bosp eosp)))
        (message summary)
        (delete-region bosp eosp)
        (delete-region bolp eolp)
        (goto-char bolp)
        (insert summary))
      (save-buffer)
      )))

(defun sync-google-calendar ()
  (interactive)
  (start-process-shell-command "sync_google_calendar_to_org" "foo" "~/bin/sync_google_calendar_to_org"))
(setq org-google-sync (run-at-time 0 300 'sync-google-calendar))
;;(defun org-add-log-note (&optional purpose))

;; (defadvice org-add-log-note (around org-only-log-habits activate)
;;   (if (string-equal (org-entry-get nil "STYLE" t) "habit") ad-do-it))

(defun rasmus/remove-schedule ()
  "Remove SCHEDULED-cookie is switching state to WAITING."
  (save-excursion
    (and (equal (org-get-todo-state) "WAITING")
         (org-get-scheduled-time (point))
         (when (search-forward-regexp org-scheduled-time-regexp nil t)
           (or (delete-region (match-beginning 0) (match-end 0)) t))
         )))

(add-hook 'org-after-todo-state-change-hook
          'rasmus/remove-schedule)


(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
           (scheduled-repeat (org-get-repeat org-scheduled-string))
           (end (org-entry-end-position))
           (habit-entry (org-no-properties (nth 4 (org-heading-components))))
           closed-dates deadline dr-days sr-days)
      (if scheduled
          (setq scheduled (time-to-days scheduled))
        (error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
        (error
         "Habit '%s' has no scheduled repeat period or has an incorrect one"
         habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat))
      (unless (> sr-days 0)
        (error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
        (setq dr-days (org-habit-duration-to-days
                       (match-string-no-properties 1 scheduled-repeat)))
        (if (<= dr-days sr-days)
            (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
                   habit-entry scheduled-repeat))
        (setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
             (reversed org-log-states-order-reversed)
             (search (if reversed 're-search-forward 're-search-backward))
             (limit (if reversed end (point)))
             (count 0))
        (unless reversed (goto-char end))
        (while (and (< count maxdays)
                    (funcall search (format "- State \"DONE\".*\\[\\([^]]+\\)\\]"
                                            (regexp-opt org-done-keywords))
                             limit t))
          (push (time-to-days
                 (org-time-string-to-time (match-string-no-properties 1)))
                closed-dates)
          (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates))))


(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "green" :weight bold))
        ("CANCELED"  . (:foreground "red" :weight bold :strike-through t))
        ("WAITING"  . (:foreground "cyan"))))

(defun org-make-habit ()
  (interactive)
  (insert ":PROPERTIES:\n" ":STYLE: habit\n" ":END:\n"))


(defun org-agenda-get-scheduled (&optional deadline-results with-hour)
  "Return the scheduled information for agenda display.
When WITH-HOUR is non-nil, only return scheduled items with
an hour specification like [h]h:mm."
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'done-face 'org-agenda-done
                      'mouse-face 'highlight
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name buffer-file-name))))
         (regexp (if with-hour
                     org-scheduled-time-hour-regexp
                   org-scheduled-time-regexp))
         (todayp (org-agenda-todayp date)) ; DATE bound by calendar
         (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
         mm
         (deadline-position-alist
          (mapcar (lambda (a) (and (setq mm (get-text-property
                                        0 'org-hd-marker a))
                              (cons (marker-position mm) a)))
                  deadline-results))
         d2 diff pos pos1 category category-pos level tags donep
         ee txt head pastschedp todo-state face timestr s habitp show-all
         did-habit-check-p warntime inherited-tags ts-date suppress-delay
         ddays)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
        (org-agenda-skip)
        (setq s (match-string 1)
              txt nil
              pos (1- (match-beginning 1))
              todo-state (save-match-data (org-get-todo-state))
              show-all (or (eq org-agenda-repeating-timestamp-show-all t)
                           (member todo-state
                                   org-agenda-repeating-timestamp-show-all))
              d2 (org-time-string-to-absolute
                  s d1 'past show-all (current-buffer) pos)
              diff (- d2 d1)
              warntime (get-text-property (point) 'org-appt-warntime))
        (setq pastschedp (and todayp (< diff 0)))
        (setq did-habit-check-p nil)
        (setq suppress-delay
              (let ((ds (and org-agenda-skip-scheduled-delay-if-deadline
                             (let ((item (buffer-substring (point-at-bol) (point-at-eol))))
                               (save-match-data
                                 (and (string-match
                                       org-deadline-time-regexp item)
                                      (match-string 1 item)))))))
                (cond
                 ((not ds) nil)
                 ;; The current item has a deadline date (in ds), so
                 ;; evaluate its delay time.
                 ((integerp org-agenda-skip-scheduled-delay-if-deadline)
                  ;; Use global delay time.
                  (- org-agenda-skip-scheduled-delay-if-deadline))
                 ((eq org-agenda-skip-scheduled-delay-if-deadline
                      'post-deadline)
                  ;; Set delay to no later than deadline.
                  (min (- d2 (org-time-string-to-absolute
                              ds d1 'past show-all (current-buffer) pos))
                       org-scheduled-delay-days))
                 (t 0))))
        (setq ddays (if suppress-delay
                        (let ((org-scheduled-delay-days suppress-delay))
                          (org-get-wdays s t t))
                      (org-get-wdays s t)))
        ;; Use a delay of 0 when there is a repeater and the delay is
        ;; of the form --3d
        (when (and (save-match-data (string-match "--[0-9]+[hdwmy]" s))
                   (< (org-time-string-to-absolute s)
                      (org-time-string-to-absolute
                       s d2 'past nil (current-buffer) pos)))
          (setq ddays 0))
        ;; When to show a scheduled item in the calendar:
        ;; If it is on or past the date.
        (when (or (and (> ddays 0) (= diff (- ddays)))
                  (and (zerop ddays) (= diff 0))
                  (and (< (+ diff ddays) 0)
                       (< (abs diff) org-scheduled-past-days)
                       (and todayp (not org-agenda-only-exact-dates)))
                  ;; org-is-habit-p uses org-entry-get, which is expansive
                  ;; so we go extra mile to only call it once
                  (and todayp
                       (boundp 'org-habit-show-all-today)
                       org-habit-show-all-today
                       (setq did-habit-check-p t)
                       (setq habitp (and (functionp 'org-is-habit-p)
                                         (org-is-habit-p)))))
          (when (and (or (not (org-entry-get nil "HIDDEN-UNTIL"))
                         (progn
                           (and todayp)))
                     (or (not (org-entry-get nil "HIDDEN-DATE"))
                         (progn (print "hi")
                                (print (current-buffer))
                                (print (point))
                                (let ((d3 (org-time-string-to-absolute (org-entry-get nil "HIDDEN-DATE"))))
                                  (not (= today d3))))))
          (save-excursion
            (setq donep (member todo-state org-done-keywords))
            (if (and donep
                     (or org-agenda-skip-scheduled-if-done
                         (not (= diff 0))
                         (and (functionp 'org-is-habit-p)
                              (org-is-habit-p))))
                (setq txt nil)
              (setq habitp (if did-habit-check-p habitp
                             (and (functionp 'org-is-habit-p)
                                  (org-is-habit-p))))
              (setq category (org-get-category)
                    category-pos (get-text-property (point) 'org-category-position))
              (if (and (eq org-agenda-skip-scheduled-if-deadline-is-shown
                           'repeated-after-deadline)
                       (org-get-deadline-time (point))
                       (<= 0 (- d2 (time-to-days (org-get-deadline-time (point))))))
                  (throw :skip nil))
              (if (not (re-search-backward "^\\*+[ \t]+" nil t))
                  (throw :skip nil)
                (goto-char (match-end 0))
                (setq pos1 (match-beginning 0))
                (if habitp
                    (if (or (not org-habit-show-habits)
                            (and (not todayp)
                                 (boundp 'org-habit-show-habits-only-for-today)
                                 org-habit-show-habits-only-for-today))
                        (throw :skip nil))
                  (if (and
                       (or (eq t org-agenda-skip-scheduled-if-deadline-is-shown)
                           (and (eq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
                                pastschedp))
                       (setq mm (assoc pos1 deadline-position-alist)))
                      (throw :skip nil)))
                (setq inherited-tags
                      (or (eq org-agenda-show-inherited-tags 'always)
                          (and (listp org-agenda-show-inherited-tags)
                               (memq 'agenda org-agenda-show-inherited-tags))
                          (and (eq org-agenda-show-inherited-tags t)
                               (or (eq org-agenda-use-tag-inheritance t)
                                   (memq 'agenda org-agenda-use-tag-inheritance))))

                      tags (org-get-tags-at nil (not inherited-tags)))
                (setq level (make-string (org-reduced-level (org-outline-level)) ? ))
                (setq head (buffer-substring
                            (point)
                            (progn (skip-chars-forward "^\r\n") (point))))
                (if (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
                    (setq timestr
                          (concat (substring s (match-beginning 1)) " "))
                  (setq timestr 'time))
                (setq txt (org-agenda-format-item
                           (if (= diff 0)
                               (car org-agenda-scheduled-leaders)
                             (format (nth 1 org-agenda-scheduled-leaders)
                                     (- 1 diff)))
                           head level category tags
                           (if (not (= diff 0)) nil timestr)
                           nil habitp))))
            (when txt
              (setq face
                    (cond
                     ((and (not habitp) pastschedp)
                      'org-scheduled-previously)
                     (todayp 'org-scheduled-today)
                     (t 'org-scheduled))
                    habitp (and habitp (org-habit-parse-todo)))
              (org-add-props txt props
                'undone-face face
                'face (if donep 'org-agenda-done face)
                'org-marker (org-agenda-new-marker pos)
                'org-hd-marker (org-agenda-new-marker pos1)
                'type (if pastschedp "past-scheduled" "scheduled")
                'date (if pastschedp d2 date)
                'ts-date d2
                'warntime warntime
                'level level
                'priority (if habitp
                              (org-habit-get-priority habitp)
                            (+ 94 (- 5 diff) (org-get-priority txt)))
                'org-category category
                'category-position category-pos
                'org-habit-p habitp
                'todo-state todo-state)
              (push txt ee)))))))
    (nreverse ee)))

(defun org-time-stamp-string (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
WITH-HM means use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (let ((fmt (funcall (if with-hm 'cdr 'car) org-time-stamp-formats))
        stamp
        (result ""))
    (if inactive (setq fmt (concat "[" (substring fmt 1 -1) "]")))

    ;(insert-before-markers (or pre ""))
    (when (listp extra)
      (setq extra (car extra))
      (if (and (stringp extra)
               (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
          (setq extra (format "-%02d:%02d"
                              (string-to-number (match-string 1 extra))
                              (string-to-number (match-string 2 extra))))
        (setq extra nil)))
    (when extra
      (setq fmt (concat (substring fmt 0 -1) extra (substring fmt -1))))
    (setq result (concat result (setq stamp (format-time-string fmt time))))
    (setq result (concat result (insert-before-markers (or post ""))))
    (setq org-last-inserted-timestamp stamp)))

(defun org-agenda-hide-today (&rest args)
  (interactive)
  (let* ((col (current-column))
         (marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (hdmarker (org-get-at-bol 'org-hd-marker))
         (pos (marker-position marker))
         just-one)

    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-entry-put nil "HIDDEN-DATE" (org-time-stamp-string (current-time) nil t))
        (setq newhead (org-get-heading))
        (when (and (org-bound-and-true-p
                    org-agenda-headline-snapshot-before-repeat)
                   (not (equal org-agenda-headline-snapshot-before-repeat
                               newhead))
                   todayp)
          (setq newhead org-agenda-headline-snapshot-before-repeat
                just-one t)))
      (org-agenda-change-all-lines (concat "HIDDEN " newhead) hdmarker 'fixface just-one))
    ))

(defun org-agenda-hide-until (&rest args)
  (interactive)
  (let* ((col (current-column))
         (marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (hdmarker (org-get-at-bol 'org-hd-marker))
         (pos (marker-position marker))
         just-one)

    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-entry-put nil "HIDDEN-UNTIL" "t")
        (setq newhead (org-get-heading))
        (when (and (org-bound-and-true-p
                    org-agenda-headline-snapshot-before-repeat)
                   (not (equal org-agenda-headline-snapshot-before-repeat
                               newhead))
                   todayp)
          (setq newhead org-agenda-headline-snapshot-before-repeat
                just-one t)))
      (org-agenda-change-all-lines (concat "HIDDEN " newhead) hdmarker 'fixface just-one))
    ))

(org-defkey org-agenda-mode-map "h" 'org-agenda-hide-today)
(org-defkey org-agenda-mode-map "H" 'org-agenda-hide-until)
