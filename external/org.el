(setq org-alphabetical-lists t)

;; Agenda customizations
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files (list org-directory))
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

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
(org-remember-insinuate)
(setq org-remember-templates
      `(("Todo" ?t "* TODO %?\n  %i\n " ,(concat org-directory "remember.org") "Tasks")))
(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-default-notes-file (concat org-directory "/remember.org"))

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITINGRESPONSE(t)" "|" "DONE(d!)")
        (sequence "|" "CANCELED(c@/!)")))

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

;; If yasnippet is loaded, make TAB work properly with both org and yasnippet
(when (locate-library "yasnippet")
  (require 'yasnippet)
  (progn
    (defun yas/org-very-safe-expand ()
      (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
    (add-hook 'org-mode-hook
              (lambda ()
                (make-variable-buffer-local 'yas/trigger-key)
                (setq yas/trigger-key [tab])
                (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                (define-key yas/keymap [tab] 'yas/next-field)))))

;; General hooks for org and agenda
(add-hook 'org-mode-hook (lambda () (auto-revert-mode t)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
(remove-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-agenda-mode-hook '(lambda () (cd (car org-agenda-files))))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-heading-respect-content)))

;; Turn on appointment checking
(setq appt-time-msg-list nil)
(appt-activate 1)
(setq appt-display-format 'window)

(defadvice  org-agenda-redo (after org-agenda-redo-add-appts activate)
  "Pressing `r' on the agenda will also add appointments."
  (progn
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

;; Setup
(when (locate-library "org-babel-init")(require 'org-babel-init))
(when (locate-library "org-depend") (require 'org-depend))

(defun my-org-pending-remove-scheduled ()
  (when (string= state "PENDING")
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
  (load-file (concat emacs-repos-dir "customization/org.el")))
(defun home-org-mode ()
  (interactive)
  (setq org-directory "~/Dropbox/org/")
  (load-file (concat emacs-repos-dir "customization/org.el")))

;; Make the agenda
(org-agenda-list)
