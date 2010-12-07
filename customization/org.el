;;Checkout the latest version of org mode, if I don't already have it.
(require 'org-install)

;;Agenda customizations
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files (list org-directory))
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;;Time logging
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

;;org-remember - quickly jot down thoughts
(org-remember-insinuate)
(setq org-remember-templates
      `(("Todo" ?t "* TODO %?\n  %i\n " ,(concat org-directory "remember.org") "Tasks")))
(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-default-notes-file (concat org-directory "/remember.org"))

;;General org customizations
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

;;TODO customizations
(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "|" "CANCELED(c@/!)")
        (sequence "|" "STALLED(s@/!)")
        (sequence "PENDING(p@/!)" "|" )))

(setq org-todo-keyword-faces
      '(("CANCELED"  . (:foreground "blue" :weight bold))
        ("STALLED"  . (:foreground "RED" :weight bold))
        ("PENDING"  . (:foreground "orange" :weight bold))))

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

;;If yasnippet is loaded, make TAB work properly with both org and yasnippet
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

(defun bh/insert-inactive-timestamp ()
  "Insert a timestamp for the current time at point."
  (interactive)
  (save-excursion
    (insert "\n")
    (org-cycle)
    (org-insert-time-stamp nil t t nil nil nil)))

;;General hooks for org and agenda
(add-hook 'org-mode-hook (lambda () (auto-revert-mode t)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-agenda-mode-hook '(lambda () (cd (car org-agenda-files))))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-heading-respect-content)))

;;Keybindings
(define-key org-mode-map (kbd "C-<RET>" ) 'org-insert-heading-respect-content)
(global-set-key (kbd "C-c i") 'bh/insert-inactive-timestamp)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(define-key global-map "\C-cr" 'org-remember)

(setq org-hide-leading-stars t)
(require 'org)
(when (locate-library "org-babel-init")(require 'org-babel-init))
(when (locate-library "org-depend") (require 'org-depend))

(defun my-org-pending-remove-scheduled ()
  (when (string= state "PENDING")
    (org-remove-timestamp-with-keyword org-scheduled-string)))

(add-hook 'org-after-todo-state-change-hook 'my-org-pending-remove-scheduled)

;;graph habits
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(add-hook 'org-mode-hook 'org-hide-block-all)

(setq org-entities-user
      '(("intersection" "\\cap" t "&cap;" "[intersection]" "[intersection]" "∩")
        ("union" "\\cup" t "&cup;" "[union]" "[union]" "∪")
        ("Z" "\\mathbb{Z}" t "&#8477" "[Set of Integers]" "[Set of Integers]" "ℤ")
        ("R" "\\mathbb{R}" t "&#8484" "[Set of Reals]" "[Set of Reals]" "ℝ")
        ("integers" "\\mathbb{Z}" t "&#8477" "[Set of Integers]" "[Set of Integers]" "ℤ")
        ("reals" "\\mathbb{R}" t "&#8484" "[Set of Reals]" "[Set of Reals]" "ℝ")
        ("nullset" "\\emptyset" t "&Phi;" "Phi" "Phi" "∅")
        ("null" "\\emptyset" t "&Phi;" "Phi" "Phi" "∅")))

;; (defun org-pretty-entities ()
;;   (interactive)
;;   (mapcar
;;    (lambda (lst)
;;      (font-lock-add-keywords
;;          nil (mapcar
;;               (lambda (el)
;;                 (list
;;                  (concat "\\(" (regexp-quote "\\") (nth 0 el) "\\)[\s^_]"  )
;;                  `(0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                             ,(nth 6 el)) nil))))
;;               lst)))
;;    (list org-entities org-entities-user)))

;; (remove-hook 'find-file-hook 'org-pretty-entities)

(setq org-agenda-repeating-timestamp-show-all nil)
(org-agenda-list)

(define-key org-remember-mode-map (kbd "C-x C-s") 'org-remember-finalize)



