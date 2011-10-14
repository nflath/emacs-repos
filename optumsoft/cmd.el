
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Neat features:
;; 
;; Most neat features (intentionally) work only when the cursor is to
;; the right of a command prompt.
;; 
;; ^C interrupts your current job, ^Z suspends it.
;; ^A moves to the beginning of the command being typed.
;; Delete and meta-delete work only on the command being typed.
;; Meta-. starts/stops command redirection.
;; Meta-p and meta-n go through command history.
;; C-x p masks the next line typed from displaying on the screen.
;; The "man" command does neat, funky stuff.
;; A new "cwd" command "cd"'s to the previous buffer's default directory.
;; The "su" command does the right thing!
;; ^L repositions the command window (and the redirection window) nicely.
;; Control-Meta-W puts the last command's output on the kill ring.
;; Use ^Q^J to insert literal newlines into commands.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cmd-history-max 50)
(defvar cmd-specials '(("man" . cmd-man)
                       ("vi" . cmd-no-go)
                       ("su" . cmd-su)
                       ("kinit" . (lambda (arg) (cmd-su arg "kinit")))
                       ("more" . cmd-more)
                       ("cd" . cmd-cd)
                       ("pushd" . (lambda (arg) (cmd-dir-changing-command "pushd" arg)))
                       ("popd" . (lambda (arg) (cmd-dir-changing-command "popd" arg)))
                       ("cwd" . cmd-set-cwd)))
(defvar cmd-shell-arguments '("-i"))
(defvar cmd-run-commands (if (file-exists-p "~/.cmdrc")
                             (concat "source " (getenv "HOME") "/.cmdrc")
                           nil))
(defvar cmd-rs6000-mode nil)
(defvar cmd-win32-mode (or (fboundp 'w32-version)
                           (and (boundp 'windows) windows)
                           (eq window-system 'mswindows)))
(defvar cmd-shell-name (or (getenv "SHELL") (and cmd-win32-mode "c:/windows/system32/cmd.exe") "/bin/csh"))

(defvar cmd-colorize-prompt (featurep 'xemacs))

(defvar newline "\n")

;;; This function is called after a `cd' command is executed, and is
;;; passed the name of the new working directory as a string.  To
;;; use:  (setq cmd-cd-function (function (lambda (newdir) ...)))

(defvar cmd-cd-function nil)
(setq cmd-cd-function (function (lambda (newdir) (message newdir))))

;;; There are two ways of getting cmd to know your working directory.
;;; you can either use the following regexp, which it will try to use to
;;; parse your prompt, or you can make it remember, by setting the remember
;;; option to t.  If it's remembering, pushd and popd won't work... but if
;;; it's prompt-parsing, it won't be able to tell outside of the shell (i.e.,
;;; if you're running something inside the shell.)  Take your pick.

(defvar cmd-working-directory-regexp
  (if cmd-win32-mode
      "^\\([a-zA-Z]:?[\\/~][^:%>\n]*\\)"
    "^[^/~\n]*\\([/~][^ :%>\n]*\\)"))

(defvar cmd-remember-working-directory nil) ;set to t to catch cd rather than parse prompt

;;; special functions return t to send a CR to the shell when they exit
;;; (usually to generate a new prompt for a user)   

(defvar cmd-mode-map (make-sparse-keymap))
(progn 
  (define-key cmd-mode-map "\C-?" 'cmd-delete-backward-char)
  (define-key cmd-mode-map "\C-a" 'cmd-beginning-of-line)
  (define-key cmd-mode-map "\e\C-?" 'cmd-backward-kill-word)
  (define-key cmd-mode-map "\e\C-w" 'cmd-copy-output-as-kill)
  (define-key cmd-mode-map "\ep" 'cmd-previous-history)
  (define-key cmd-mode-map "\en" 'cmd-next-history)
  (define-key cmd-mode-map "\C-l" 'cmd-recenter)
  (define-key cmd-mode-map "\C-z" 'cmd-stop-process)
  (define-key cmd-mode-map "\C-c" 'cmd-interrupt-process)
  (define-key cmd-mode-map "\C-]" 'cmd-send-control-right-bracket)
  (define-key cmd-mode-map "\C-xp" 'cmd-read-password)
  (define-key cmd-mode-map "\C-xs" 'cmd-send-shell)
; (define-key cmd-mode-map "\e." 'cmd-redirect)
  (define-key cmd-mode-map "\C-d" 'cmd-delete-char)
  (define-key cmd-mode-map "\C-x\C-f" 'cmd-find-file)
  (define-key cmd-mode-map "\C-i" 'cmd-completion)
  (define-key cmd-mode-map "\C-m" 'cmd-execute))

(defvar cmd-last-cmd ?1)

(defvar cmd-top-m nil)
(defvar cmd-prompt-m nil)
(defvar cmd-cmd-m nil)
(defvar cmd-shell nil)
(defvar cmd-redirection nil)
(defvar cmd-just-started nil)
(defvar cmd-working-directory nil)
(defvar cmd-history nil)
(defvar cmd-history-end nil)
(defvar cmd-history-count nil)
(defvar cmd-history-pointer nil)
(defvar cmd-pre-completion-configuration nil)
(mapcar 'make-variable-buffer-local 
        '(cmd-top-m cmd-prompt-m cmd-cmd-m cmd-shell cmd-redirection
          cmd-just-started cmd-working-directory cmd-history cmd-history-end
          cmd-history-count cmd-history-pointer cmd-pre-completion-configuration))

(defun cmd ()
  (interactive)
  (cmd-goto cmd-last-cmd))
      

  

(defun cmd-goto (arg)
  (interactive "c")
  (if (or (< arg 32) (> arg 126))
      (error "Please use printable character for cmd name"))
  (let* ((buffer (get-buffer-create (format "*cmd*<%c>" arg)))
         (window (get-buffer-window buffer)))
    (if window 
        (select-window window)
      (switch-to-buffer buffer)))
  (setq cmd-last-cmd arg)
  (if (eq major-mode 'cmd-mode)
      (if (eq (point-max) (1+ (point)))
          (goto-char (point-max))))
  (if (or (not (eq major-mode 'cmd-mode))
          (not (eq (process-status cmd-shell) 'run)))
      (cmd-mode)))

; Run this out of .emacs if you want to have a *cmd* waiting.
(defun cmd-start (ch)
  (let ((buffer-name (format "*cmd*<%c>" ch)))
    (or (get-buffer buffer-name)
        (save-excursion
          (set-buffer (generate-new-buffer buffer-name))
          (cmd-mode)))))
  
(defun cmd-rs6000 ()
  (interactive)
  (cmd-send-shell "\C-mstty nl -inlcr icrnl -echo susp \\^Z intr \\^C isig\C-munset lineedit\C-j")
  (setq cmd-rs6000-mode t)
  (message "RS 6000 compatibility enabled"))


(defun cmd-mode ()
  (setq cmd-top-m (make-marker))
  (setq cmd-prompt-m (make-marker))
  (setq cmd-cmd-m (make-marker))
  (setq cmd-history nil)
  (setq cmd-history-pointer nil)
  (setq cmd-history-count 0)
  (setq cmd-history-end nil)
  (setq cmd-redirection nil)
  (setq cmd-working-directory (getenv "HOME"))
  (use-local-map cmd-mode-map)
  (setq mode-name "Cmd")
  (setq major-mode 'cmd-mode)
  ;  (setq local-abbrev-table cmd-mode-abbrev-table)
  ;  (set-syntax-table cmd-mode-syntax-table)
  (cmd-start-shell)
  (run-hooks 'cmd-mode-hook))

(defun cmd-start-shell ()
  (end-of-buffer)
  (insert "\n*** Starting cmd process\n")
  (if (not (file-directory-p default-directory))
      (setq default-directory (concat (getenv "HOME") "/")))
  (if (not (file-directory-p default-directory))
      (setq default-directory "/"))
  (cmd-prompt)
  (let ((process-environment process-environment)
        (execfile (if (file-executable-p cmd-shell-name) cmd-shell-name
                    (progn
                      (goto-char (point-min))
                      (insert "*** could not start " cmd-shell-name 
                              "; using csh instead\n")
                      (goto-char (point-max))
                      "/bin/csh"))))
    (setenv "TERM" "emacs")
    (let ((process-connection-type t))
      (setq cmd-shell (apply 'start-process 
                             (append (list "cmd" (current-buffer) execfile)
                                     cmd-shell-arguments)))))
  (setq cmd-just-started t)
  (set-process-filter cmd-shell 'cmd-filter))

;;;;;;;;;;;;;;;;;;;;; COMMAND HISTORY

(defun cmd-add-history (command)
  (let ((new (cons (cons command nil) cmd-history)))
    (if cmd-history 
        (rplacd (car cmd-history) new)
      (setq cmd-history-end new))
    (setq cmd-history new)
    (if (/= cmd-history-count cmd-history-max)
        (setq cmd-history-count (1+ cmd-history-count))
      (setq cmd-history-end (cdr (car cmd-history-end)))
      (rplacd cmd-history-end nil))))

(defun cmd-set-history (hist command)
  (rplaca (car hist) command))

(defun cmd-goto-history (hist)
  (cmd-set-history 
    cmd-history-pointer
    (concat (buffer-substring cmd-cmd-m (point-max)) newline))
  (delete-region cmd-cmd-m (point-max))
  (goto-char cmd-cmd-m)
  (setq cmd-history-pointer hist)
  (insert (car (car hist)))
  (delete-backward-char 1))

;;;;;;;;;;;;;;;;;;;;;; COMMAND EXECUTION

(defun cmd-prompt ()
  (set-marker cmd-top-m (point))
  (set-marker cmd-prompt-m (point))
  (insert ">")
  (set-marker cmd-cmd-m (point))
  (or (and cmd-history (equal (car (car cmd-history)) newline))
      (cmd-add-history newline))
  (setq cmd-history-pointer cmd-history))


(defun cmd-color-prompt ()
  (save-excursion
    (beginning-of-line)
      ; color everything before the prompt red, and after blue
      (if (looking-at "^\\([^>]*\\)>\\(.*\\)$")
          (progn
            (set-extent-face (make-extent (match-beginning 1) (match-end 1))
                             'red)
            (set-extent-face (make-extent (match-beginning 2) (match-end 2)) 
                             'blue)))))

(defun cmd-execute ()
  (interactive)
  (if cmd-pre-completion-configuration
      (progn
        (set-window-configuration cmd-pre-completion-configuration)
        (setq cmd-pre-completion-configuration nil)
        (if (get-buffer "*Completions2*") (kill-buffer "*Completions2*"))))
  (delete-region cmd-prompt-m cmd-cmd-m)
  (goto-char cmd-cmd-m)
  (if cmd-colorize-prompt (cmd-color-prompt))
  (let ((was-prompt (> (current-column) 0)))
    (goto-char (point-max))
    (insert "\n")
    (let* ((command (buffer-substring cmd-cmd-m (point)))
           (wsp (string-match "[ \t\n]+" command))
           (first (substring command 0 wsp))
           (special (and was-prompt (assoc first cmd-specials)))
           (arg (substring command (match-end 0))))
      (if (or (eq (length arg) 0)
              (/= (aref arg (1- (length arg))) ?\n))
          (setq arg (concat arg "\n")))
      (if (not (equal newline "\n"))
          (let ((n 0))
            (while (< n (length command))
              (if (eq ?\n (aref command n))
                  (aset command n (aref newline 0)))
              (setq n (1+ n)))))
      (cmd-set-history cmd-history command)
      (cmd-prompt)
      (if special
          (if (funcall (cdr special) arg) 
              (cmd-send-shell newline))
        (cmd-send-shell command)))))

(defun cmd-redirect (arg)
  (interactive "P")
  (if arg
      (if cmd-redirection
          (progn 
            (switch-to-buffer-other-window cmd-redirection)
            (other-window 1))
        (message "Redirection is off"))
    (if cmd-redirection
        (progn
          (setq cmd-redirection nil)
          (message "Redirection turned off"))
      (setq cmd-redirection 
            (get-buffer-create 
              (concat ">>" (buffer-name (current-buffer)))))
      (message (format "Redirecting to buffer %s" 
                       (buffer-name cmd-redirection)))
      (switch-to-buffer-other-window cmd-redirection)
      (delete-region (point-min) (point-max))
      (make-local-variable 'cmd-prompt-m)
      (setq cmd-prompt-m (make-marker))
      (set-marker cmd-prompt-m (point-max))
      (other-window 1))))

(defun cmd-send-shell (string)
  (interactive "sSend shell: ")
  (process-send-string cmd-shell string)
  (if cmd-redirection
      (cmd-fake-output string)))

(defun cmd-filter (process string)
  (save-excursion
    (set-buffer (process-buffer process))
    (if cmd-redirection (set-buffer cmd-redirection))
    (goto-char cmd-prompt-m)
    (insert string)
    (set-marker cmd-prompt-m (point))
    (if (and cmd-remember-working-directory (not cmd-working-directory))
        (if (string-match "current-directory-is \\([^\n]*\\)" string)
            (setq cmd-working-directory (substring string 
                                                   (match-beginning 1) (match-end 1)))))
    (if cmd-just-started
        (progn 
          (setq cmd-just-started nil)
          (if cmd-run-commands
              (cmd-send-shell (concat cmd-run-commands newline)))))))

(defun cmd-cd (arg)
  (setq arg (cmd-fix-slashes (substring arg 0 -1)))
  (if cmd-remember-working-directory
      (progn
        (cmd-send-shell (concat "cd " arg "; echo current-directory-is $cwd" newline))
        (setq cmd-working-directory nil))
    (cmd-send-shell (concat "cd " arg newline)))
  (if cmd-cd-function
      (funcall cmd-cd-function arg))
  nil)

(defun cmd-dir-changing-command (cmd arg)
  (setq arg (cmd-fix-slashes (substring arg 0 -1)))
  (if cmd-remember-working-directory
      (progn
        (cmd-send-shell (concat cmd " " arg "; echo current-directory-is $cwd" newline))
        (setq cmd-working-directory nil))
    (cmd-send-shell (concat cmd " " arg newline)))
  nil)




(defun cmd-get-cwd ()
  (if cmd-remember-working-directory
      (setq default-directory (concat cmd-working-directory "/"))
    (save-excursion
      (beginning-of-line)
      (if (or (and (not (looking-at (regexp-quote ">")))
                   (looking-at cmd-working-directory-regexp))
              (progn
                (goto-char (point-max))
                (beginning-of-line)
                (looking-at cmd-working-directory-regexp))
              (progn
                (forward-line -1)
                (looking-at cmd-working-directory-regexp))
              (progn
                (forward-line -1)
                (looking-at cmd-working-directory-regexp))
              (progn
                (forward-line -1)
                (looking-at cmd-working-directory-regexp))
              (progn
                (forward-line -1)
                (looking-at cmd-working-directory-regexp)))
          (setq default-directory 
                (concat (buffer-substring (match-beginning 1) (match-end 1)) "/")))))
  (if cmd-win32-mode
      (if (string-match "[\\/][\\/]$" default-directory)
          (setq default-directory (concat (substring default-directory 0 (match-beginning 0)) "/")))
      (if (string-match "^//[a-zA-Z]/" default-directory)
          (setq default-directory 
                (concat (substring default-directory 2 3)
                        ":"
                        (substring default-directory 3))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;; SPECIALLY-HANDLED KEYS

(defun cmd-in-input ()
  (>= (point) cmd-cmd-m))

(defun narrow ()
  (if (cmd-in-input)
      (narrow-to-region cmd-cmd-m (point-max))))

(defun cmd-delete-char (arg)
  (interactive "p")
  (if (and (= (point) cmd-cmd-m) (eolp))
      (cmd-send-shell "\C-d")
    (delete-char arg)))
    

(defun cmd-delete-backward-char (arg)
  (interactive "p")
  (narrow)
  (unwind-protect 
      (backward-delete-char-untabify arg)
    (widen)))

(defun cmd-beginning-of-line (arg)
  (interactive "p")
  (if (= arg 1) (narrow))
  (unwind-protect
      (beginning-of-line arg)
    (widen)))

(defun cmd-previous-line (arg)
  (interactive "p")
  (if (= arg 1) (narrow))
  (unwind-protect
      (if (= arg 1)
          (cmd-previous-history arg)
        (previous-line arg))
    (widen)))

(defun cmd-next-line (arg)
  (interactive "p")
  (if (= arg 1) (narrow))
  (unwind-protect
      (if (= arg 1)
          (cmd-next-history arg)
        (next-line arg))
    (widen)))

(defun cmd-backward-kill-word (arg)
  (interactive "p")
  (narrow)
  (unwind-protect
      (backward-kill-word arg)
    (widen)))

(defun cmd-copy-output-as-kill ()
  (interactive)
  (if (cmd-in-input)
      (copy-region-as-kill cmd-top-m cmd-prompt-m)
    (append-next-kill)))

(defun cmd-previous-history (arg)
  (interactive "p")
  (if (and (eq arg 1) (cmd-in-input))
      (let ((prev (cdr cmd-history-pointer)))
        (if prev
            (cmd-goto-history prev)))
    (previous-line arg)))

(defun cmd-next-history (arg)
  (interactive "p")
  (if (and (eq arg 1) (cmd-in-input))
      (let ((next (cdr (car cmd-history-pointer))))
        (if next
            (cmd-goto-history next)))
    (next-line arg)))

(defun cmd-recenter (arg)
  (interactive "P")
  (if (or arg (not (cmd-in-input)))
      (recenter arg)
    (recenter (- (window-height (selected-window)) 2))
    (if cmd-redirection 
        (progn
          (switch-to-buffer-other-window cmd-redirection)
          (end-of-buffer)
          (recenter (- (window-height (selected-window)) 2))
          (other-window 1)))))

(defun cmd-stop-process ()
  (interactive)
  (if (or (cmd-in-input)
          (console-on-window-system-p))
      (cmd-send-shell "\C-z")
    (suspend-emacs)))

(defun cmd-interrupt-process ()
  (interactive)
  (if cmd-win32-mode
      (interrupt-process cmd-shell)
    (cmd-send-shell "\C-c")))

(defun cmd-send-control-right-bracket ()
  (interactive)
  (cmd-send-shell "\C-]"))
    

(defun cmd-find-file ()
  (interactive)
  (cmd-get-cwd)
  (call-interactively 'find-file))
          
(defun cmd-completion ()
  (interactive)
  (cmd-get-cwd)
  (let ((p (point))
        word
        comps
        filespec
        wordstart
        (wordpoint (make-marker))
        replacement
        wind)
    (widen)
    (if (re-search-backward "[> \t]" cmd-cmd-m 1000)
        (goto-char (1+ (point))))
    (setq word (buffer-substring (point) p))
    (let ((temp (point)))
      (goto-char p)
      (if (search-backward "/" temp 'foo)
          (goto-char (1+ (point)))))
    (set-marker wordpoint (point))
    (setq filespec 
          (expand-file-name
           (substitute-in-file-name (concat default-directory word))))
    (setq wordstart (string-match "[^/]*$" filespec))
    (setq word (substring filespec wordstart))
    (let ((prefix (concat "Completing " word " in directory "
                          (substring filespec 0 wordstart) "...")))
      (setq comps (file-name-all-completions word (substring filespec 0 wordstart)))
      (message (format "%s%d completions" prefix (length comps))))
    (cond
     ((not comps) 
      (message "No completions of %s" filespec) 
      (goto-char p))
     ((not (cdr comps)) 
      (setq replacement (car comps)))
     (t
      (setq wind (selected-window))
      (setq replacement (try-completion word (mapcar 'list comps)))
      (if (and (not (one-window-p)) cmd-pre-completion-configuration)
          (set-buffer (get-buffer-create "*Completions2*"))
        (setq cmd-pre-completion-configuration (current-window-configuration))
        (split-window)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*Completions2*")))
      (toggle-read-only 0)
      (erase-buffer)
      (insert "Possible completions are:\n")
      (while comps 
        (insert (car comps))
        (setq comps (cdr comps))
        (if comps
            (progn
              (indent-to-column 40)
              (insert (car comps))
              (insert "\n")
              (setq comps (cdr comps)))))
      (select-window wind)))
    (if replacement
        (progn
          (set-buffer (marker-buffer wordpoint))
          (goto-char wordpoint)
          (delete-region (point) p)
          (insert replacement)))))

;;;;;;;;;;;;;;; SPECIALLY-HANDLED COMMANDS

(defun cmd-fake-output (str)
  (cmd-filter cmd-shell str))

(defun cmd-compose-mode (send-func)
  (message "Press C-d at the end to send the message...")
  (let ((sendit
          (list
            '(delete-region cmd-prompt-m cmd-cmd-m)
            (list 'define-key '(current-local-map) "\C-j"
                  (list 'quote (lookup-key (current-local-map) "\C-j")))
            (list 'define-key '(current-local-map) "\C-m"
                  (list 'quote (lookup-key (current-local-map) "\C-m")))
            (list 'define-key '(current-local-map) "\C-d"
                  (list 'quote (lookup-key (current-local-map) "\C-d")))
            '(insert "\n")
            '(goto-char (point-max))
            (list send-func 'cmd-cmd-m '(point)))))
    (define-key (current-local-map) "\C-j"
      (append (list 'lambda nil '(interactive)) sendit))
    (define-key (current-local-map) "\C-d"
      (list 'lambda '(arg) '(interactive "p")
            (list 'if '(= (point) (point-max))
                          (cons 'progn sendit)
                          '(delete-char arg)))))
  (define-key (current-local-map) "\C-m" 'newline))
                            
(defun cmd-man (arg)
  (cmd-send-shell newline)
  (manual-entry (substring arg 0 -1))
  nil)

(defun cmd-make (arg)
  (cmd-get-cwd)
  (cmd-send-shell newline)
  (compile (concat  " make " (substring arg 0 -1)))
  nil)

(defun cmd-no-go (arg)
  (cmd-fake-output "You really, really don't want to do that.\n")
  t)

(defun cmd-set-cwd (arg)
  (let (wd)
    (save-excursion
      (set-buffer (car (cdr (buffer-list))))
      (setq wd default-directory))
    (cmd-send-shell (concat "cd " (cmd-fix-slashes wd))))
  t)

(defun cmd-emacs (arg)
  (cmd-no-go arg))

(defun cmd-more (arg)
  (cmd-get-cwd)
  (cmd-send-shell newline)
  (setq arg (substring arg 0 -1))
  (if (> (length arg) 0)
      (progn
        (if (not (eq ?/ (aref arg 0))) 
            (setq arg (concat default-directory arg)))
        (find-file-other-window arg)
        (setq buffer-read-only t)))
  nil)

(defun cmd-read-password ()
  (interactive)
  (let ((password "")
        (done nil))
    (while (not done)
      (message "Enter password: ")
      (let ((key (read-char)))
        (cond
          ((eq key ?\C-m) 
           (setq done t))
          ((eq key ?\C-?)
           (if (> (length password) 0)
               (setq password (substring password 0 -1))))
          ((and (> key ? ) (< key ?\C-?))
           (setq password (concat password (make-string 1 key))))
          (t
            (setq password nil)
            (setq unread-command-char key)
            (setq done t)))))
    (if password
        (cmd-send-shell (concat password newline)))))

(defun cmd-su (arg &optional program)
  (cmd-send-shell (concat (or program "su") " " arg))
  (cmd-read-password)
  (setq cmd-just-started t)
  t)


(or (fboundp 'replace-regexp-in-string)

(defun replace-regexp-in-string (regexp rep string &optional
                                        fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
        (start (or start 0))
        matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
        (setq mb (match-beginning 0)
              me (match-end 0))
        ;; If we matched the empty string, make sure we advance by one char
        (when (= me mb) (setq me (min l (1+ mb))))
        ;; Generate a replacement for the matched substring.
        ;; Operate only on the substring to minimize string consing.
        ;; Set up match data for the substring for replacement;
        ;; presumably this is likely to be faster than munging the
        ;; match data directly in Lisp.
        (string-match regexp (setq str (substring string mb me)))
        (setq matches
              (cons (replace-match (if (stringp rep)
                                       rep
                                     (funcall rep (match-string 0 str)))
                                   fixedcase literal str subexp)
                    (cons (substring string start mb) ; unmatched prefix
                          matches)))
        (setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))
)


(defun cmd-fix-slashes (path)
  (if cmd-win32-mode (replace-regexp-in-string "/" "\\\\" path) path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 
;;  default bindings

(global-set-key "\C-z" 'cmd)
(global-set-key "\ez" 'cmd-goto)

