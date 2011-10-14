(if (fboundp 'server-start)
    (progn
      (setq server-use-tcp t)
      (server-start))
  (gnuserv-start))

(defvar arastra-timestamp-format "%Y-%m-%d")
(defvar arastra-signature-format "-%s %s")

(defun arastra-timestamp () 
  (interactive) 
  (insert (format-time-string arastra-timestamp-format)))

(defun arastra-sign ()
  (interactive)
  (insert (format arastra-signature-format
                  (user-real-login-name) 
                  (format-time-string arastra-timestamp-format))))

(defun prefix-region (prefix)
  (interactive "sPrefix: ")
  (save-excursion
    (save-restriction
      (narrow-to-region (mark) (point))
      (goto-char (point-min))
      (insert prefix)
      (while (search-forward "\n" nil t)
        (unless (= (point) (point-max))
          (insert prefix))))))

(setq find-file-compare-truenames t)
(setq find-file-visit-truename t)
(setq truncate-partial-width-windows nil)
(setq inhibit-startup-message t)

;; Avoid those horrible "File %s changed on disk.  Reread from disk?" messages.
(setq revert-without-query '(".*"))
;; And same for "buffer %s has changed on disk.  Really edit?"
(defun ask-user-about-supersession-threat (filename)
  (message "Reverting file %s..." filename)
  (revert-buffer t t)
  (message "Reverting file %s... done" filename))

;; These settings make cut-and-paste actually work:
;; you can "copy" from emacs with M-w and then paste into a gnome app with ^V,
;; or you can copy from a gnome app with ^C and "paste" into emacs with C-y.
;; See http://www.jwz.org/doc/x-cut-and-paste.html 
;; or http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste
(if (featurep 'xemacs)
    (progn
      (setq interprogram-cut-function 'own-clipboard)
      (setq interprogram-paste-function 'get-clipboard))
  (if (equal window-system 'x)
      (progn
        (setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
        (setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
        (setq interprogram-cut-function 'x-select-text)
        (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
        )))

;; This setting makes iswitchb always bring the requested buffer up in
;; the current window.  The default behavior (which simply sets focus
;; into an existing frame that is already showing the buffer) is bad
;; when the existing frame is a gnuclient buffer over a pty over
;; screen over ssh, and you're sitting in front of the X display!  It
;; makes iswitchb just behave brokenly.  -kduda 2005-05-09
(setq iswitchb-default-method 'samewindow)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; for reasons I have not yet determined, this raises an exception on
;;; emacs. -holbrook 2010-02-02
(if (featurep 'xemacs)
    (uniquify-rationalize-file-buffer-names))

(global-set-key '[(control ?c) ?t] 'arastra-timestamp)
(global-set-key '[(control ?c) ?s] 'arastra-sign)

(autoload 'browse-url "browse-url" "Ask a WWW browser to load the URL at or before point." t)
(autoload 'browse-url-at-point "browse-url" "Ask a WWW browser to load the URL at or before point." t)

;; Guess the right mode for perforce tmp files
(defun guess-the-mode ()
  (save-excursion
    (goto-char (point-min))
    (cond ((looking-at "^# A Perforce")
           (progn (text-mode)
                  (setq indent-tabs-mode t)))
          ((looking-at "^#!/usr/bin.*python")
           (python-mode))
          ((looking-at "//\\|/*")
           (c++-mode))
          (t (text-mode)))))

(autoload 'amerge-mode "amerge" "Major mode for processing annotated merge3 files." t)

(setq auto-mode-alist 
      (append (list (cons "\\.h\\'" 'c++-mode) 
                    (cons "\\.tac$" 'c++-mode)
                    (cons "\\.tin$" 'c++-mode)
                    (cons "\\.tiin$" 'c++-mode)
                    (cons "\\.itin$" 'c++-mode)
                    (cons "\\.merge3\\'" 'amerge-mode)
                    (cons "[Mm]akefile" 'makefile-mode)
                    (cons "\\.spec\\.in$" 'rpm-spec-mode)
                    (cons "\\.in$" 'c++-mode)
                    (cons "\\.i$" 'c++-mode)
                    (cons "\\.swig$" 'c++-mode)
                    (cons "\\.hdl$" 'python-mode)
                    (cons "[/^]__init__$" 'python-mode)
                    (cons "\\.html$" 'text-mode) ;; Because html-mode is too weird for me.  -kduda 2005-05-11
                    (cons "tmp.[0-9]*\\.0$" 'guess-the-mode) 
                    ) auto-mode-alist))

; Make that silly graphical progress bar go back into the echo region.
(setq progress-feedback-use-echo-area t)

; Turn on line numbers and column numbers in status bar
(line-number-mode 1)
(column-number-mode 1)

;; Without the below, pdbtrack does not work.
(setq fixed-pdbtrack-regexp "^> \\(.*\\)(\\([0-9]+\\))\\(<?[?a-zA-Z0-9_]+>?\\)()")
(if (featurep 'xemacs)
    (progn
        (require 'python-mode)
        (setq py-pdbtrack-stack-entry-regexp fixed-pdbtrack-regexp))
  (require 'python)
        (setq python-pdbtrack-stack-entry-regexp fixed-pdbtrack-regexp))

;; We modify the below function (lifted from python-mode.el) to search
;; more sensibly for the file containing a function, taking advantage
;; of the fact that we understand our source tree structure.
(defun arastra-py-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having 
having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match py-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-int (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (arastra-grub-for-file filename))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))
                                  ))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun arastra-grub-for-file (filename)
  "Find a file like the given filename somewhere within the local filesystem."
  

  ;; If it looks like an arastra source filename:
  ;;    ~/{what-it-said}/src/...
  ;;    $arasta-root/src/...
  ;;    ~/ar/src/...
  ;; An existing buffer with the same basename.

  (if (string-match "\\([^/]+\\)/src/\\(.*\\)/\\([^/]+\\)$" filename)
      (let* ((arname (substring filename (match-beginning 1) (match-end 1)))
             (dirname (substring filename (match-beginning 2) (match-end 2)))
             (basename (substring filename (match-beginning 3) (match-end 3)))
             (files (list (format "~/%s/src/%s/%s" arname dirname basename)
                          (format "%ssrc/%s/%s" arastra-root dirname basename)
                          (format "~/ar/src/%s/%s" dirname basename)))
             (result nil))
        (while (and files (not result))
          (let ((f (expand-file-name (car files))))
            (if (file-exists-p f)
                (setq result (find-file-noselect f))
              (setq files (cdr files)))))
        (or result
            (get-buffer basename)
            (let ((buffers (buffer-list))
                  buf
                  got)
              (while (and buffers (not got))
                (setq buf (car buffers)
                      buffers (cdr buffers))
                (if (and (save-excursion (set-buffer buf)
                                         (string= major-mode "python-mode"))
                         (string-match (regexp-quote basename) 
                                       (buffer-name buf)))
                    (setq got buf)))
              got)))))

                    
(defun arastra-py-help-at-point ()
  "Get help from Python based on the symbol nearest point.  This is a slightly better 
version of py-help-at-point than what Xemacs ships with."
  (interactive)
  (let* ((sym (py-symbol-near-point))
         (base (substring sym 0 (or (search "." sym :from-end t) 0)))
         cmd)
    (if (not (equal base ""))
        (setq cmd (concat "import " base "\n")))
    (setq cmd (concat "try:\n"
                      (let* ((buf (buffer-name))
                             (base (substring buf
                                              (or (search "/"  buf :from-end t) 0)
                                              (or (search "." buf :from-end t) 0))))
                        (if (length base) (concat "   import " base 
                                                  "\n   from " base " import *\n")
                          "   pass\n"))
                      "except: pass\n"
                      cmd
                      "try: help(" sym ")\n"
                      "except: print 'No help available on:', \"" sym "\""))
    (message cmd)
    (py-execute-string cmd)
    (set-buffer "*Python Output*")
    ;; BAW: Should we really be leaving the output buffer in help-mode?
    (help-mode)))

(defvar arastra-pdbtrack-input-prompt "
\\(?:\\[[^ ]+\\] \\)?[(<]*[p]db[>)]+ "
  "pdbtrack prompt that handles the case where the subprocess name is
printed out before the prompt.  For instance, this is matched:
   [FruSanity001/Simulation] (Pdb)
"
)
(defun arastra-utils-python-mode-hook ()
  ;; Use our tricks for finding python buffers.
  (fset 'py-pdbtrack-get-source-buffer
        (symbol-function 'arastra-py-pdbtrack-get-source-buffer))
  (setq py-pdbtrack-input-prompt
        arastra-pdbtrack-input-prompt)
  ;; A better help
  (fset 'py-help-at-point
        (symbol-function 'arastra-py-help-at-point)))
  
(defun arastra-c-mode-hook ()
  (let* ((bfn (buffer-file-name))
         (style 
          (cond ((and bfn (string-match "/gated-ctk/" bfn))
                 "bsd")
                ((and bfn 
                      (or (string-match "/linux" bfn)
                          (string-match "AroraKernel" bfn)
                          (string-match "EosKernel" bfn)))
                 "linux"))))
    (if style
        (progn (c-set-style style)
               (set-variable 'indent-tabs-mode t)))))

(add-hook 'c-mode-hook 'arastra-c-mode-hook)


(defun arastra-run-command-in-shell (buffer command dir env)
  (let ((buffer (get-buffer buffer))
        (explicit-shell-file-name "/bin/sh")
        (explicit-sh-args (quote ("-c" command)))
        (default-directory dir)
        )
    (and buffer (kill-buffer buffer))
    (let ((process-environment env))
      (shell) 
      (rename-buffer buffer)
      (process-id (get-buffer-process (current-buffer))))))
  

(add-hook 'python-mode-hook 'arastra-utils-python-mode-hook)

(if (fboundp 'server-mode)
    
    ; Emacs version
    (defun arastra-kill-buffer ()
      "Kill buffer, taking gnuclient into account"
      (interactive)
      (if (and (boundp 'server-mode) server-mode)
          (server-edit)
        (call-interactively 'kill-buffer)))

  ; XEmacs version
  (defun arastra-kill-buffer ()
    "Kill buffer, taking gnuclient into account"
    (interactive)
    (if (and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode)
        (gnuserv-edit)
      (call-interactively 'kill-buffer))))

(global-set-key '[(control ?x) ?k] 'arastra-kill-buffer)

(defun make-file-executable ()
 (interactive)
 (set-file-modes (file-truename (buffer-file-name)) 493))
 ;; 493 == 755 octal
