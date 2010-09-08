;;; p4.el --- Perforce-Emacs Integration Library

;; $Id: dev-p4.el,v 10.152 2009/02/05 20:34:13 stewart Exp stewart $

;; Description: Perforce depot manipulation from within Emacs/Xemacs
;; Maintainer: See p4el SourceForge project
;; URL: http://p4el.sourceforge.net/
;; Keywords: version control, source control, VCS, SCM, Perforce, p4

;; LCD Archive Entry:
;; p4|Rajesh Vaidheeswarran|rv@NoSpAm.lOsEtHiS.dsmit.com|
;; P4 SCM Integration into Emacs/XEmacs|
;; 2004/06/11|10.6|not_assigned_yet|

;;; Commentary;

;; Applied the GNU G.P.L. to this file - rv 3/27/1997

;; Copyright (c) 1996-1997 Eric Promislow
;; Copyright (c) 1997-2004 Rajesh Vaidheeswarran
;; Portions copyright (c) 2004 Ron Isaacson
;; Portions copyright (c) 2006-2007 Susquehanna International Group, LLP.
;;
;; Includes Ron Isaacson's 20040625 SourceForge patch entitled, "Changes to
;; async window & error handling."
;;
;; Includes non-attributed 20050201 SourceForge patch entitled, "make p4.el work
;; w/beta releases."

;; This program is free software; you can redistribute it and/or modify it under
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; For a copy of the GNU General Public License, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; This package provides emacs integration with the Perforce version control
;; system, though it does *not* do so within the normal vc package.  Refer to
;; the P4 menu or view the key bindings using the C-x p prefix to discover what
;; functionality is available.
;;
;; The integration works with the command line program p4 which must be
;; configured in the p4-executable variable.  That variable is customizable and
;; its default is based upon introspection of your system.  When not found, the
;; user is prompted for a pathname.  If that fails to produce a valid pathname,
;; then Perforce commands will silently abort.

;; ______________________________________
;; WARNING:
;;
;; Be careful mixing command line and p4.el commands, depending upon how you
;; configure your system, the following might happen:
;;
;;    % p4 edit foo.c
;;     ... Make changes to foo.c in emacs
;;    % p4 submit
;;     ... Keep the writable copy of foo.c in emacs.
;;     ... Start making changes to it.
;;     ... Discover that you can't save it.
;;     ... If you invoke p4-edit, you'll lose your changes.  Instead:
;;    % p4 edit foo.c

;;; Installation

;; Features required by this library:
;;
;; `timer'

;; Features that may be required by this library if used with Xemacs or when
;; accessing select functionality:
;;
;; `dired', `easymenu', `ediff', `browse-url'

;; You should byte compile this file like this:
;;
;;    % emacs -batch -q -f batch-byte-compile /full/path/to/file/p4.el
;;
;; That creates a binary file /full/path/to/file/p4.elc.  Now add
;; /full/path/to/file to your load-path variable in your .emacs file.  Here's
;; one way to do it:
;;
;;    (setq load-path (cons "/full/path/to/file" load-path))
;;
;; Here's another way (inside a top-level progn special form, for example):
;;
;;    (push "/full/path/to/file" load-path)
;;
;; Finally, load the library in your .emacs like this:
;;
;;    (require 'p4)

;; If you have any problems to report, or suggestions, please send them
;; to p4el-bugs@lists.sourceforge.net or use M-x p4-bug-report (C-x p b).

;;; History:

;; The file history can be found in the separate file ChangeLog.

;;; Code:

(require 'comint)
(require 'timer)

(defconst p4-emacs-version-number "11.0"
  "The current P4-Emacs integration revision.")

(defconst p4-emacs-maintainer
  "p4.el maintainers <p4el-bugs@lists.sourceforge.net>"
  "The maintainer(s) of the emacs-p4 integration.  Used for bug reports.")

(defconst p4-web-page "http://p4el.sourceforge.net/" "The home of p4.el.")

;; Determine whether running in Xemacs or GNU Emacs and adapt accordingly
(eval-and-compile
  (defvar p4-running-xemacs nil
    "The current Emacs is XEmacs/Lucid, if non-nil.  Otherwise, GNU Emacs.")

  (when (string-match "XEmacs\\|Lucid" emacs-version)
    (setq p4-running-xemacs t))

  (when p4-running-xemacs
    (require 'dired)                    ; why?
    (require 'easymenu))
  )

;; For compatibility with flavors of Emacs missing these functions
(eval-and-compile
  (defalias 'p4-compose-mail
    (if (fboundp 'compose-mail)
        (function (lambda (to subject)
                    (compose-mail to subject)))
      (function (lambda (to subject)
                  (mail nil to subject)))))

  (defalias 'p4-replace-in-string
    (if (fboundp 'replace-in-string)
        'replace-in-string
      (if (fboundp 'replace-regexp-in-string)
          (function
           (lambda (str regexp newtext &optional literal)
             (funcall (symbol-function 'replace-regexp-in-string)
                      regexp
                      newtext
                      str
                      'fixedcase
                      literal)))
        (function
         (lambda (str regexp newtext &optional literal)
           "Replace all matches in STR of REGEXP with NEWTEXT, returning the new string.
NEWTEXT is a string.
Optional argument LITERAL means do a literal replacement, if non-nil.
Otherwise treat \\ in NEWTEXT string as special:
   \\& means substitute original matched text
   \\N means substitute match for \(...\) number N
   \\\\ means insert one \\."
           ;; Not present in GNU
           ;; (check-argument-type 'stringp str)
           ;; (check-argument-type 'stringp newtext)
           (let ((result "")
                 (start 0)
                 (special)
                 match prev-start)
             (while (setq match (string-match regexp str start))
               (setq prev-start start
                     start (match-end 0)
                     result (concat
                             result
                             (substring str prev-start match)
                             (if literal
                                 newtext
                               (mapconcat
                                (function
                                 (lambda (c)
                                   (if special
                                       (progn
                                         (setq special nil)
                                         (cond ((eq c ?\\)
                                                "\\")
                                               ((eq c ?&)
                                                (substring str
                                                           (match-beginning 0)
                                                           (match-end 0)))
                                               ((and (>= c ?0)
                                                     (<= c ?9))
                                                (if (> c
                                                       (+ ?0
                                                          (length
                                                           (match-data))))
                                                    ;; Invalid match num
                                                    (error
                                                     "Invalid match num: %c"
                                                     c)
                                                  (setq c (- c ?0))
                                                  (substring str
                                                             (match-beginning c)
                                                             (match-end c))))
                                               (t
                                                (char-to-string c))))
                                     (if (eq c ?\\)
                                         (not (setq special t))
                                       (char-to-string c)))))
                                newtext
                                "")))))
             (concat result (substring str start))))))))
  )

(defmacro with-current-buffer-writeable (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current, writeable buffer.
BUFFER is made the current buffer and is made writeable before executing the
forms in BODY.  Then, BUFFER's writeable state is restored.

The value returned is the value of the last form in BODY."
  `(let (read-only)
     (save-current-buffer
       (set-buffer ,buffer)
       (setq read-only buffer-read-only)
       (when read-only
         (toggle-read-only 0))
       (unwind-protect
           ,@body
         (when read-only
           (toggle-read-only 1))))))
;; tell list-indent-function how to indent with-current-buffer-writeable
(put 'with-current-buffer-writeable 'lisp-indent-function 1)

;; Symbols needed at compile time for the following Customize section
(eval-and-compile
  (defvar p4-home (eval (getenv "HOME"))
    "The current user's home directory (caches $HOME).")

  (defvar p4-window-config nil
    "Saved window configuration.  Used as a buffer local variable.")

  (defvar p4-windows-os (memq system-type '(ms-dos windows-nt))
    "Running on a Windows OS, if non-nil.")

  (defun p4-useful-list-p (list)
    "Return LIST if it is a non-empty list.
An empty list is a list with no elements (nil) or with nil for its one element."
    (and list
         (listp list)
         (or (< 1 (length list))
             (car list))
         list))

  (defsubst p4-list-to-string (list)
    "Concatenate the words in LIST into a string with a space between each."
    (mapconcat 'identity list " "))

  (defun p4-as-string (input)
    "Return INPUT as a string or nil if empty."
    (when input
      (cond ((stringp input)
             input)
            ((listp input)
             (p4-list-to-string input))
            (t
             (format "%s" input)))))

  (defun p4-buffer-name (command &optional args-list &rest details)
    "Return a string naming a buffer suitable for p4 COMMAND ARGS-LIST output.
The buffer name depends upon whether ARGS-LIST and details are nil or not as
follows:

   Buffer Name                      ARGS-LIST  DETAILS
   ---------------------------------------------------
   *P4 COMMAND*                     nil        nil
   *P4 COMMAND ARGS-LIST*           non-nil    nil
   *P4 COMMAND: DETAILS*            nil        non-nil
   *P4 COMMAND ARGS-LIST: DETAILS*  non-nil    non-nil"
    (let ((name (concat "*P4 " command)))
      (when args-list
        (setq name (format "%s %s" name (p4-as-string args-list))))
      (when details
        (setq name (format "%s: %s" name (p4-as-string details))))
      (concat name "*")))

  (defun p4-locate-p4-executable ()
    (let ((directories (append exec-path
                               (list (if p4-windows-os
                                         "C:/Program Files/Perforce"
                                       "/usr/local/bin")
                                     (concat p4-home "/bin")
                                     ".")))
          (progname (if p4-windows-os
                        "p4.exe"
                      "p4"))
          result
          pathname)
      (while (and (not result)
                  directories)
        (setq pathname (concat (file-name-as-directory (car directories))
                               progname))
        (and (file-executable-p pathname)
             (not (file-directory-p pathname))
             (setq result pathname))
        (setq directories (cdr directories)))
      result))
  )

;;; Customize

(when (fboundp 'defgroup)              ; no-op when not available
  (defgroup p4 nil "Emacs-P4: Perforce (P4) version control system in Emacs"
    :group 'tools
    :group 'vc)
  (defgroup p4-faces nil "Emacs-P4 faces"
    :group 'p4
    :group 'faces))

(eval-and-compile
  (defmacro p4-defcustom (symbol value docstring &rest args)
    "Macro to transform defcustom to defvar when the former is not defined."
    (if (fboundp 'defcustom)
        `(defcustom ,symbol ,value ,docstring ,@args)
      `(defvar ,symbol ,value ,docstring)))

  (defmacro p4-defface (face spec docstring &rest args)
    "Macro to transform defface to make-face when the former is not defined."
    (if (fboundp 'defface)
        `(defface ,face ,spec ,docstring ,@args)
      ;; if these aren't available, then we need some code to decode spec to
      ;; find the foreground and background colors, if given, and set them via
      ;; set-foreground-color and set-background-color
      `(make-face ,face)
      `(face-spec-set ,face ,spec)))

  (p4-defcustom p4-allow-backups nil
                "Allow Emacs to create backup files of P4 controlled files, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-auto-refresh t
                "Automatically refresh p4 submitted files in buffers, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-bury-invisible-buffers nil
                "Bury invisible Emacs-P4 buffers when popping a window configuration.
When a window configuration is popped off the Emacs-P4 window configuration
stack, all Emacs-P4 buffers will be buried, if non-nil.  If nil, then Emacs-P4
buffers retain their current position in the buffer order, so commands like
`switch-to-buffer' and buffer lists will show them in the usual order.  Set this
non-nil if you prefer Emacs-P4 buffers to get as far out of your way as possible
when you dismiss them.

This option has no effect on `p4-quit-output-buffer'."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-check-empty-diffs t
                "Check for files with empty diffs before submitting, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-colorized-diffs t
                "Enable colorized diffs, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-kill-output-buffer-when-echoing t
                "Delete output buffers when their content is displayed in the echo area.
When an Emacs-P4 command creates an output buffer, the output is often
sufficiently short to be displayed in the echo area (colloquially, in the
minibuffer).  When that happens, the output buffer is killed if this option is
non-nil.  Otherwise, the output buffer is buried."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-follow-symlinks nil
                "Use `file-truename' on all opened files when running p4 commands, if non-nil.
This can be useful to ensure that the Root or AltRoots in your client
specification agree with the pathnames in Emacs."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-notify nil
                "Notify users of any Emacs-P4 change submitted if non-nil.
The notification list is set by `p4-set-p4notify'."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-strict-client-completion t
                "Require that `p4-set-p4client' accept only existing clients, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-use-p4config-exclusively nil
                "Indicates how to check whether a file is under P4 control.
`p4-check-mode' is always called, if nil.  Otherwise, check to see if the file
named by the P4CONFIG environment variable exists in the current or a parent
directory and, if so, run `p4-check-mode'.

Setting this to non-nil makes `p4-find-file-hook' much faster in non-P4
directories, but only works if you always create a configuration file where you
want to use P4."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-default-depot-completion-prefix "//depot/"
                "Prefix to be used for completion prompt when prompting user for a depot file."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-sendmail-program (if (boundp 'sendmail-program)
                                        sendmail-program
                                      nil)
                "The sendmail program.  To set this use `customize'."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-user-email (if (boundp 'user-mail-address)
                                  user-mail-address
                                nil)
                "E-mail address of the current user, used by the notification system.
This must be set if notification should occur."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-default-port "perforce:1666"
                "The P4PORT environment variable default.
If the e-var P4PORT is not set when Emacs-P4 loads, the value of
`p4-default-port' is used to set P4PORT."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-default-annotate-options "-c -i -dw"
                "The default options to pass to p4 annotate."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-default-changes-options "-m 200"
                "The default options to pass to p4 changes."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-default-log-options "-li"
                "The default options to pass to p4 changes and p4 filelog."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-default-describe-options "-du"
                "The default options to pass to p4 describe.
p4 describe only recognizes one -d option, but you can combine the suboptions,
so -dub means a unified diff that ignores whitespace changes."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-executable (p4-locate-p4-executable)
                "The pathname to the p4 executable.
This can be set interactively using the function `p4-set-p4-executable' and
with `customize'"
                :type 'string
                :group 'p4)

  (p4-defcustom p4-exec-timeout 90
                "The number of seconds to wait for a p4 command to execute.
This is useful when accessing a Perforce depot remotely or when other factors
lead to commands failing to execute."
                :type 'integer
                :group 'p4)

  (p4-defcustom p4-password-file ""
                "File containing the password used to log into P4.
When `p4-login' tries to log into P4, it tries several means to find a password.
The contents of this file is among them.

You probably want to set the mode on this file to 0600."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-use-cygpath t
                "Use cygpath to convert a P4 pathname to one recognized by cygwin systems."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-cygpath-exec "cygpath"
                "Pathname of cygpath binary on cygwin systems."
                :type 'string
                :group 'p4)

  (p4-defcustom p4-clean-caches t
                "Clean up the branches/clients/dirs/labels caches periodically, if non-nil."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-clean-caches-period 600
                "The number of seconds after which `p4-cache-cleanup' will check for dirty caches."
                :type 'integer
                :group 'p4)

  (p4-defcustom p4-hook-find-file t
                "If non-nil, the `p4-find-file-hook' will run when opening files."
                :type 'boolean
                :group 'p4)

  (p4-defcustom p4-mode-hook nil
                "Hook run by `p4-mode'."
                :type 'sexp
                :group 'p4)

  (p4-defcustom p4-command-line-length 20000
                "Maximum p4 command line length.
The total number of characters permitted on the command line when invoking p4
commands."
                :type 'integer
                :group 'p4)

  (p4-defcustom p4-file-refresh-timer-period 60
                "Period for checking for modified files.
The number of seconds after which Emacs-P4 will check for modified files in the
buffers.  Set to 0 to disable periodic refreshing."
                :type 'integer
                :group 'p4)

  (p4-defcustom p4-default-diff-options "-du"
                "The default options to pass to p4 diff.
p4 diff only recognizes one -d option, but you can combine the suboptions,
so -dub means a unified diff that ignores whitespace changes."
                :type 'string
                :group 'p4-faces)

  (p4-defcustom p4-default-diff2-options "-du"
                "The default options to pass to p4 diff2.
p4 diff2 only recognizes one -d option, but you can combine the suboptions,
so -dub means a unified diff that ignores whitespace changes."
                :type 'string
                :group 'p4-faces)

  (p4-defcustom p4-not-in-depot-files "* *.*"
                "The files `p4-not-in-depot' should check.
Set this value to file globbing patterns appropriate for your shell to find the
files that `p4-not-in-depot' should check for in the depot."
                :type 'string
                :group 'p4-faces)

  (p4-defface p4-diff-file-face '((t (:background "gray90")))
              "Face used for file pathnames in difference buffers."
              :group 'p4-faces)

  (p4-defface p4-diff-head-face '((t (:background "gray95")))
              "Face used for ?"
              :group 'p4-faces)

  (p4-defface p4-diff-inserted-face '((t (:foreground "blue")))
              "Face used for new (inserted) text in difference buffers.
When the newer revision contains text not in the older revision, that text will
be marked with this face."
              :group 'p4-faces)

  (p4-defface p4-diff-deleted-face '((t (:foreground "red")))
              "Face used for old (deleted) text in difference buffers.
When the older revision contains text not in the newer revision, that text will
be marked with this face."
              :group 'p4-faces)

  (p4-defface p4-diff-changed-face '((t (:foreground "dark green")))
              "Face used for changed text in difference buffers.
When a section of text is in both the newer and older revision, but differs
between them, that text will be marked with this face."
              :group 'p4-faces)

  (p4-defface p4-depot-branched-face '((t (:foreground "blue4")))
              "Face used for branched files."
              :group 'p4-faces)

  (p4-defface p4-depot-added-face '((t (:foreground "blue")))
              "Face used for files added to the depot."
              :group 'p4-faces)

  (p4-defface p4-depot-deleted-face '((t (:foreground "red")))
              "Face used for files deleted from the depot."
              :group 'p4-faces)

  (p4-defface p4-depot-unmapped-face '((t (:foreground "grey30")))
              "Face used for files not mapped to the depot."
              :group 'p4-faces)

  (p4-defface p4-form-label-face '((t (:weight bold :style released-button)))
              "Face used for p4 form section labels.
This face is used to mark the section labels in forms for p4 client, p4 submit,
etc."
              :group 'p4-faces)

  (p4-defface p4-form-error-face '((t (:foreground "red")))
              "Face used for p4 error messages when form submission fails.
This face is used to mark p4 error messages when p4 client, p4 submit, etc.
fails."
              :group 'p4-faces)

  (p4-defface p4-opened-not-changed-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"is opened and not being changed.\""
              :group 'p4-faces)

  (p4-defface p4-open-for-edit-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"is opened for edit and can't be replaced.\""
              :group 'p4-faces)

  (p4-defface p4-permission-denied-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"Permission denied.\""
              :group 'p4-faces)

  (p4-defface p4-must-resolve-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"must resolve X before submitting.\""
              :group 'p4-faces)

  (p4-defface p4-must-sync-before-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"must sync before X.\""
              :group 'p4-faces)

  (p4-defface p4-cannot-clobber-face '((t (:foreground "red")))
              "Face used for select lines in p4 command output.
Applied to lines ending with \"Can't clobber writable file.\""
              :group 'p4-faces)
  )
;; End Customize

;; Variables
(defmacro defp4localvar (symbol initial docs)
  "Create a permanent buffer local variable SYMBOL.
INITIAL is the initial value of SYMBOL.
DOCS is the docstring for SYMBOL."
  `(eval-and-compile
     (defvar ,symbol ,initial ,docs)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(defvar p4-all-p4-buffer-files nil
  "The list of all buffers and their files under p4 version control.
This enables auto-refreshing buffers of p4 submitted files.")

(defvar p4-async-command-hook nil
  "The hook run after `p4-call-form-command' sets up an async buffer.")

(defvar p4-async-minor-mode nil
  "The minor mode for editing p4 asynchronous command buffers.")
(make-variable-buffer-local 'p4-async-minor-mode)

(or (assoc 'p4-async-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(p4-async-minor-mode " P4") minor-mode-alist)))
(or (assoc 'p4-async-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons '(p4-async-minor-mode . p4-async-minor-map)
                minor-mode-map-alist)))

(defvar p4-change-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defp4localvar p4-current-args nil
  "Arguments used for the latest command invoked on the current buffer.")

(defp4localvar p4-form-command nil
  "The command run to produce the current form buffer.")

(defp4localvar p4-error-extent nil
  "The portion of the current form buffer that is a p4 error message.")

(defvar p4-enabled t
  "A flag indicating whether P4 commands are enabled.
This is set with `p4-enable' or the 'Enable Emacs-P4 Commands' menu option and
cleared with `p4-disable' or the 'Disable Emacs-P4 Commands' menu option.")

(defvar p4-file-refresh-timer nil
  "The timer that will be set to refresh buffers after a submit.
Files modified by `p4-submit' will be refreshed when this timer fires.")

(defp4localvar p4-output-buffer nil
  "The most recent Emacs-P4 output buffer created from the current buffer.")

(defp4localvar p4-mode nil
  "The current buffer's file is under P4 control, if non-nil.")

(defvar p4-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'p4-toggle-read-only)
    map)
  "Keymap for p4 minor mode.")
(fset 'p4-minor-map p4-minor-map)
(or (assoc 'p4-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-mode p4-mode) minor-mode-alist)))
(or (assoc 'p4-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons '(p4-mode . p4-minor-map) minor-mode-map-alist)))
(or (assoc 'p4-offline-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(p4-offline-mode p4-offline-mode) minor-mode-alist)))
(or (assoc 'p4-offline-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons '(p4-offline-mode . p4-minor-map) minor-mode-map-alist)))

(defvar p4-my-clients nil
  "List of p4 clients that the function `p4-set-p4client' can complete on.
Set this variable *only* if you don't want Emacs-P4 to complete on all the
clients in the P4 server.

You can set this list using the function `p4-set-my-clients'.")

(defvar p4-notify-list (getenv "P4NOTIFY") "The Emacs-P4 notify list.")

(defp4localvar p4-offline-mode nil
  "The current buffer's file is under offline mode P4 control, if non-nil.")

(eval-and-compile
  (defvar p4-default-output-buffer-name (p4-buffer-name "output")
    "P4 output buffer name."))

(defp4localvar p4-revision nil
  "P4 revision number of the current buffer's file, if any.")

(defvar p4-server-version nil "Cache for the server version string.")

(defvar p4-timer nil
  "The timer object that will be set to clean up the caches periodically.")

(defvar p4-output-buffers nil "Stack of Emacs-P4 output buffers.")
;; End variables

;; Key maps
(eval-and-compile
  (defun p4-add-mouse-keys (map)
    "Add mouse button handlers to MAP."
    (cond (p4-running-xemacs
           (define-key map [button2] 'p4-buffer-on-mouse-button-2)
           (define-key map [button3] 'p4-buffer-on-mouse-button-3))
          (t
           (define-key map [mouse-2] 'p4-buffer-on-mouse-button-2)
           (define-key map [mouse-3] 'p4-buffer-on-mouse-button-3))))
  )

(defconst p4-async-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'p4-commit-form-changes)
    (define-key map "\C-c\C-k" 'p4-kill-form-buffer)
    (define-key map "\C-x k" 'p4-kill-form-buffer)
    (p4-add-mouse-keys map)
    map)
  "Keymap for p4 async minor mode.")

(defun p4-make-derived-map (base-map)
  (let (map)
    (if (not p4-running-xemacs)
        (setq map (cons 'keymap base-map))
      (setq map (make-sparse-keymap))
      (funcall (symbol-function 'set-keymap-parents) map (list base-map)))
    map))

(defconst p4-basic-map
  (let ((map (make-sparse-keymap)))
    (p4-add-mouse-keys map)
    (define-key map [return]    'p4-buffer-on-enter)
    (define-key map "q"         'p4-quit-and-bury-current-buffer)
    (define-key map "Q"         'p4-quit-and-kill-current-buffer)
    (define-key map "k"         'p4-scroll-down-1-line)
    (define-key map "j"         'p4-scroll-up-1-line)
    (define-key map "b"         'p4-scroll-down-1-window)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "         'p4-scroll-up-1-window)
    (define-key map "<"         'p4-top-of-buffer)
    (define-key map ">"         'p4-bottom-of-buffer)
    (define-key map "="         'p4-delete-other-windows)
    map))

(defconst p4-diff-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n" 'p4-goto-next-diff)
    (define-key map "p" 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d" 'p4-next-depot-diff)
    (define-key map "u" 'p4-prev-depot-diff)
    map))

(defconst p4-change-log-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "d"         'p4-action-diff2)
    (define-key map "f"         'p4-action-find-file-other-window)
    (define-key map "s"         'p4-action-filelog-short-format)
    (define-key map "l"         'p4-action-filelog-long-format)
    (define-key map "\C-k"      'p4-scroll-down-1-line-other-w)
    (define-key map "\C-j"      'p4-scroll-up-1-line-other-w)
    (define-key map "b"         'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "         'p4-scroll-up-1-window-other-w)
    (define-key map "<"         'p4-top-of-buffer-other-w)
    (define-key map ">"         'p4-bottom-of-buffer-other-w)
    (define-key map "="         'p4-delete-other-windows)
    (define-key map "n"         'p4-goto-next-change)
    (define-key map "p"         'p4-goto-prev-change)
    (define-key map "N"         (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defconst p4-opened-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n" 'p4-next-depot-file)
    (define-key map "p" 'p4-prev-depot-file)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting opened files.")

;; We use the C-x p Keymap for all perforce commands
(defconst p4-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'p4-add)
    (define-key map "b" 'p4-bug-report)
    (define-key map "B" 'p4-branch)
    (define-key map "c" 'p4-client)
    (define-key map "C" 'p4-changes)
    (define-key map "d" 'p4-diff2)
    (define-key map "D" 'p4-describe)
    (define-key map "e" 'p4-edit)
    (define-key map "E" 'p4-reopen)
    (define-key map "f" 'p4-filelog)
    (define-key map "F" 'p4-files)
    (define-key map "\C-f" 'p4-depot-find-file)
    (define-key map "g" 'p4-display-p4client)
    (define-key map "G" 'p4-get)
    (define-key map "h" 'p4-help)
    (define-key map "H" 'p4-have)
    (define-key map "i" 'p4-info)
    (define-key map "I" 'p4-integrate)
    (define-key map "j" 'p4-job)
    (define-key map "J" 'p4-jobs)
    (define-key map "l" 'p4-label)
    (define-key map "L" 'p4-labels)
    (define-key map "\C-l" 'p4-labelsync)
    (define-key map "m" 'p4-rename)
    (define-key map "n" 'p4-notify)
    (define-key map "o" 'p4-opened)
    (define-key map "p" 'p4-print)
    (define-key map "P" 'p4-set-p4port)
    (define-key map "q" 'p4-quit-and-bury-output-buffer)
    (define-key map "Q" 'p4-quit-and-kill-output-buffer)
    (define-key map "r" 'p4-revert)
    (define-key map "R" 'p4-refresh)
    (define-key map "\C-r" 'p4-resolve)
    (define-key map "s" 'p4-set-p4client)
    (define-key map "S" 'p4-submit)
    (define-key map "t" 'p4-toggle-hook-find-file)
    (define-key map "u" 'p4-user)
    (define-key map "U" 'p4-users)
    (define-key map "v" 'p4-emacs-version)
    (define-key map "V" 'p4-blame)
    (define-key map "w" 'p4-where)
    (define-key map "x" 'p4-delete)
    (define-key map "X" 'p4-fix)
    (define-key map "=" 'p4-diff)
    (define-key map "-" 'p4-ediff)
    (define-key map "~" 'p4-print-revision)
    (define-key map "?" 'p4-describe-bindings)
    map)
  "The prefix for Emacs-P4 library commands.")
(or (keymapp (lookup-key global-map "\C-xp"))
    (define-key global-map "\C-xp" p4-prefix-map))

(defconst p4-print-rev-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"  'p4-next-change-rev-line)
    (define-key map "p"  'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "l"  'p4-toggle-line-wrap)
    map)
  "The key map to use for browsing print-revs buffers.")
;; End key maps

;; Constants
(defconst p4-begin-form-error "##### Perforce Error Message")
(defconst p4-end-form-error "##### End Error Message")

(defconst p4-change-history-change-regex
  (concat "^\\.\\.\\. #"     "\\([0-9]+\\)" ; revision
          "\\s-+change\\s-+" "\\([0-9]+\\)" ; change
          "\\s-+"            "\\([^ \t]+\\)" ; type
          "\\s-+on\\s-+"     "\\([^ \t]+\\)" ; date
          "\\s-+by\\s-+"     "\\([^ \t]+\\)" ; author
          "@"))

(defconst p4-change-history-index-regex
  (concat " *\\([0-9]+\\)"              ; change
          " *\\([0-9]+\\)"              ; revision
          " *\\([0-9]+/[0-9]+/[0-9]+\\)" ; date
          "\\s-+\\([^:]*\\)"            ; author
          ":"))

(defconst p4-change-history-revision-regex
  (concat "^\\([0-9]+\\),?"
          "\\([0-9]*\\)"
          "\\([acd]\\)"
          "\\([0-9]+\\),?"
          "\\([0-9]*\\)"))

(defconst p4-pathname-with-revision-plus-
  "\\(^\\)\\(//[^/@# ][^/@#]*/[^@#]+\\)#[0-9]+ - ")

(defconst p4-signal-names
  (list "Unknown signal 0"
        "Hangup"
        "Interrupt"
        "Quit"
        "Illegal instruction"
        "Trace/breakpoint trap"
        "Aborted"
        "Bus error"
        "Floating point exception"
        "Killed"
        "User defined signal 1"
        "Segmentation fault"
        "User defined signal 2"
        "Broken pipe"
        "Alarm clock"
        "Terminated"
        "Stack fault"
        "Child exited"
        "Continued"
        "Stopped (signal)"
        "Stopped"
        "Stopped (tty input)"
        "Stopped (tty output)"
        "Urgent I/O condition"
        "CPU time limit exceeded"
        "File size limit exceeded"
        "Virtual timer expired"
        "Profiling timer expired"
        "Window changed"
        "I/O possible"
        "Power failure"
        "Bad system call"
        "Unknown signal 32"
        "Unknown signal 33"
        "Real-time signal 0"
        "Real-time signal 1"
        "Real-time signal 2"
        "Real-time signal 3"
        "Real-time signal 4"
        "Real-time signal 5"
        "Real-time signal 6"
        "Real-time signal 7"
        "Real-time signal 8"
        "Real-time signal 9"
        "Real-time signal 10"
        "Real-time signal 11"
        "Real-time signal 12"
        "Real-time signal 13"
        "Real-time signal 14"
        "Real-time signal 15"
        "Real-time signal 16"
        "Real-time signal 17"
        "Real-time signal 18"
        "Real-time signal 19"
        "Real-time signal 20"
        "Real-time signal 21"
        "Real-time signal 22"
        "Real-time signal 23"
        "Real-time signal 24"
        "Real-time signal 25"
        "Real-time signal 26"
        "Real-time signal 27"
        "Real-time signal 28"
        "Real-time signal 29"
        "Real-time signal 30")
  "The proper names for the various signals that `p4-call-p4' can return.

This list was generated on a Linux box by calling strsignal() in a loop.  It is,
hopefully, applicable to other systems, too.  All that is expected is that this
list matches what `call-process' would return for the same signals.")

(defconst p4-AUTHOR 2)                  ; author
(defconst p4-DATE 1)
(defconst p4-FILE 3)
(defconst p4-REVISION  0)               ; revision
(defconst p4-SIZE 7)                    ; file-revision size element
;; End constants

;; Dired-specific functions
(eval-and-compile
  (defsubst p4-dired-p ()
    "Return non-nil if the current buffer is a Dired buffer."
    (eq major-mode 'dired-mode))
  )

(defun p4-get-dired-filename ()
  "Return Dired's notion of the current filename, if any."
  (when (fboundp 'dired-get-filename)
    (condition-case nil
        (p4-fix-dired-filename
         (funcall (symbol-function 'dired-get-filename) nil t))
      (error nil))))

(defun p4-fix-dired-filename (filename)
  (when filename
    (when (string-match "^~/\\(.+\\)$" filename)
      (setq filename (concat p4-home
                             "/"
                             (substring filename (match-beginning 1)))))
    (when filename
      (setq filename (p4-follow-link-name filename))))
  filename)

(defun p4-get-dired-marked-files ()
  "Return the files marked in the current buffer if it is a Dired buffer.
If there are no marked files, use the file on the Dired buffer line containing
point.  If there is no file on the current line, return nil."
  (and (p4-dired-p)
       ;; make sure this Dired provides the expected function
       (fboundp 'dired-get-marked-files)
       (condition-case nil
           (let ((files (funcall (symbol-function 'dired-get-marked-files)))
                 result)
             (while files
               (setq result (append result
                                    (list (p4-fix-dired-filename (car files))))
                     files (cdr files)))
             result)
         (error nil))))                 ; no filename

;; Viper-specific functions
(defun p4-viper-current-state ()
  "Return `viper-current-state' if set."
  (when (boundp 'viper-current-state)
    (symbol-value 'viper-current-state)))

(defun p4-exit-viper-edit-state ()
  "Exit viper edit states, if active."
  (let ((state (p4-viper-current-state))
        function)
    (when state
      (cond ((eq state 'insert-state)
             (call-interactively 'viper-exit-insert-state))
            ((eq state 'replace-state)
             (call-interactively 'viper-replace-state-exit-cmd))))))

;; GNU Emacs/XEmacs compatibility
(eval-and-compile
  (defalias 'p4-create-extent
    (if p4-running-xemacs
        (function (lambda (start end)
                    (funcall (symbol-function 'make-extent) start end)))
      (function (lambda (start end)
                  (funcall (symbol-function 'make-overlay) start end)))))

  (defalias 'p4-extents-at
    (if p4-running-xemacs
        'extents-at
      'overlays-at))

  (defalias 'p4-extent-properties
    (if p4-running-xemacs
        'extent-properties
      'overlay-properties))

  (defalias 'p4-set-extent-property
    (if p4-running-xemacs
        'set-extent-property
      'overlay-put))

  (defalias 'p4-set-extent-properties
    (if p4-running-xemacs
        'set-extent-properties
      (function (lambda (extent plist)
                  "Set EXTENT properties to those given in PLIST.
EXTENT is actually an overlay."
                  (while plist
                    (funcall (symbol-function 'overlay-put)
                             extent
                             (car plist)
                             (cadr plist))
                    (setq plist (cddr plist)))))))

  (defalias 'p4-get-extent-start
    (if p4-running-xemacs
        'extent-start-position
      'overlay-start))

  (defalias 'p4-get-extent-end
    (if p4-running-xemacs
        'extent-end-position
      'overlay-end))

  (defalias 'p4-max-echo-area-lines
    (if p4-running-xemacs
        (function (lambda ()
                    "Return 1 as the maximum number of lines which may be shown in the echo area."
                    1))
      (function (lambda ()
                  "Return the maximum number of lines which may be shown in the echo area."
                  ;; from GNU Emacs' display-message-or-buffer in simple.el
                  (if (symbol-value 'resize-mini-windows)
                      (let ((max (symbol-value 'max-mini-window-height)))
                        (cond ((floatp max)
                               (* (frame-height) max))
                              ((integerp max)
                               max)
                              (t
                               1)))
                    1)))))

  (defalias 'p4-event-window
    (if p4-running-xemacs
        (function (lambda (event)
                    "Return the window in EVENT."
                    (funcall (symbol-function 'event-window) event)))
      (function (lambda (event)
                  "Return the window in EVENT."
                  (funcall (symbol-function 'posn-window)
                           (funcall (symbol-function 'event-end) event))))))

  (defalias 'p4-event-point
    (if p4-running-xemacs
        (function (lambda (event)
                    "Return the buffer location in EVENT."
                    (funcall (symbol-function 'event-point) event)))
      (function (lambda (event)
                  "Return the buffer location in EVENT."
                  (funcall (symbol-function 'posn-point)
                           (funcall (symbol-function 'event-start) event))))))

  (defalias 'p4-force-mode-line-update
    (if p4-running-xemacs
        (function redraw-modeline)
      (function force-mode-line-update)))
  )

(defun p4-create-timer (function period &optional repeat)
  "Create a timer that calls FUNCTION in PERIOD seconds.
REPEAT means to call FUNCTION *every* PERIOD seconds, if non-nil."
  (let ((again (when repeat period)))
    (if p4-running-xemacs
        (add-timeout period function nil again)
      (run-at-time period again function))))

(defun p4-cancel-timer (timer)
  "Cancel TIMER.
TIMER must have been created with function `p4-create-timer'."
  (if p4-running-xemacs
      (when (integerp timer)
        (disable-timeout timer))
    (when (timerp timer)
      (cancel-timer timer))))

;; End GNU Emacs/XEmacs compatibility

;; All other functions start here

(defun p4-kill-buffer-hook ()
  "Remove a file and its associated buffer from the list of P4 controlled files."
  (if p4-revision
      (p4-forget-p4-file-and-buffer (p4-buffer-file-name) (current-buffer))
    ;; Ediff seems to remove all buffer local variables or otherwise manages
    ;; to remove the buffer local variable p4-window-config which would be a
    ;; good flag to find output buffers that might actually need to be removed
    ;; from the stack; since that doesn't work, try to remove all killed to
    ;; ensure the Emacs-P4 output buffers are removed
    (p4-remove-output-buffer (current-buffer))))
(add-hook 'kill-buffer-hook 'p4-kill-buffer-hook)

(defun p4-disable ()
  "Disable Emacs-P4 commands."
  (interactive)
  (p4-set-enabled nil))

(defun p4-enable ()
  "Enable Emacs-P4 commands."
  (interactive)
  (p4-set-enabled t))

(defun p4-toggle-enabled ()
  "Toggle the value of `p4-enabled'."
  (interactive)
  (setq p4-enabled (not p4-enabled)))

(defun p4-set-enabled (enabled)
  "Set `p4-enabled' to ENABLED if not already."
  (let ((was-enabled p4-enabled))
    (setq p4-enabled enabled)
    (message "Emacs-P4 commands %s %sabled."
             (if (eq was-enabled enabled)
                 "already were"
               "are")
             (if enabled
                 "en"
               "dis"))))

(defun p4-available-p ()
  "Indicate whether p4 commands are available.
Return nil if `p4-enabled' is nil.
Otherwise, return nil if `p4-executable' is nil and function
   `p4-set-p4-executable' fails.
Otherwise, consider `p4-use-p4config-exclusively'.
If it is non-nil, return nil if there is no configuration file available.
Otherwise, if it is nil, return non-nil."
  (and p4-enabled
       (or p4-executable
           (call-interactively 'p4-set-p4-executable))
       (or (not p4-use-p4config-exclusively)
           (p4-config-file))))

;;; Functions needed at compile time to build the docstrings for p4 commands
;;; defined with the defp4cmd macro (and at runtime for normal functioning)
(eval-and-compile
  (defsubst p4-alert (string &rest args)
    "Beep or flash the screen and display a message.
STRING and ARGS as for `message'."
    (ding)
    (apply 'message string args))

  (defun p4-buffer-string ()
    "Return the text in the current buffer minus any trailing newlines."
    (let ((text (buffer-string)))
      (while (string= "\n" (substring text -1))
        (setq text (substring text 0 -1)))
      text))

  (defun p4-menu-add ()
    "Add P4 menu bar button for files not in the depot or current client view."
    (interactive)
    (when p4-running-xemacs
      (when (boundp 'p4-mode)
        (setq p4-mode nil))
      (funcall (symbol-function 'easy-menu-add)
               (cons "P4" (symbol-value 'p4-menu-def)))))

  (defsubst p4-re-search (regexp)
    "Find the first match for REGEXP in the current buffer after point.
Return nil if no match."
    (re-search-forward regexp nil 'noerror))

  (defun p4-count-lines (&optional buffer)
    "Return the number of lines in BUFFER.
Ignores blank lines at the top and bottom of BUFFER.

BUFFER is the buffer of lines to count, if non-nil.  Use the current buffer, if
   nil."
    (if (zerop (buffer-size buffer))
        0
      (save-excursion
        (when buffer
          (set-buffer buffer))
        (goto-char (point-min))
        (while (looking-at "^$")
          (forward-line))
        (let ((start (point)))
          (goto-char (point-max))
          (beginning-of-line)
          (while (looking-at "^$")
            (beginning-of-line 0))
          (end-of-line)
          (count-lines start (point))))))

  (defun p4-depot-output (command &optional no-error &rest args)
    "Execute p4 COMMAND ARGS inside the current buffer.
NO-ERROR means ignore errors if the command fails, if non-nil.  Otherwise,
   signal an error if the command fails.

Return non-nil if the command succeeds.
Return nil if the command fails and NO-ERROR is non-nil.

Throws 'p4-executable-missing' to `p4-executable-missing if `p4-executable' is
nil."
    (let ((status (apply 'p4-exec-p4 command t args)))
      (if (= status 0)
          t
        (unless no-error
          (apply 'p4-report-error status t command args))
        nil)))

  (defun p4-disable-hilit ()
    "Disable the old hilit highlighting since we use font-lock."
    (and (not p4-running-xemacs)
         (boundp 'hilit-auto-rehighlight)
         (set 'hilit-auto-rehighlight nil)))

  (defun p4-executable-p ()
    "Indicate whether the p4 executable is available.
If p4 is available, return non-nil.
Otherwise, print informational message and return nil."
    (if p4-executable
        t
      (message "p4 executable not found; set `p4-executable' or run p4-set-p4-executable")
      nil))

  (defun p4-make-output-buffer (&optional buffer)
    "Make an Emacs-P4 output buffer from BUFFER.
The current window configuration is saved in a buffer local variable of the
output buffer and the output buffer is added to the Emacs-P4 output buffer
stack, to support the `p4-quit-and-*' functions.

BUFFER is a live buffer, the name of existing buffer, or the name to use for a
   new buffer, if non-nil.  If BUFFER is nil (or was omitted), then
   `p4-default-output-buffer-name' is used."
    (let* ((buffer (or buffer
                       p4-default-output-buffer-name))
           (frame (selected-frame))
           (config (current-window-configuration frame))
           (result (get-buffer buffer)))
      (if (not result)
          (setq result (get-buffer-create buffer))
        (with-current-buffer result
          (toggle-read-only 0)
          (erase-buffer)
          (kill-all-local-variables))
        (buffer-disable-undo result)
        (buffer-enable-undo result)
        (p4-remove-output-buffer result))
      (p4-push-output-buffer result)
      (with-current-buffer result
        (set (make-local-variable 'p4-window-config) config))
      result))

  (defun p4-get-buffer (&optional buffer)
    "Return the buffer identified by BUFFER.
BUFFER may be a buffer, the name of a buffer, t, 0, nil, or omitted.

Return current buffer if BUFFER is t.
Otherwise, return BUFFER if BUFFER is a live buffer.
Otherwise, return the buffer named BUFFER if BUFFER is a string.
Otherwise, return zero if BUFFER is zero.
Otherwise, return the default Emacs-P4 output buffer, if BUFFER is nil.
Otherwise, return nil."
    (cond ((eq 't buffer)
           (current-buffer))
          ((not buffer)
           (p4-make-output-buffer))
          ((or (buffer-live-p buffer)
               (and (numberp buffer)
                    (= 0 buffer)))
           buffer)
          ((stringp buffer)
           (get-buffer buffer))
          (t
           nil)))

  (defun p4-get-p4-executable ()
    "Return the p4 executable, if available.
If not found, throw `p4-executable-missing to `p4-executable-missing."
    (if (p4-executable-p)
        p4-executable
      (throw 'p4-executable-missing 'p4-executable-missing)))

  (defun p4-report-error (status buffer command &rest args)
    "Report an error if STATUS indicates p4 COMMAND ARGS failed or was signaled.
STATUS is the status code returned by `call-process' or similar.  A string is
   considered a signal name.
BUFFER contains the output of the command that may be extracted to display at
   the bottom of the screen or may be brought to the fore.  Use the current
   buffer if BUFFER is t."
    (apply 'p4-report-signal status command args)
    (apply 'p4-report-failure status buffer command args))

  (defun p4-build-command-string (command &rest args)
    "Build a string representing p4 COMMAND ARGS.
ARGS is the set of arguments given to p4 COMMAND, if non-nil."
    (concat "p4 "
            command
            (when args
              (concat " \""
                      (mapconcat 'identity args "\" \"")
                      "\""))))

  (defun p4-report-failure (status buffer command &rest args)
    "Report an error if STATUS indicates that p4 COMMAND ARGS failed.
STATUS is the status code returned by `call-process' or similar.  Only numeric
   values are considered.
BUFFER contains the output of the command that may be extracted to display at
   the bottom of the screen or may be brought to the fore.  Use the current
   buffer if BUFFER is t."
    (and (numberp status)
         (not (zerop status))
         (let* ((arguments (apply 'p4-build-command-string command args))
                (buffer (p4-get-buffer buffer))
                (msg (format "%s failed with exit status %s" arguments status))
                text)
           (with-current-buffer buffer
             (when (= 1 (p4-count-lines))
               (setq text (p4-buffer-string)))
             (if (not text)
                 (display-buffer buffer t)
               (kill-buffer buffer)
               (setq msg (concat msg ": " text))))
           (error msg))))

  (defun p4-report-signal (status command &rest args)
    "Report an error if STATUS indicates p4 COMMAND ARGS received a signal.
STATUS is expected to be a string iff it represents a process received a signal.
Report no error for SIGQUIT or SIGTERM."
    (when (and (stringp status)
               (or (string= "Quit" status)
                   (string= "Terminated" status)))
      (error "Failed with signal \"%s\": %s"
             status
             (apply 'p4-build-command-string command args))))

  (defun p4-check-for-command-output (command buffer &rest args)
    "Check BUFFER for some content or signal an error."
    (when (or (not (buffer-live-p buffer))
              (= (point-min) (point-max)))
      (let ((command (apply 'p4-build-command-string command args)))
        (p4-alert "Missing output from " command))))

  (defun p4-exec-p4 (command buffer &rest args)
    "Run p4 COMMAND ARGS synchronously in a separate process with output to BUFFER.
This is the normal interface for invoking p4.  Run p4 COMMAND ARGS in the
current buffer's default directory.  Add the P4 menu to the buffer described by
BUFFER.

BUFFER as described for function `p4-get-buffer'.

Return result as described for function `p4-call-p4'.

If `p4-executable' is nil, throw `p4-executable-missing to
`p4-executable-missing."
    (save-excursion
      (let ((status (apply 'p4-call-p4 command buffer t args)))
        (p4-menu-add)
        (p4-disable-hilit)              ; we use font-lock
        status)))

(defun p4-async-call-p4 (command buffer &rest args)
  "Call p4 COMMAND ARGS asynchronously in a separate process.
This is a low level interface that calls p4 COMMAND ARGS and captures the output
as described by BUFFER.  It also detects non-quit signals received by the p4
process.  If the command takes longer than `p4-exec-timeout' seconds, the call
fails.

Use function `p4-exec-p4' if you want the P4 menu added to the buffer.

The program's input comes from /dev/null.

COMMAND is the p4 command to run.
BUFFER as described for function `start-process', except that nil means to use
   the default Emacs-P4 output buffer and 0 means to return immediately with
   nil.
ARGS are passed to p4 COMMAND.

If there's an output buffer, it is saved in `p4-output-buffer' in the current
buffer.

Return nil if BUFFER is 0.
Otherwise, return 'timeout if the process takes more than `p4-exec-timeout'
   seconds to finish.
Otherwise, return nil if the process receives a quit signal (see
   `p4-quit-signal-p').
Otherwise, signal an error if the process receives a signal.
Otherwise, return a numeric exit code from p4 COMMAND ARGS."
  ;; must manage buffer according to the needs of start-process rather than
  ;; call-process
  (let* ((process (apply 'start-process "p4" buffer (p4-get-p4-executable) args))
         (end-time (+ (float-time) p4-exec-timeout))
         (continue t)
         result
         status)
    (while (and continue
                (< (float-time) end-time))
      (setq status (process-status process)
            continue (or (eq 'run status)
                         (eq 'stop status))))
    (setq status (process-status process))
    (cond ((eq 'run status)
           (interrupt-process process 'current-group)
           (setq result 'timeout))
          ((eq 'stop status)
           (continue-process process 'current-group)
           (interrupt-process process 'current-group)
           (setq result 'timeout))
          ((eq 'exit status)
           (setq result (process-exit-status process)))
          ((eq 'signal status)
           ;; call-process produces strings for signals, but there appears to be
           ;; no means exposed for us to reuse that code; this should produce
           ;; the same result
           (setq result (nth (process-exit-status process) p4-signal-names)))
          (t
           (error "Unrecognized process status: %s" status)))
    result))
  (defun p4-call-p4 (command buffer update &rest args)
    "Call p4 COMMAND ARGS synchronously in a separate process.
This is a low level interface that merely calls p4 COMMAND ARGS and captures the
output as described by BUFFER.  It also detects non-quit signals received by the
p4 process.

Use function `p4-exec-p4' if you want the P4 menu added to the buffer.

The program's input comes from /dev/null.

BUFFER as described for function `call-process', except nil means to use the
   default Emacs-P4 output buffer.
UPDATE means to update the buffer while the process runs, if non-nil and output
   is being written to a buffer.
COMMAND is the p4 command to run.
ARGS are passed to p4 COMMAND.

If BUFFER is 0, return immediately with nil.  Otherwise, wait for p4 ARGS to
terminate.

If there's an output buffer, it is saved in `p4-output-buffer' in the current
buffer.

Return nil if the process receives a quit signal (see `p4-quit-signal-p').
Otherwise, signal an error if the process receives a signal.
Otherwise, return a numeric exit code from p4 COMMAND ARGS.

If you quit, the process is killed with SIGINT.  If you quit again, it is
killed with SIGKILL."
    (let ((directory (list "-d" default-directory))
          stderr
          stdout
          status)
      (if (not (and buffer
                    (listp buffer)))
          (setq stdout buffer)
        (setq stdout (car buffer)
              stderr (cdr buffer)))
      (setq stdout (p4-get-buffer stdout))
      (with-current-buffer stdout
        (when (p4-dired-p)
          (setq default-directory
                (funcall (symbol-function 'dired-current-directory)))))
      (setq args (append directory
                         (list command)
                         (p4-useful-list-p args)))
      (setq p4-output-buffer (and stdout
                                  (bufferp stdout)))
      (setq status (apply 'call-process
                          (p4-get-p4-executable)
                          nil
                          (cons stdout stderr)
                          update
                          args))
      (apply 'p4-report-signal status command args)
      ;; if p4-report-signal doesn't call error, but status is a string, then
      ;; status represents a quit signal
      (when (stringp status)
        (setq status nil))
      status))

  (defun p4-command-docstring (command text)
    "Append p4 help COMMAND to TEXT.
If `p4-executable' is not set, then "
    (let ((result text))
      (when command
        (when (eq 'p4-executable-missing
                  (catch 'p4-executable-missing
                    (with-temp-buffer
                      (let ((status (p4-call-p4 "help" t nil command)))
                        (when (and (numberp status)
                                   (zerop status))
                          (setq result (concat text
                                               "\n"
                                               (p4-buffer-string))))))))
          (when (and (fboundp 'byte-compiling-files-p)
                     (funcall (symbol-function 'byte-compiling-files-p)))
            (message "Cannot locate the p4 executable to run p4 help %s"
                     command))))
      result))
  )
;;; End functions needed at compile time for help text

(eval-and-compile
  (defmacro defp4cmd (func arg-list help-command docstring &rest body)
    "Create alias for FUNC that enhances DOCSTRING with p4 help HELP-COMMAND.
Evaluate BODY with the argument list ARG-LIST when invoked."
    `(defalias ',func ,(append (list 'lambda
                                     arg-list
                                     (p4-command-docstring help-command
                                                           docstring))
                               body)))

  (defun p4-differences-in (buffer)
    (let ((result t))
      (with-current-buffer buffer
        (and (= 1 (p4-count-lines))
             (goto-char (point-min))
             (or (looking-at "==== ")
                 (looking-at ".* - file(s) not opened on this client\.$"))
             (setq result nil)))
      result))

  (defun p4-differences-p (&optional filespec)
    "Indicate whether p4 diff FILENAME indicates differences.
FILESPEC is the file to check for differences, if non-nil.  Otherwise, the file
   associated with the current buffer is used."
    (let* ((command "diff")
           (filespec (or filespec
                         (p4-buffer-file-name-2)))
           buffer
           status)
      (with-temp-buffer
        (setq buffer (current-buffer)
              status (p4-exec-p4 command buffer filespec))
        (p4-report-failure status buffer command filespec)
        (p4-differences-in buffer))))
  )

(defsubst p4-re-search-back (regexp)
  "Find the first match for REGEXP in the current buffer before point.
Return nil if no match."
  (re-search-backward regexp nil 'noerror))

(defun p4-revert-buffer (&optional preserve-modes)
  "Revert the current buffer.
Point is returned to the same line and column after reverting.  This isn't
perfect, but it is better than jumping to line 1, column 0.

Auto-save files are ignored.  There is no confirmation prompt.

The optional PRESERVE-MODES as for `revert-buffer'."
  (let ((old-message (and (p4-dired-p)
                          (current-message)))
        (line (count-lines (point-min) (point)))
        (column (current-column)))
    (revert-buffer 'ignore-auto 'noconfirm preserve-modes)
    (goto-line line)
    (forward-char column)
    (when old-message
      (message old-message))))

(defun p4-kill-form-buffer ()
  "Kill a p4 form buffer without executing the pending p4 command."
  (interactive)
  (kill-buffer nil)
  (message "p4 %s cancelled" p4-form-command))

(defun p4-call-command (command buffer error-handling
                                &optional callback
                                &rest args)
  "Run `p4 COMMAND ARGS' capturing the output in BUFFER.
The output appears in the echo area, if possible, and BUFFER is killed or
buried.  Otherwise, BUFFER is displayed.

BUFFER identifies the output buffer.  If BUFFER is a non-nil list, it is assumed
   to have the form (BUF . KEEP), where BUF is a buffer and KEEP is as described
   in `p4-display-output'.  Otherwise, BUFFER is passed to
   `p4-make-output-buffer' and is killed if the output can be displayed in the
   echo area.
ERROR-HANDLING controls error handling for the command.  If it is a non-nil
   list, it is assumed to have the form (SIGNAL . MERGE), where SIGNAL means
   signal an error on non-zero exit status, if non-nil, and MERGE means merge
   error output with standard output in BUFFER.  Otherwise, if ERROR-HANDLING is
   non-nil, signal an error on non-zero exit status.
CALLBACK is a callback to invoke when the command succeeds, if non-nil.  If
   CALLBACK is a function, it is called with the output buffer as its only
   argument.  Otherwise, CALLBACK is expected to be a list of the form
   (CALLBACK ARGS) where CALLBACK is a function to be called with the output
   buffer plus ARGS as arguments.

Signals an error if p4 COMMAND ARGS was signaled.  Otherwise, returns the
p4 COMMAND ARGS exit status.

Throws t to `p4-executable-missing' if `p4-executable' is nil."
  (let ((buffer buffer)
        keep
        merge
        result
        signal)
    (if (and buffer
             (listp buffer))
        (setq keep (cdr buffer)
              buffer (car buffer))
      (setq buffer (p4-make-output-buffer buffer)))
    (when error-handling
      (if (listp error-handling)
          (setq signal (car error-handling)
                merge (cdr error-handling))
        (setq signal 't)))
    (save-excursion
      (with-current-buffer buffer
        (erase-buffer))
      (setq result (apply 'p4-exec-p4 command (cons buffer merge) args))
      (when signal
        (apply 'p4-report-failure result buffer command args))
      (and (numberp result)
           (zerop result)
           (p4-partial-cache-cleanup command)))
    (p4-display-output buffer keep)
    (when (and result
               (buffer-live-p buffer))
      (cond ((not callback)
             t)
            ((functionp callback)
             (funcall callback buffer))
            ((listp callback)
             (apply (car callback) buffer (cdr callback)))))
    result))

(defun p4-call-form-command (command &optional buffer regexp &rest args)
  "Call p4 COMMAND ARGS asynchronously without an external editor.
The input comes from and output goes to a local buffer to avoid an external
editor.

COMMAND is the p4 command to run.
BUFFER is the buffer to create, if non-nil.  If BUFFER is nil, use the buffer
   created by calling `p4-make-command-buffer' with COMMAND.
REGEXP is the regular expression to search for to set the cursor on.
ARGS is the set of arguments to pass to p4 COMMAND, if non-nil."
  (let ((arguments (append (list "-o")
                           (p4-useful-list-p args)))
        (directory default-directory)   ; save before calling set-buffer
        result)
    (set-buffer (if buffer
                    (p4-make-output-buffer buffer)
                  (p4-make-command-buffer command)))
    ;; save for p4-commit-form-changes
    (setq p4-form-command command)
    ;; this affects p4 which is run below, but it is also convenient for users
    ;; that the form buffer has the same current directory
    (cd directory)
    (setq result (catch 'p4-executable-missing
                   (p4-call-form-command-2 command
                                           (current-buffer)
                                           regexp
                                           arguments)
                   t))
    (if (eq 'p4-executable-missing result)
        nil
      result)))

(defun p4-call-form-command-2 (command buffer regexp arguments)
  "Helper function for `p4-call-form-command'."
  (let ((status (apply 'call-process-region
                       (point-min)
                       (point-max)
                       (p4-get-p4-executable)
                       t
                       t
                       nil
                       command
                       arguments)))
    (apply 'p4-report-error status t command arguments)
    (goto-char (point-min))
    (insert (concat "# Created using " (p4-emacs-version) ".\n"
                    "# Type C-c C-c to submit changes and exit buffer.\n"
                    "# Type C-c C-k or C-x k to abort.\n"
                    "#\n"))
    (p4-format-form)
    (switch-to-buffer-other-window buffer)
    (indented-text-mode)
    (setq p4-async-minor-mode t
          fill-column 80
          p4-current-args nil
          buffer-offer-save t
          indent-tabs-mode t)
    (p4-call-on-matches buffer '('p4-activate-form-files))
    (use-local-map p4-async-minor-map)
    (run-hooks 'p4-async-command-hook)
    (goto-char (point-min))
    (when regexp
      ;; advance to matching part of buffer
      (p4-re-search regexp))
    (set-buffer-modified-p nil)
    (message (concat "C-c C-c to submit changes and exit buffer; "
                     "C-c C-k or C-x k to abort"))))

(defun p4-commit-form-changes ()
  "Commit the changes in the current buffer.
Internal function called by the key bindings defined by `p4-call-form-command'
to process the buffer after form editing is finished."
  (interactive)
  (let* ((command p4-form-command)
         (args (list "-d" default-directory command "-i"))
         form-end)
    (when p4-current-args
      (setq args (append args (list p4-current-args))))
    (p4-exit-viper-edit-state)
    (p4-form-error-delete)
    (setq form-end (point-max))
    (goto-char form-end)
    (catch 'p4-executable-missing
      (let ((status (apply 'call-process-region
                           (point-min)
                           form-end
                           (p4-get-p4-executable)
                           nil
                           t
                           nil
                           args))
            (refresh (or (string= "submit" command)
                         (string= "change" command))))
        (apply 'p4-report-signal status args)
        (if (and (not (stringp status))
                 (zerop status))
            (progn
              (kill-buffer nil)
              (p4-partial-cache-cleanup command)
              (when refresh
                (p4-refresh-p4-files)
                (p4-check-mode-all-p4-buffers)
                (when p4-notify
                  (p4-notify p4-notify-list)))
              (message "p4 %s done." command))
          (goto-char form-end)
          (p4-form-error-format)
          (p4-recover-failed-submit)
          (p4-form-error-goto-line)
          (recenter 1))))))

(defun p4-form-error-goto-line ()
  "Find p4 error message and go to the indicated line."
  (let ((start (point)))
    (goto-char (point-min))
    (if (p4-re-search "^# Error detected at line \\([0-9]+\\)\\.$")
        (let ((line (string-to-number (match-string 1))))
          (when (p4-re-search
                 "^# Change description missing.  You must enter one.")
            (setq line (- line 2)))
          (goto-line line))
      (goto-char start))))

(defun p4-form-error-format ()
  "Format p4 error message as a comment block and save its extent.
The extent is saved in the buffer local variable `p4-error-extent'."
  (save-excursion
    (let ((divider "# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
          (notice  "# (n.b. All text in this block will be deleted)\n")
          (start (point)))
      (insert "\n"
              p4-begin-form-error
              "\n"
              notice
              divider)
      (while (not (eobp))
        (insert "# ")
        (forward-line))
      (insert divider
              p4-end-form-error
              "\n")
      (setq p4-error-extent (p4-create-extent start (point)))
      (p4-set-extent-property p4-error-extent 'face 'p4-form-error-face)
      (p4-set-extent-property p4-error-extent 'read-only t))))

(defun p4-form-error-delete ()
  "Delete p4 error message formatted by `p4-form-error-format'."
  (when p4-error-extent
    (let ((start (p4-get-extent-start p4-error-extent))
          (end (p4-get-extent-end p4-error-extent)))
      (p4-set-extent-property p4-error-extent 'read-only nil)
      (delete-region start end)
      (setq p4-error-extent nil))))

(defun p4-format-form ()
  "Apply form faces to the current buffer."
  (save-excursion
    (let (extent)
      (while (p4-re-search "^[A-Z][^:]+")
        (setq extent (p4-create-extent (match-beginning 0) (match-end 0)))
        (p4-set-extent-property extent 'face 'p4-form-label-face)))))

(defun p4-recover-failed-submit ()
  "Capture change list from p4 error message and update Change and Status fields."
  (save-excursion
    (goto-char (point-min))
    (when (p4-re-search "^# Change [1-9][0-9]* created")
      (let (change
            end
            start)
        (beginning-of-line)
        (forward-word 1)
        (forward-char 1)
        (setq start (point))
        (forward-word 1)
        (setq end (point))
        (setq change (buffer-substring start end))
        (goto-char (point-min))
        (when (p4-re-search "^Change:")
          (forward-char 1)
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (delete-region start end)
          (insert change)
          (when (p4-re-search "^Status:")
            (forward-char 1)
            (setq start (point))
            (end-of-line)
            (setq end (point))
            (delete-region start end)
            (insert "pending")
            (when (p4-re-search "^# Submit \\(aborted\\|failed\\) -- fix problems\.\*then use 'p4 submit -c .+$")
              (delete-region (match-beginning 0) (1+ (match-end 0))))))))))

(defun p4-set-p4-executable (pathname)
  "Set the path to the correct p4 executable to PATHNAME.
Save PATHNAME in `p4-executable' as the new p4 executable pathname.  If PATHNAME
is a directory or is not an executable file, set `p4-executable' to nil.

Return `p4-executable'."
  (interactive "FFull pathname of the p4 executable: ")
  (when (or (file-directory-p pathname)
            (not (file-executable-p pathname)))
    (setq pathname nil))
  (setq p4-executable pathname)
  p4-executable)

(defun p4-remove-output-buffer (buffer)
  "Indicate whether BUFFER was removed from the Emacs-P4 output buffer stack."
  (let ((length (length p4-output-buffers)))
    (setq p4-output-buffers (delq buffer p4-output-buffers))
    (not (= length (length p4-output-buffers)))))

(defun p4-push-output-buffer (buffer)
  "Push BUFFER onto the front of the Emacs-P4 output buffer stack."
  (setq p4-output-buffers (cons buffer p4-output-buffers)))

(defun p4-pop-output-buffer ()
  "Pop the first buffer from the Emacs-P4 output buffer stack."
  (let ((result (car p4-output-buffers)))
    (setq p4-output-buffers (cdr p4-output-buffers))
    result))

(defun p4-quit-buffer (buffer &optional kill)
  "Restore the window configuration saved in BUFFER then bury or kill BUFFER.
BUFFER must be an Emacs-P4 output buffer created by `p4-make-output-buffer'.
KILL means kill BUFFER, if non-nil.  Otherwise, if nil or omitted, bury BUFFER."
  (when (buffer-live-p buffer)
    (let (config)
      (when (local-variable-p 'p4-window-config buffer)
        (with-current-buffer buffer
          (setq config (symbol-value 'p4-window-config))))
      (when config
        (set-window-configuration config)
        (if kill
            (kill-buffer buffer)
          (bury-buffer buffer))))))

(defun p4-quit-and-bury-output-buffer ()
  "Pop, quit, and bury the most recent Emacs-P4 output buffer.
The most recent Emacs-P4 output buffer is popped from the output buffer stack,
if any.  The window configuration saved in that buffer is restored, and then
that buffer is buried."
  (interactive)
  (p4-quit-buffer (p4-pop-output-buffer)))

(defun p4-quit-and-kill-output-buffer ()
  "Pop, quit, and kill the most recent Emacs-P4 output buffer.
The most recent Emacs-P4 output buffer is popped from the output buffer stack,
if any.  The window configuration saved in that buffer is restored, and then
that buffer is killed."
  (interactive)
  (p4-quit-buffer (p4-pop-output-buffer) 'kill))

(defun p4-quit-and-bury-current-buffer ()
  "Forget, quit, and bury the current buffer if it is an Emacs-P4 output buffer.
If the current buffer is an Emacs-P4 output buffer, it is removed from the
output buffer stack, the window configuration saved in the buffer is restored,
and then the buffer is buried."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (p4-remove-output-buffer buffer)
      (p4-quit-buffer buffer))))

(defun p4-quit-and-kill-current-buffer ()
  "Forget, quit, and kill the current buffer if it is an Emacs-P4 output buffer.
If the current buffer is an Emacs-P4 output buffer, it is removed from the
output buffer stack, the window configuration saved in the buffer is restored,
and then the buffer is killed."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (p4-remove-output-buffer buffer)
      (p4-quit-buffer buffer 'kill))))

(defun p4-make-command-buffer (command &rest args)
  "Create a buffer with a standardized name for p4 COMMAND ARGS output."
  (let ((name (apply 'p4-buffer-name command args)))
    (p4-make-output-buffer name)))

(defun p4-display-output (buffer &optional keep error)
  "Display the contents of BUFFER in the echo area, if possible, or show BUFFER.
The output will appear in the echo area if it is sufficiently short, as defined
by the function `p4-max-echo-area-lines'.

If the output is shown in the echo area, then BUFFER is deleted if
`p4-kill-output-buffer-when-echoing' is non-nil.

BUFFER must be a live buffer.
KEEP means keep the output buffer, if non-nil, regardless of the value of
   `p4-kill-output-buffer-when-echoing'.
ERROR means report the output as an error, if non-nil.  Otherwise, just report
   the output in the echo area."
  (let ((lines (p4-count-lines buffer)))
    (unless (zerop lines)
      (if (or (> 2 lines)
              (<= lines (p4-max-echo-area-lines)))
          (let (text)
            (with-current-buffer buffer
              (goto-char (point-max))
              (when (bolp)
                (backward-char 1))
              (setq text (buffer-substring (point-min) (point))))
            (if (and (not keep)
                     p4-kill-output-buffer-when-echoing)
                (kill-buffer buffer)
              (bury-buffer buffer))
            (if error
                (error text)
              (message text)))
        (with-current-buffer buffer
          (goto-char (point-min)))
        (display-buffer buffer 'not-this-window)))))

(defun p4-is-opened (&optional filename)
  "Indicates whether FILENAME is currently open for some action.
This checks the answer from p4 opened FILENAME."
  (interactive)
  (let ((command "opened")
        buffer
        result
        status)
    (unless filename
      (setq filename (p4-buffer-file-name-2)))
    (with-temp-buffer
      (setq buffer (current-buffer)
            status (p4-call-p4 "opened" t nil filename))
      (p4-report-failure status buffer command filename)
      (and (numberp status)
           (zerop status)
           (p4-display-output buffer 'keep)
           (goto-char (point-min))
           (if (p4-re-search " - file(s) not opened on this client.$")
               (message "File not opened on this client")
             (goto-char (point-min))
             (if (p4-re-search
                  (concat " \\(is not under client's root "
                          "\\|unknown - use 'client' command to create it\."
                          "\\)"))
                 (when (interactive-p)
                   (message (buffer-string)))
               (when (interactive-p)
                 (p4-display-output buffer))
               (setq result t)))))
    result))

(defun p4-call-describe (&rest args)
  "Run p4 describe ARGS and activate the filenames in the output."
  (let ((buffer (apply 'p4-make-command-buffer "describe" args)))
    (apply 'p4-call-command "describe" buffer 'signal-errors nil args)
    (and (buffer-live-p buffer)
         (p4-activate-diff-buffer buffer))))

(defun p4-call-diff (&rest args)
  "Display the output of p4 diff ARGS or a \"No differences\" message."
  (let ((buffer (apply 'p4-make-command-buffer "diff" args)))
    (apply 'p4-call-command "diff" (cons buffer 'keep) 'signal-errors nil args)
    (if (p4-differences-in buffer)
        (p4-activate-diff-buffer buffer)
      (kill-buffer buffer)
      (setq buffer nil)
      (p4-alert "No differences"))))

(defun p4-call-diff-get-args (&rest args)
  "Call p4 diff ARGS, possibly prompting for user input.
If current prefix argument is non-nil, prompt user for input."
  (setq args (p4-get-args "diff options"
                          current-prefix-arg
                          p4-default-diff-options
                          args))
  (apply 'p4-call-diff args))

(defun p4-call-diff2 (filespec1 filespec2 &rest args)
  "Create a buffer with the output of p4 diff2 ARGS FILESPEC1 FILESPEC2.
FILESPEC1 and FILESPEC2 are the files to compare.
ARGS are additional arguments for p4 diff2, if non-nil."
  (let ((arguments (append (when (p4-useful-list-p args)
                             args)
                           (list filespec1 filespec2)))
        (buffer (p4-make-command-buffer "diff2")))
    (apply 'p4-call-command "diff2" buffer 'signal-errors nil arguments)
    (and (buffer-live-p buffer)
         (p4-activate-diff-buffer buffer))))

(defun p4-call-login ()
  "Log into P4.
If the P4PASSWD environment variable is set, its value is used as the password.
Otherwise, if the file named by `p4-password-file' is non-empty, its contents
   are used as the password.
Otherwise, prompt the user for a password."
  (let ((password (p4-get-current-variable "P4PASSWD")))
    (if password
        (p4-password-login password)
      (let ((size (nth p4-SIZE (file-attributes p4-password-file))))
        (if size
            (p4-file-login p4-password-file)
          (call-interactively 'p4-prompt-login))))))

(defun p4-call-opened (buffer &rest args)
  "Populate BUFFER with the output of p4 opened ARGS.
BUFFER as for `p4-get-buffer'

Return the buffer."
  (let ((buffer (p4-get-buffer buffer)))
    (if (apply 'p4-list-opened-files buffer args)
        (progn
          (p4-call-on-matches buffer '('p4-activate-opened-buffer))
          (with-current-buffer buffer
            (use-local-map p4-opened-map))
          (p4-make-read-only buffer))
      (kill-buffer buffer)
      (setq buffer nil)
      (p4-alert "No files opened"))
    buffer))

(defun p4-call-print (buffer report-revision &rest args)
  "Run p4 print ARGS and put the output in BUFFER.
BUFFER is the buffer or buffer name into which to put the output, if non-nil.
   Otherwise, create a buffer.
REPORT-REVISION means to report the filename and revision on the first line of
   the buffer, if non-nil.

Return the output buffer if the command succeeds.  Otherwise kill output buffer,
if BUFFER was not a buffer, and return nil."
  (let* ((result (and buffer
                      (bufferp buffer)))
         (was-buffer result))
    (or result
        (setq result (if buffer
                         (p4-make-output-buffer buffer)
                       (p4-make-command-buffer "print" args))))
    (when report-revision
      (setq args (append (list "-q") args)))
    (if (zerop
         (apply 'p4-call-command "print" (cons result 'keep) nil nil args))
        (with-current-buffer result
          (goto-char (point-min))
          (if (and (= 1 (p4-count-lines))
                   (looking-at "^.+ - no file(s) at that revision.$"))
              (progn
                (kill-buffer result)
                (p4-alert "No such revision"))
            (p4-font-lock-buffer result)
            (p4-activate-client-pathnames result
                                          p4-pathname-with-revision-plus-)))
      (unless was-buffer
        (kill-buffer result))
      (setq result nil))
    result))

(defun p4-call-simple-p4-command (command refresh &optional args)
  "Run p4 COMMAND ARGS.
Refresh buffers for all files under P4 version control if REFRESH is non-nil.

Don't call unless `p4-available-p' is non-nil."
  (let ((buffer (p4-make-command-buffer command (car args))))
    (apply 'p4-call-command command buffer 'signal-errors nil args)
    (and refresh
         (p4-refresh-p4-files))
    (and (buffer-live-p buffer)
         (p4-activate-client-pathnames buffer))))

(defun p4-call-sync (&rest args)
  "Call p4 sync ARGS.
ARGS is the argument list with which to call p4 sync, if non-nil.

If the current prefix argument is non-nil, prompt user for arguments."
  (when (p4-available-p)
    (let ((command "sync")
          buffer
          error-message)
      (setq args (p4-get-args command current-prefix-arg args args)
            buffer (p4-make-command-buffer command args))
      (condition-case error-description
          (apply 'p4-call-command command buffer 'signal-errors nil args)
        (error
         (setq error-message (cadr error-description))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (or (> 1 (p4-count-lines))
                  (not (looking-at " - file(s) not in client view.")))
              (progn
                (p4-call-on-matches
                 buffer
                 '('p4-mark-cannot-clobber
                   'p4-mark-open-for-edit
                   'p4-mark-opened-not-changed
                   'p4-mark-permission-denied
                   'p4-mark-must-resolve))
                (p4-refresh-p4-files)
                (p4-check-mode-all-p4-buffers))
            (kill-buffer buffer)
            (p4-alert "No matching files in client view"))))
      (when error-message
        (error "%s" error-message)))))

(defun p4-get-extent-property (extent property)
  "Return EXTENT's PROPERTY."
  (get-char-property (p4-get-extent-start extent) property))

(defun p4-save-some-files (&rest files)
  "Prompt the user to save FILES if there are unsaved changes.
FILES is the list of files to check.  If 'all, check all Emacs-P4 controlled
files.  If nil, check the current buffer's file only."
  (cond ((not files)
         (setq files (list (p4-buffer-file-name-2))))
        ((and (= 1 (length files))
              (eq 'all (car files))
         (setq files p4-all-p4-buffer-files))))
  (save-some-buffers nil
                     (function (lambda ()
                                 (member (p4-buffer-file-name-2) files)))))

(defun p4-diff-all-opened ()
  "Diff all opened files."
  (interactive)
  (when (p4-available-p)
    (p4-save-some-files 'all)
    (p4-call-diff nil p4-default-diff-options)))

(defun p4-get-file-revision (filename rev)
  "The filename with revision number suitable for calling p4 commands.
FILENAME is the file portion, if any.
REV is the revision number string as given in various forms by p4 output,
   including \"head,\" an empty string, a number optionally prefixed by \"@\"
   or \"#,\" etc."
  (cond ((or (char-equal ?# (elt rev 0))
             (char-equal ?@ (elt rev 0)))
         (concat filename rev))
        ((string= "" rev)
         filename)
        ((string-match "^\\([0-9]+\\|none\\|head\\|have\\)$" rev)
         (concat filename "#" rev))
        (t
         rev)))

(defun p4-ediff ()
  "Compare the client workspace and have revision using Ediff."
  (interactive)
  (when (p4-available-p)
    (p4-ediff-revision (p4-file-revision))))

(defun p4-ediff-head ()
  "Compare the client workspace and head revision using Ediff."
(interactive)
  (when (p4-available-p)
    (p4-ediff-revision "#head")))

(defun p4-ediff-revision (revision)
  "Compare the client workspace file and REVISION from the depot using Ediff."
  (interactive "sRevision to compare against: ")
  (when (p4-available-p)
    (if (not revision)
        (p4-alert "Revision must be non-nil")
      (let ((filespec (p4-get-full-filespec revision)))
        (p4-ediff-files (p4-buffer-file-name-2) ; might be from Dired
                        filespec)))))

(defun p4-ediff-files (client-file depot-filespec)
  "Compare CLIENT-FILE and DEPOT-FILESPEC using Ediff.
CLIENT-FILE is the pathname of the client workspace file to compare.
DEPOT-FILESPEC is the depot file specification of the file to compare."
  (let* ((config (current-window-configuration))
         (buffer (p4-get-depot-file-buffer depot-filespec)))
    (when buffer
      (bury-buffer buffer)
      (let* ((current-buffer (find-buffer-visiting client-file))
             (was-buffer current-buffer))
        (unless was-buffer
          (setq current-buffer (find-file-noselect client-file)))
        (with-current-buffer buffer
          (p4-set-major-mode current-buffer))
        (p4-call-ediff-buffers config buffer nil current-buffer was-buffer)))))

(defun p4-set-major-mode (exemplar)
  "Set the major mode of EXEMPLAR on the current buffer.
EXEMPLAR is the buffer with the major mode to use."
  (let (mode
        was-modified)
    (with-current-buffer exemplar
      (setq mode major-mode))
    (setq was-modified (buffer-modified-p))
    (toggle-read-only 0)
    (funcall mode)
    (set-buffer-modified-p was-modified)
    (toggle-read-only 1)))

(defun p4-ediff2 (version1 version2)
  "Compare two files described by VERSION1 and VERSION2 using Ediff.
VERSION1 and VERSION2 are filespecs or revision numbers for the current buffer's
   file."
  (interactive
   (let ((filename (p4-buffer-file-name-2))
         rev)
     (when filename
       (let ((rev-num 0))
         (setq rev (p4-file-revision nil filename))
         (when rev
           (setq rev-num (string-to-number rev)))
         (if (> rev-num 1)
             (setq rev (number-to-string (1- rev-num)))
           (setq rev nil))))
     (setq rev (p4-read-arg-string "First filespec or revision: " rev))
     (list rev (p4-read-arg-string "Second filespec or revision: " rev))))
  (when (p4-available-p)
    (let ((config (current-window-configuration))
          result
          buffer1
          buffer2)
      (setq version1 (p4-get-full-filespec version1))
      (setq buffer1 (p4-get-ediff-buffer version1))
      (when buffer1
        (setq version2 (p4-get-full-filespec version2)
              buffer2 (p4-get-ediff-buffer version2))
        (if buffer2
            (setq result
                  (p4-call-ediff-buffers config buffer1 nil buffer2 nil))
          (kill-buffer buffer1)))
      result)))

(defun p4-get-ediff-buffer (filespec)
  "Create buffer with the contents of p4 print FILESPEC."
  (let ((result (catch 'p4-no-such-file
                  (p4-get-depot-file-buffer filespec 'require))))
    (when (eq 'p4-no-such-file result)
      (setq result nil)
      (p4-alert "Bad filespec: %s" filespec))
    result))

(defun p4-call-ediff-buffers (window-config buffer-A keep-A buffer-B keep-B)
  "Run Ediff on BUFFER-A and BUFFER-B and restore WINDOW-CONFIG when finished.
WINDOW-CONFIG must be a value returned by `current-window-configuration'.
BUFFER-A is Ediff's buffer A.
BUFFER-B is Ediff's buffer B.
KEEP-A means to not kill BUFFER-A when quitting Ediff, if non-nil.
KEEP-B means to not kill BUFFER-B when quitting Ediff, if non-nil."
  ;; This seems to be the closest we can get to restore the original window
  ;; configuration.  Calling ediff-cleanup-mess directly (the usual default
  ;; value of ediff-quit-hook) followed by restoring the window configuration
  ;; doesn't work any better.  Saving the selected frame and current buffer and
  ;; restoring them before restoring the window configuration doesn't work any
  ;; better.  There must be something else we can do, but this will have to do
  ;; for now.
  (require 'ediff)
  (ediff-buffers buffer-A
                 buffer-B
                 `((lambda ()
                    (add-hook 'ediff-quit-hook
                              (lambda ()
                                (set-window-configuration ,window-config))
                              'append
                              'local)
                    (add-hook 'ediff-cleanup-hook
                              (lambda ()
                                (unless ,keep-A
                                  (kill-buffer ,buffer-A))
                                (unless ,keep-B
                                  (kill-buffer ,buffer-B))
                                (p4-menu-add))
                              'append
                              'local)))))

(defun p4-print-revision (revision)
  "Display the output of p4 print for REVISION of the current buffer's file.
Create a new buffer with the output from calling p4 print for the specified
REVISION of the current buffer's file.

REVISION is the revision, as a string, of the current buffer's file to print."
  (interactive (list (read-from-minibuffer "Revision: ")))
  (let ((current (current-buffer))
        (filename (p4-buffer-file-name-2))
        buffer
        buffer-name
        filespec)
    (if (not filename)
        (error "No filename associated with this buffer")
      (setq buffer-name (concat
                         (file-name-nondirectory filename)
                         ".~"
                         revision
                         "~")
            filespec (p4-get-file-revision filename revision))
      (when (eq 'p4-no-such-file
                (catch 'p4-no-such-file
                  (setq buffer
                        (p4-get-depot-file-buffer filespec
                                                  'require buffer-name))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (goto-char (point-min))
                      (if (looking-at "^.* - no file(s) at that [^.]+.$")
                          (p4-display-output buffer nil 'error)
                        (set-visited-file-name buffer-name)
                        (p4-set-major-mode current)
                        (p4-make-read-only)
                        (pop-to-buffer (current-buffer))
                        (switch-to-buffer-other-window current)
                        (message ""))))))
        (funcall (if (interactive-p)
                     'p4-alert
                   'error)
                 (format "Invalid file specification: \"%s\"" filespec))))))

(defun p4-get-depot-file-buffer (filespec &optional require buffer-name)
  "Create a buffer with the output of p4 print FILESPEC.
FILESPEC is assumed to be a good p4 print argument.
REQUIRE means to throw 'p4-no-such-file to 'p4-no-such-file if P4 indicates no
   file matches FILESPEC, if non-nil.
BUFFER-NAME is the name given to the buffer, if non-nil.  Otherwise, the buffer
   name is FILESPEC minus any directory path components."
  (when (p4-available-p)
    (let* ((name (or buffer-name
                     (file-name-nondirectory filespec)))
           (buffer (p4-call-print name t filespec)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-min))
          (and require
               (looking-at "^.* - \\\(no file(s) at that revision\\\|protected namespace - access denied\\\).$")
               (progn
                 (kill-buffer buffer)
                 (throw 'p4-no-such-file 'p4-no-such-file)))
          (p4-make-read-only)
          (message "")))
      buffer)))

(defun p4-get-full-filespec (initial)
  "Compute a complete filespec from the string INITIAL.
Return INITIAL if it is a fully formed filespec.
Return \"#\" and INITIAL appended to the current buffer's filename if INITIAL is
   the string form of a number greater than zero or a named revision like \"head\"
   or \"have.\"
Return INITIAL appended to the current buffer's filename if INITIAL starts with
   \"#\" or \"@\".
Otherwise, return nil."
  (when initial
    (let ((first (elt initial 0)))
      (if (or (char-equal ?# first)
              (char-equal ?@ first))
          (concat (p4-buffer-file-name-2) initial)
        (if (< 0 (string-to-number initial))
            (concat (p4-buffer-file-name-2) "#" initial)
          (if (p4-file-revision nil initial)
              initial
            (p4-file-revision nil (concat (p4-buffer-file-name-2) "#" initial)))
          )))))

(defun p4-make-read-only (&optional buffer)
  "Make BUFFER unmodified and read only.
BUFFER is the buffer to mark, if non-nil.  Otherwise, mark the current buffer."
  (if buffer
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (toggle-read-only 1))
    (set-buffer-modified-p nil)
    (toggle-read-only 1)))

(defsubst p4-set-extent-face (start end face)
  "Mark START-END with FACE."
  (p4-set-extent-property (p4-create-extent start end) 'face face))

(defun p4-create-active-link (start end &optional plist)
  "Set properties on the region START-END of the current buffer to make it active.
An active region is visually highlighted and reacts visually to mouse movements.

PLIST is an optional property list of additional properties to apply to the
   region.

It may also have associated actions, though this function sets none."
  (let ((extent (p4-create-extent start end)))
    (p4-set-extent-property extent 'face 'bold)
    (p4-set-extent-property extent 'mouse-face 'highlight)
    (p4-set-extent-properties extent plist)))

(defun p4-move-buffer-point-to-top (buffer)
  "Move point to the top of BUFFER."
  (let ((window (get-buffer-window buffer)))
    (when window
      (save-selected-window
        (select-window window)
        (goto-char (point-min))))))

(defun p4-create-change-log (command &rest args)
  "Call p4 COMMAND ARGS and make files in the resulting list active."
  (let* ((args (append (list p4-default-log-options)
                       args))
         (buffer (apply 'p4-make-command-buffer command args)))
    (apply 'p4-call-command command buffer 'signal-errors nil args)
    (and (buffer-live-p buffer)
         (if (< 1 (p4-count-lines))
             (p4-activate-change-log-buffer buffer)
           (kill-buffer buffer)
           (message "No changes in depot")))))

(defun p4-get-server-version ()
  "Return the version number of the p4 server."
  (let ((port (p4-current-server-port))
        result)
    (setq result (cdr (assoc port p4-server-version)))
    (unless result
      (with-temp-buffer
        (when (zerop (p4-call-p4 "info" t nil))
          (goto-char (point-min))
          (p4-re-search
           "^Server version: .*\/.*\/\\(\\([0-9]+\\)\.[0-9]+\\)[^/]*\/.*(.*)$")
          (setq result (string-to-number (match-string 2)))
          (setq p4-server-version (cons (cons port result)
                                        p4-server-version)))))
    result))

(defun p4-directory-p (pathname)
  "Indicate whether PATHNAME is a directory"
  (eq 't (elt (file-attributes pathname) 0)))

(defun p4-get-client-root (client)
  "Return the root of the client specification of CLIENT.
Both Root: and AltRoots: are checked for a valid directory name.

Return nil if no valid directory name was found."
  (let (result)
    (with-temp-buffer
      (and (p4-depot-output "client" nil "-o" client)
           (goto-char (point-min))
           (p4-re-search "^Root:[ \t]+\\(.*\\)$")
           (setq result (p4-canonicalize-client-root (match-string 1))))
      (unless (p4-directory-p result)
        (setq result nil)
        (goto-char (point-min))
        (when (p4-re-search "^AltRoots:")
          (while (and (not result)
                      (p4-re-search "^[ \t]+\\(.*\\)$"))
            (setq result (p4-canonicalize-client-root (match-string 1)))
            (unless (p4-directory-p result)
              (setq result nil))))))
    result))

(defun p4-canonicalize-client-root (client)
  "Canonicalizes the root of the client specification of CLIENT.
Use `file-truename' to adjust CLIENT if `p4-follow-symlinks' is non-nil.

Return client if `p4-windows-os' is nil.
Otherwise, return client minus a trailing backslash, if any."
  (let ((len (length client)))
    ;; For Windows, since the client root may be terminated with
    ;; a \ as in c:\ or drive:\foo\bar\, we need to strip the
    ;; trailing \ .
    (and p4-windows-os
         (> len 1)
         (string= "\\" (substring client -1))
         (setq client (substring client 0 (1- len))))
    (if p4-follow-symlinks
        (file-truename client)
      client)))

(defun p4-map-depot-to-client (filespec &optional required)
  "Map a depot filespec to client workspace pathnames or nil if not mapped.
FILESPEC is the depot filespec to map.
REQUIRED means to signal an error if FILESPEC cannot be mapped to the client
   workspace, if non-nil."
  (let ((map (p4-map-depot-files (list filespec)))
        result)
    (when map
      (setq result (cdar map)))
    (and required
         (not result)
         (error "Not mapped into the client workspace: %s" filespec))
    result))

(defun p4-map-depot-files (file-list)
  "Map FILE-LIST in the depot for the current client.
Returns an alist of depot/client workspace pathnames."
  (let (arg-len
        continue
        element
        len
        result
        sublist)
    ;; process all files in file-list
    (while file-list
      (setq arg-len 0
            continue t
            sublist nil)
      ;; process only as many as will fit within p4-command-line-length
      (while (and file-list
                  continue)
        (setq element (car file-list)
              len (+ arg-len 1 (length element))) ; include space between
        (when (>= (length element) p4-command-line-length)
          (error "Pathname in FILE-LIST exceeds p4-command-line-length: \"%s\""
                 element))
        (if (<= len p4-command-line-length)
            (setq file-list (cdr file-list)
                  sublist (cons element sublist)
                  arg-len len)
          (setq continue nil)))
      (setq result (append result
                           (p4-build-depot-files-map sublist))))
    result))

(defun p4-build-depot-files-map (file-list)
  "Run p4 where FILE-LIST and return alist of depot/client workspace pathnames."
  (let* ((current-client (p4-current-client))
         (client-root (p4-get-client-root current-client))
         (re-current-client (regexp-quote current-client))
         (re-client-root (regexp-quote client-root))
         (re-path (concat "^\\([^\n]+\\) //" re-current-client))
         result)
    (with-temp-buffer
      (when (zerop (apply 'p4-call-p4 "where" t nil file-list))
        (goto-char (point-min))
        (if (> 98 (p4-get-server-version))
            (progn
              (setq re-path (concat re-path "\\(.*\\)$"))
              (while (p4-re-search re-path)
                (setq result (cons (cons (match-string 1)
                                         (concat client-root
                                                 (match-string 2)))
                                   result))))
          (setq re-path (concat re-path
                                "\\([^\n]+\\) \\("
                                re-client-root
                                ".*\\)$"))
          (while (p4-re-search re-path)
            (setq result (cons (cons (match-string 1)
                                     (match-string 3))
                               result))))))
    result))

(defconst p4-activate-form-files "^	\\(//[^	]+\\)")
(defun p4-activate-form-files ()
  "Make depot pathnames in the form active in the current buffer.
Clicking mouse button one or pressing enter when point is on a depot pathname
will open the corresponding workspace file.  Clicking mouse button two(three)
when point is on a depot pathname will show differences with diff(ediff) between
the depot file and the corresponding workspace file."
  (message "p4-activate-form-files")
  (let ((extent (p4-create-extent (match-beginning 1) (match-end 1)))
        (pathname (match-string 1))
        (test 1))
    (p4-set-extent-property extent 'mouse-face 'highlight)
    (p4-set-action extent
                   'default ; enter
                   (p4-make-action-closure 'p4-open-client-file pathname))
    (p4-set-action extent
                   2 ; mouse button 2
                   (p4-make-action-closure 'p4-open-client-file pathname))
    (p4-set-action extent
                   3 ; mouse button 3
                   (p4-make-action-closure 'p4-ediff-client-file pathname))))

(defun p4-confirm-undo-delete (pathname)
  "p4 revert PATHNAME if the user agrees.
This should only be called for a file that has been deleted or the prompt will
be nonsensical.

PATHNAME is the depot pathname of a file to revert."
  (interactive)
  (when (yes-or-no-p (concat "Really undo deletion of " pathname))
    (p4-file-command "revert" nil nil nil pathname)))

(defconst p4-activate-opened-buffer
  (concat "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?" ; branch delimiters
          "\\(//[^/@# ][^/@#]*/[^#\n]*\\)" ; depot pathname
          "#\\([1-9][0-9]*\\)"          ; revision
          " - \\([^ ]+\\)"))             ; action
(defun p4-activate-opened-buffer ()
  "Activate the depot pathnames in current buffer generated by p4 opened.
Clicking mouse button one or pressing enter when point is on a depot pathname
will open the corresponding workspace file."
  (let* ((action (match-string 4))
         (depot-file (match-string 2))
         (end (match-end 2))
         (extra (match-string 1))
         (face 'bold)
         (start (match-beginning 2))
         (extent (p4-create-extent start end))
         (open-client-file (p4-make-action-closure 'p4-open-client-file
                                                   depot-file)))
    (p4-set-action extent 'default open-client-file)
    (p4-set-action extent 2 open-client-file) ; mouse button 2
    (p4-set-extent-property extent 'mouse-face 'highlight)
    (cond ((and extra
                (string= "... ..." (substring extra 0 7)))
           (setq face 'p4-depot-branched-face))
          ((string= "delete" action)
           (let ((print-client-file (p4-make-action-closure 'p4-print
                                                            depot-file))
                 (undo-delete (p4-make-action-closure 'p4-confirm-undo-delete
                                                      depot-file)))
             (setq face 'p4-depot-deleted-face)
             (p4-set-action extent 'default print-client-file)
             (p4-set-action extent 2 print-client-file)
             (p4-set-action extent 3 undo-delete)))
          ((string-match "\\(add\\|branch\\)\\(ed\\)?" action)
           (setq face 'p4-depot-added-face))
          ((string= "edit" action)
           (p4-set-action extent
                          3 ; mouse button 3
                          (p4-make-action-closure 'p4-ediff-client-file
                                                  depot-file))))
    (p4-set-extent-property extent 'face face)))

(defun p4-mark-match-with-face (face)
  "Mark matched text with FACE."
  (let ((extent (p4-create-extent (match-beginning 0) (match-end 0))))
    (p4-set-extent-property extent 'face face)))

(defconst p4-mark-permission-denied "^.+: Permission denied$")
(defun p4-mark-permission-denied ()
  "Mark matched text with `p4-permission-denied-face'."
  (p4-mark-match-with-face 'p4-permission-denied-face))

(defconst p4-mark-opened-not-changed "^.+ - is opened and not being changed$")
(defun p4-mark-opened-not-changed ()
  "Mark matched text with `p4-opened-not-changed-face'."
  (p4-mark-match-with-face 'p4-opened-not-changed-face))

(defconst p4-mark-must-resolve "^.+ - must resolve .+ before submitting$")
(defun p4-mark-must-resolve ()
  (p4-mark-match-with-face 'p4-must-resolve-face))

(defconst p4-mark-open-for-edit
  "^\\(.*\\) - is opened for edit and can't be replaced$")
(defun p4-mark-open-for-edit ()
  "Mark matched text with `p4-open-for-edit-face'."
  (p4-mark-match-with-face 'p4-open-for-edit-face))

(defconst p4-mark-cannot-clobber "^Can't clobber writable file .+$")
(defun p4-mark-cannot-clobber ()
  "Mark matched text with `p4-cannot-clobber-face'."
  (p4-mark-match-with-face 'p4-cannot-clobber-face))

(defvar p4-must-sync-before "^.+ - must sync before .+\.$")
(defun p4-must-sync-before ()
  "Mark matched text with `p4-must-sync-before-face'."
  (p4-mark-match-with-face 'p4-must-sync-before-face))

(defun p4-replace-with-first-capture ()
  "Replace the matched text with the first capture."
  (replace-match "\\1" 'fixedcase))

(defconst p4-match-all-but-trailing-cr "^\\(.*\\)$")

(defconst p4-match-all-but-trailing-whitespace "^\\(.*[^ 	]\\)[ 	]+$")

(defconst p4-pathname-with-revision
  "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[^/@# ][^/@#]*/[^#\n]*\\)"
  "Matches pathnames in common output buffers.
Matches forward slash-delimited pathnames, optionally followed by `#' and a
revision number.  The pathname must appear at the start of a line or immediately
following three periods or equal signs and a space at the start of a line.")

(defun p4-activate-client-pathnames (buffer &optional regexp)
  "Make depot filenames active in BUFFER.
Make clicking on depot filenames open the corresponding workspace file.

BUFFER is the buffer to activate.
REGEXP is the regular expression which provides client pathnames in BUFFER in
   capture number 2.  That is, after matching REGEXP, calling `match-string'
   with an argument of 2 returns the client pathname, if any.  If nil, use
   `p4-pathname-with-revision'."
  (let (args
        files)
    (or regexp
        (setq regexp p4-pathname-with-revision))
    (set-buffer buffer)
    ;; collect all client pathnames to get depot mappings (via a single call)
    (goto-char (point-min))
    (while (p4-re-search regexp)
      (or (member (match-string 2) args)
          (setq args (cons (match-string 2) args))))
    (setq files (p4-map-depot-files args))
    ;; find each client pathname and activate it
    (goto-char (point-min))
    (while (p4-re-search regexp)
      (let* ((client-file (cdr (assoc (match-string 2) files)))
             (depot-file (match-string 2))
             (start (match-beginning 2))
             (end (match-end 2))
             (extent (p4-create-extent start end))
             ;; some kind of operation related to branching/integration
             (branching-op-p (and (match-string 1)
                                  (string-match "\\.\\.\\. \\.\\.\\..*"
                                                (match-string 1))))
             (plist (if (and client-file
                             (file-readable-p client-file))
                        (list 'link-client-name client-file)
                      (list 'link-depot-name depot-file))))
        (when branching-op-p
          (setq plist
                (append plist
                        (list 'history-for depot-file
                              'face 'p4-depot-branched-face))))
        (cond
         ((not client-file)
          (p4-set-extent-property extent 'face 'p4-depot-unmapped-face)
          (p4-set-extent-properties extent plist))
         ((save-excursion
            (goto-char end)
            (looking-at ".* deleted?[ \n]"))
          (p4-set-extent-property extent 'face 'p4-depot-deleted-face)
          (p4-set-extent-properties extent plist))
         ((save-excursion
            (goto-char end)
            (looking-at ".* \\(add\\|branch\\)\\(ed\\)?[ \n]"))
          (p4-create-active-link
           start end (append plist (list 'face 'p4-depot-added-face))))
         (t
          (p4-create-active-link start end plist)))))
    (use-local-map p4-opened-map)
    (p4-make-read-only buffer)
    (p4-move-buffer-point-to-top buffer)))

(defun p4-insert-no-properties (text)
  "Insert TEXT w/o inheriting properties from surrounding text.
This is needed for Xemacs because text inserted inherits properties."
  (let ((start (point))
        end)
    (insert text)
    (setq end (point))
    (set-text-properties start end nil)))

(defun p4-font-lock-buffer (buffer)
  (with-current-buffer (get-buffer buffer)
    (let ((first-line "")
          filename)
      (goto-char (point-min))
      (when (looking-at "^//[^#@]+/\\([^/#@]+\\)")
        (setq filename (match-string 1))
        (forward-line)
        (setq first-line (buffer-substring (point-min) (point)))
        (delete-region (point-min) (point)))
      (setq buffer-file-name filename)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (condition-case nil
          (font-lock-fontify-buffer)
        (error nil))
      (message "")
      (fundamental-mode)
      (p4-disable-hilit)
      (goto-char (point-min))
      (p4-insert-no-properties first-line))))

(defun p4-print-with-revision-history ()
  "Print a depot file with revision history to a buffer.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let ((args (p4-buffer-file-name-2)))
      (setq args (p4-get-args "print-revs" current-prefix-arg args args))
      (apply 'p4-print-rev-history args))))
(defalias 'p4-blame 'p4-print-with-revision-history)
(defalias 'p4-print-with-rev-history 'p4-print-with-revision-history)

(defun p4-print-rev-history (filespec)
  (when (p4-available-p)
    (let* (
           (filename filespec)
           (buffer
            (p4-make-command-buffer "print-with-rev-history" filename))
           ;; buffer of changelists which last modified each line of the file
           (change-buffer
            (get-buffer-create "*P4 print-with-rev-history change*"))
           ;; scratch output buffer
           (scratch (p4-make-output-buffer
                     "*P4 print-with-rev-history scratch*"))
           arguments
           cur-file ;; filename of current branch
           change
           change-alist
           depot-pathname
           head-rev)
      ;; blame constrained by change number?
      (if (string-match "\\(.*\\)@\\([0-9]+\\)" filespec)
          (setq filename (match-string 1 filespec)
                change (string-to-number (match-string 2 filespec)))
        ;; blame constrained by revision?
        (when (string-match "\\(.*\\)#\\([0-9]+\\)" filespec)
          (setq filename (match-string 1 filespec)
                head-rev (string-to-number (match-string 2 filespec)))))
      ;; ensure filespec is unambiguous
      (p4-exec-p4 "files" scratch filename)
      (p4-check-for-command-output "files" scratch filename)
      (with-current-buffer scratch
        (when (< 1 (count-lines (point-min) (point-max)))
          (error "File pattern maps to more than one file"))
        (erase-buffer))
      ;; process change history
      (setq change-alist
            (p4-get-change-history scratch filespec head-rev change))
      (when (> 1 (length change-alist))
        (error "Head revision not available"))
      (p4-annotate-changes scratch change-buffer change-alist)
      ;;(kill-buffer scratch)
      ;;(p4-apply-change-history buffer change-buffer change-alist)
      ;;(kill-buffer change-buffer)
      (p4-activate-client-pathnames buffer)
      (message "")
      (set-buffer buffer)
      (p4-make-read-only buffer)
      (setq truncate-lines t)         ; should we decide this?
      (use-local-map p4-print-rev-map))))

(defun p4-max-change-length (changes)
  "Compute the length of the largest changelist number in CHANGES.
CHANGES is an alist in which the keys are changelist numbers."
  (let ((largest 0)
        change)
    (while changes
      (setq change (caar changes)
            changes (cdr changes)
            largest (if (< largest change)
                        change
                      largest)))
    (length (number-to-string largest))))

(defun p4-annotate-changes (buffer change-buffer change-alist)
  (message "Annotating changes...")
  (let* ((base-change (number-to-string (caar change-alist)))
         (base-file (nth p4-FILE (cdar change-alist)))
         (changes change-alist)
         (arguments (list "-q" (concat base-file "@" base-change))))
    (apply 'p4-call-p4 "print" (cons change-buffer 'keep) 'update arguments)
    (with-current-buffer change-buffer
      (toggle-read-only 0)     ; we'll be modifying it henceforth
      (goto-char (point-min))
      (while (p4-re-search ".*\n")
        (replace-match (concat base-change "\n"))))
    (let* (change1
           change2
           file1
           file2
           ins-string)
      (while (< 1 (length changes))
        (setq change1 (caar changes)
              change2 (car (cadr changes))
              file1 (nth p4-FILE (cdar changes))
              file2 (nth p4-FILE (cdr (cadr changes)))
              ins-string (format "%d\n" change2)
              changes (cdr changes))
        (p4-apply-changelist-changes buffer
                                     change-buffer
                                     ins-string
                                     file1 change1
                                     file2 change2)))))

(defun p4-apply-changelist-changes (buffer
                                    change-buffer
                                    ins-string
                                    file1 change1
                                    file2 change2)
  (with-current-buffer buffer
    (erase-buffer))
  (message "Applying changes from changelist %d..." change2)
  (let ((arguments (list (format "%s@%d" file1 change1)
                         (format "%s@%d" file2 change2))))
    (apply 'p4-exec-p4 "diff2" buffer arguments)
    (apply 'p4-check-for-command-output "diff2" buffer arguments)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (while (p4-re-search-back p4-change-history-revision-regex)
        (let ((la (string-to-number (match-string 1)))
              (lb (string-to-number (match-string 2)))
              (op (match-string 3))
              (ra (string-to-number (match-string 4)))
              (rb (string-to-number (match-string 5)))
              start)
          (and (= 0 lb)
               (setq lb la))
          (and (= 0 rb)
               (setq rb ra))
          (cond ((string= op "a")
                 (setq la (1+ la)))
                ((string= op "d")
                 (setq ra (1+ ra))))
          (save-excursion
            (set-buffer change-buffer)
            (goto-line la)
            (setq start (point))
            (forward-line (1+ (- lb la)))
            (delete-region start (point))
            (while (<= ra rb)
              (insert ins-string)
              (setq ra (1+ ra)))))))))

(defun p4-apply-change-history (buffer change-buffer changes)
  "Apply change history annotations to BUFFER.
CHANGES is an alist of change information in which the keys are changelist
   numbers and the associated values are lists of the form
   (REVISION DATE AUTHOR FILE), where FILE is the current depot pathname of a
   branch."
  (let (
        ;; length of longest change number (as a string)
        (change-length (p4-max-change-length changes))
        (filespec (format "%s@%d" (nth p4-FILE (cdar changes)) (caar changes)))
        change-data
        blank-change
        change-format
        headings
        line
        previous-change
        change
        xth-revision
        xth-date
        xth-author
        xth-file)
    (setq
     ;; includes the spaces to insert when there is no changelist
     ;; shift; must account for the changelist number, revision number,
     ;; date, and author's name, plus the intervening spaces
     blank-change (concat (make-string (+ 1 change-length 1 4 1 10 1 8) 32)
                          "|")
     ;; the format string which accounts for the length of the longest
     ;; changelist number
     change-format (concat (format "%%%dd" change-length)
                           " %4d %10s %8s|")
     ;; column headings
     headings (format (concat (format "%%%ds" change-length)
                              "  Rev       Date   Author|\n")
                      "Change")
     ;; most revent change processed
     previous-change 0)
    ;; minimum length is 6 due to the Change heading's length
    (when (> 6 change-length)
      (setq change-length 6))
    (when (zerop (p4-call-command "print" (cons buffer 'keep) nil nil filespec))
      (p4-font-lock-buffer buffer)
      (save-excursion
        (set-buffer buffer)
        (goto-line 2)
        (move-to-column 0)
        (p4-insert-no-properties headings)
        (while (setq line (p4-read-depot-output change-buffer))
          (setq change (string-to-number line))
          (if (= change previous-change)
              (p4-insert-no-properties blank-change)
            ;; extract the change data from our alist (`eq' works for
            ;; integers so we can use assq here)
            (setq change-data (cdr (assq change changes))
                  xth-revision (nth p4-REVISION  change-data)
                  xth-date (nth p4-DATE change-data)
                  xth-author (nth p4-AUTHOR change-data)
                  xth-file (nth p4-FILE change-data))
            (p4-insert-no-properties
             (format change-format change xth-revision xth-date xth-author))
            (move-to-column 0)
            (when (looking-at p4-change-history-index-regex)
              (let ((nth-change (match-string 1))
                    (nth-revision (match-string 2))
                    (nth-user (match-string 4)))
                (p4-create-active-link (match-beginning 1)
                                       (match-end 1)
                                       (list 'change nth-change))
                ;; link revision to a file since we follow integrations/
                ;; branches
                (p4-create-active-link (match-beginning 2)
                                       (match-end 2)
                                       (list 'rev  nth-revision
                                             'link-depot-name xth-file))
                (p4-create-active-link (match-beginning 4)
                                       (match-end 4)
                                       (list 'user nth-user))
                ;; truncate username
                (let ((start (+ (match-beginning 4) 7))
                      (end (match-end 4)))
                  (when (> end start)
                    (delete-region start end))))))
          (setq previous-change change)
          (forward-line))))))

(defun p4-get-change-history (buffer filespec earliest-revision earliest-change)
  (let ((arguments (list "-i" filespec))
        current-file
        depot-pathname
        head-filespec ;; file spec of head revision
        found-earliest
        result)
    (message "Retrieving change history...")
    (apply 'p4-exec-p4 "filelog" buffer arguments)
    (apply 'p4-check-for-command-output "filelog" buffer arguments)
    (with-current-buffer-writeable buffer
      (setq depot-pathname (p4-read-depot-output buffer)
            current-file  depot-pathname
            head-filespec depot-pathname))
  ;; parse history
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (looking-at "^\\(//.*\\)$") ;; record current filename
          (setq current-file (match-string 1))
        ;; non-branch change line?
        (when (looking-at p4-change-history-change-regex)
          (let ((revision (string-to-number (match-string 1)))
                (change (string-to-number (match-string 2)))
                (op (match-string 3))
                (date (match-string 4))
                (author (match-string 5)))
            (cond
             ;; after the change constraint _for this file_ [note: branches
             ;; complicate this]
             ((and earliest-change
                   (< earliest-change change))
              nil)
             ;; after the revision constraint _for this file_ [note: branches
             ;; complicate this]
             ((and earliest-revision
                   (< earliest-revision revision)
                   (string= head-filespec current-file))
              nil)
             ;; file has been deleted, can't assign blame
             ((string= op "delete")
              (or found-earliest
                  (goto-char (point-max))))
             ;; change to process
             (t
              (setq result
                    (cons (cons change (list revision date author current-file))
                          result))
              ;; record head revision, if not seen already
              (or earliest-revision
                  (setq earliest-revision revision))
              (setq found-earliest t))))))
      (forward-line)))
  result))

(defun p4-make-basic-buffer (buffer &optional map)
  "Make BUFFER read-only and set its local map.
BUFFER is the buffer to modify.
MAP is the map to set on BUFFER, if non-nil.  Otherwise, set `p4-basic-map'.

Return the buffer."
  (with-current-buffer buffer
    (goto-char (point-min))
    (use-local-map (if (keymapp map)
                       map
                     p4-basic-map))
    (toggle-read-only 1)
    (p4-move-buffer-point-to-top buffer)
    buffer))

(defun p4-scroll-down-1-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line."
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line."
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window."
  (interactive)
  (scroll-down (- (window-height) next-screen-context-lines)))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window."
  (interactive)
  (scroll-other-window (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window ()
  "Scroll up one window."
  (interactive)
  (scroll-up (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window-other-w ()
  "Scroll other window up one window."
  (interactive)
  (scroll-other-window (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer."
  (interactive)
  (goto-char (point-min)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window."
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer."
  (interactive)
  (goto-char (point-max)))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window."
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-delete-other-windows ()
  "Make buffer full height."
  (interactive)
  (delete-other-windows))

(defun p4-goto-next-diff ()
  "Next diff."
  (interactive)
  (goto-char (window-start))
  (when (= (point) (point-max))
    (error "At bottom"))
  (forward-line)
  (p4-re-search "^====")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff."
  (interactive)
  (when (= (point) (point-min))
    (error "At top"))
  (goto-char (window-start))
  (p4-re-search-back "^====")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-file ()
  "Next file."
  (interactive)
  (goto-char (window-start))
  (when (= (point) (point-max))
    (error "At bottom"))
  (forward-line)
  (p4-re-search "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - ")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-file ()
  "Previous file."
  (interactive)
  (when (= (point) (point-min))
    (error "At top"))
  (goto-char (window-start))
  (p4-re-search-back "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - ")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-diff ()
  "Next diff."
  (interactive)
  (goto-char (window-start))
  (when (= (point) (point-max))
    (error "At bottom"))
  (forward-line)
  (p4-re-search "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff."
  (interactive)
  (when (= (point) (point-min))
    (error "At top"))
  (goto-char (window-start))
  (p4-re-search-back "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)")
  (set-window-start (selected-window) (point)))

(defun p4-moveto-print-rev-column (old-column)
  (let ((colon (save-excursion
                 (move-to-column 0)
                 (if (looking-at "[^:\n]*:")
                     (progn
                       (goto-char (match-end 0))
                       (current-column))
                   0))))
    (move-to-column old-column)
    (when (and (< (current-column) colon)
               (p4-re-search "[^ ][ :]"))
      (goto-char (match-beginning 0)))))

(defun p4-next-change-rev-line ()
  "Next change/revision line."
  (interactive)
  (let ((c (current-column)))
    (move-to-column 1)
    (p4-re-search "^ *[0-9]+ +[0-9]+[^:]+:")
    (p4-moveto-print-rev-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line."
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (move-to-column 32)
    (p4-re-search-back "^ *[0-9]+ +[0-9]+[^:]*:")
    (p4-moveto-print-rev-column c)))

(defun p4-toggle-line-wrap ()
  "Toggle line wrap mode."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (save-window-excursion
    (recenter)))

;; Actions
;;
;; Actions are functions associated with a text extent so that mouse clicks and
;; other triggers on that text can invoke the desired behavior.  A keymap
;; provides the means to translate user actions into a call to `p4-run-action'
;; which extracts the action associated with the text where the keymap event was
;; triggered.  `p4-set-action' sets the action for a particular key (as in
;; key/value pair, not keymap key, though mnemonic associations can help to keep
;; them straight) that `p4-run-action' seeks.
(defun p4-run-action (position &optional key)
  "Run the action described at POSITION, if found.
The `actions' text property at POSITION, if found, is an alist of functions to
call.

KEY is the key in the `actions' text property for the function to call, if
   non-nil.  If KEY is nil or omitted, then the `default' element, if present,
   or the first element, if not, is used.

If an action is found, and it is a function, it is called with POSITION.

Returns non-nil if an action was found and called."
  (let ((actions (get-char-property position 'actions)))
    (when actions
      (let ((action (assq (or key
                              'default)
                          actions)))
        ;; use first action if no 'default action or key
        (and (not action)
             (not key)
             (setq action (cdar actions)))
        (and action
             (setq action (cdr action))
             (funcall action position)
             t)))))

(defun p4-set-action (extent key action &rest args)
  "Set EXTENT's 'actions alist KEY association to ACTION or (ACTION . ARGS).
This establishes associations for `p4-run-action' to use to find the action to
run.

EXTENT is the text extent with which to associate the action.
KEY is the trigger key with which to associate the action.
ACTION is the function to call.
ARGS is the argument list with which to call ACTION, if non-nil.

KEY values 1, 2, and 3 are reserved for mouse buttons 1, 2, and 3.

`p4-set-action' creates a closure for calling the function since it embeds ARGS,
if non-nil, for `p4-run-action' to use when calling the function."
  (let* ((actions (p4-get-extent-property extent 'actions))
         (bundle action)
         (element (assq key actions)))
    (when args
      (setq bundle (cons action args)))
    (if element
        (setcdr element bundle)
      (setq actions (append actions (list (cons key bundle))))
      (p4-set-extent-property extent 'actions actions))))

(defun p4-make-action-closure (function &rest args)
   "Return a function taking one arg that calls FUNCTION with ARGS, if non-nil.
The returned function takes a single argument, ostensibly the position from
`p4-run-action', which it ignores.  The function argument invokes FUNCTION with
ARGS, if non-nil.

FUNCTION the function to call when the returned function is called.  FUNCTION
    must be a function taking the number of arguments in ARGS.
ARGS is the argument list to pass to FUNCTION, if non-nil."
   ;; If you have a better idea for generating a function taking one argument
   ;; that calls the supplied (possibly anonymous) function with the supplied
   ;; argument list, please submit a patch.  This is the result of research and
   ;; trial and error.
   (let (f)
     (fset 'f (cond ((symbolp function)
                    (symbol-function function))
                   ((and (listp function)
                         (eq 'lambda (car function)))
                    function)
                   (t
                    (error "Invalid function: %s" function))))
     (list 'lambda
          '(position)
          (if args
              (list 'apply
                    (list 'quote (symbol-function 'f))
                    (list 'quote (symbol-value 'args)))
            (list (symbol-function 'f))))))

(defun p4-buffer-on-enter (point)
  "React to hitting Enter in an Emacs-P4 buffer."
  (interactive "d")
  (p4-run-action point))

(defun p4-buffer-on-mouse-button-2 (event)
  "React to a mouse button 2 click (EVENT) in an Emacs-P4 buffer."
  (interactive "e")
  (p4-buffer-mouse-clicked event 2))

(defun p4-buffer-on-mouse-button-3 (event)
  "React to a mouse button 3 click (EVENT) in an Emacs-P4 buffer."
  (interactive "e")
  (p4-buffer-mouse-clicked event 3))

(defun p4-buffer-mouse-clicked (event button)
  "React to a mouse button click by calling the associated action, if any.
EVENT is the mouse click event information from which the associated window and
   buffer position is extracted.
BUTTON is the mouse button number which is the associated actions index."
  (select-window (p4-event-window event))
  (p4-run-action (p4-event-point event) button))

(defun p4-action-diff2 (event)
  (select-window (p4-event-window event))
  (p4-run-action (p4-event-point event) 'diff2))

(defun p4-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (let ((buffer (p4-call-print nil t depot-name)))
      (when buffer
        (p4-font-lock-buffer buffer)
        (p4-activate-client-pathnames buffer p4-pathname-with-revision-plus-)
        (other-window 1)))))

(defun p4-find-file-other-window (pathname)
  "Open the buffer currently visiting PATHNAME or visit it in the other window.
If a buffer is already open for PATHNAME, display that buffer in the other
window (unless it is the current buffer).  Otherwise, visit PATHNAME in the
other window.

PATHNAME is the pathname of the file to visit."
  (let ((buffer (find-buffer-visiting pathname)))
    (if buffer
        (unless (eq buffer (current-buffer))
          (switch-to-buffer-other-window buffer))
      (find-file-other-window pathname))))

(defun p4-action-filelog-short-format ()
  "Short format."
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-action-filelog-long-format ()
  "Long format."
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-goto-next-change ()
  "Next change."
  (interactive)
  (let ((c (current-column)))
    (forward-line)
    (while (get-char-property (point) 'invisible)
      (forward-line))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change."
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))

(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (p4-re-search regexp)
      (let ((extent (p4-create-extent (match-beginning 0) (match-end 0))))
        (p4-set-extent-property extent 'face face-property)))))

(defun p4-extent-properties-at (position)
  "Return all properties in all extents at POSITION."
  (let ((extents (p4-extents-at position))
        result)
    (while extents
      (setq result (append result
                           (p4-extent-properties (car extents)))
            extents (cdr extents)))
    result))

(defun p4-call-on-matches (buffer data &optional start end)
  "Look for lines in BUFFER matching patterns described in DATA and call back.
For each line in BUFFER, or a subset thereof, look for regular expressions in
DATA that match.  For each match, invoke the callback.  If the callback returns
non-nil, process no more regular expressions in DATA for the current line.
Otherwise, repeat with the next regular expression in DATA.

The callback can use `match-beginning', `match-end', etc. as with `looking-at'
to understand the matching line.

BUFFER as for `p4-get-buffer'.
DATA has the form of a list in which each element describes a regular
   expression to match, a function to call when a match is found, and arguments
   to pass to that function.  The elements can have one of three forms: SYMBOL,
   (SYMBOL ARGS), or (RE (FUNCTION ARGS)).
START is the starting point in BUFFER for searching, if non-nil
END is the ending point in BUFFER for searching, if non-nil

The first form of DATA elements, SYMBOL, is a symbol with a value that is the
regular expression to match, and a function definition that is called with no
arguments.  (By using both the value and function definition of SYMBOL, the two
are tightly coupled preventing the chance of coupling the wrong function with a
regular expression.  It also avoids the need to invent related symbol names for
the two values.)

The second form, (SYMBOL ARGS), is like the first form, but supplies arguments
with which to invoke the function.

The third form, (RE (FUNCTION ARGS)), is the most general form in which RE is
the regular expression to match, FUNCTION is the function to call, and ARGS is
the arguments list to pass to it.

Note that in the second and third forms, when ARGS is a list, the callback
function is called using `apply'."
  (save-excursion
    (set-buffer buffer)
    (goto-char (or start
                   (point-min)))
    (let ((last (or end
                    (point-max)))
          (normalized (p4-normalize-on-matches-data data))
          (sol (point))                 ; start of line
          d                             ; mutable ref to data
          element)
      (while (< sol last)
        (setq d normalized)
        (while d
          (setq element (car d)
                d (cdr d))
          (when (p4-call-if-line-matches (car element)
                                         (cadr element)
                                         (cddr element))
            (setq d nil))               ; skip remaining elements
          (goto-char sol))              ; callbacks can move point
        (forward-line)
        (setq sol (point))))))

(defun p4-normalize-on-matches-data (data)
  "Normalize `p4-call-if-matches' data to streamline its use.
Return a list with elements of the form RE (FUNCTION ARGS) based upon any of the
three forms allowed by `p4-call-if-matches'."
  (let (args
        element
        function
        one
        regexp
        result
        two)
    (while data
      (setq element (car data)
            data (cdr data))
      (if (symbolp element)               ; SYMBOL
          (setq function (symbol-function element)
                regexp (symbol-value element))
        (unless (listp element)
          (error "Not a symbol or list: %s" element))
        (setq one (car element)
              two (cdr element))
        (cond ((stringp one)              ; (RE (FUNCTION ARGS))
               (setq args (cdr two)
                     function (car two)
                     regexp one))
              ((eq 'quote one)            ; (quote SYMBOL)
               (setq two (car two)
                     function (symbol-function two)
                     regexp (symbol-value two)))
              ((symbolp one)              ; (SYMBOL ARGS)
               (setq args two
                     function (symbol-function one)
                     regexp (symbol-value one)))
              (t
               (error "Invalid element: %s" element))))
      (setq result (append result (list (cons regexp (cons function args))))))
    result))

(defun p4-call-if-line-matches (regexp function args)
  "Check whether current line matches a regexp and invoke callback if so.
ARGS has one of three forms as described for `DATA' in `p4-call-on-matches'."
  (when (looking-at regexp)
    (if args
        (if (listp args)
            (apply function args)
          (funcall function args))
      (funcall function))))

(defvar p4-mark-change-log
  (concat "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?" ; revision if p4 filelog
          "[Cc]hange \\([0-9]+\\) "     ; changelist
          "\\([a-z]+\\)?"               ; action, if present
          ".*on.*by \\([^ @]+\\)@\\([^ \n]+\\).*\n" ; username@client
          "\\(\n\\(.+\n\\)*\n\\)"))     ; description
(defun p4-mark-change-log ()
  "Mark parts of change log buffer with relevant actions."
  (let* ((CHANGELIST 3)
         (CLIENT 6)
         (DESCRIPTION 7)
         (REVISION 2)
         (USERNAME 5))
    (when (match-beginning REVISION)
      (p4-set-action (p4-create-extent (match-beginning REVISION)
                                       (match-end REVISION))
                     'default
                     (p4-make-action-closure
                      '(lambda (revision)
                         (let* ((previous (number-to-string
                                           (1- (string-to-number revision))))
                                (prefix (concat (p4-buffer-file-name-2) "#"))
                                (filename1 (concat prefix previous))
                                (filename2 (concat prefix revision)))
                           (p4-ediff2 filename1 filename2)))
                      (match-string REVISION))))
    (p4-set-action (p4-create-extent (match-beginning CHANGELIST)
                                     (match-end CHANGELIST))
                   'default
                   (p4-make-action-closure 'p4-describe
                                           (match-string CHANGELIST)))
    (p4-set-action (p4-create-extent (match-beginning CLIENT)
                                     (match-end CLIENT))
                   'default
                   (p4-make-action-closure 'p4-client (match-string CLIENT)))
    (p4-set-action (p4-create-extent (match-beginning USERNAME)
                                     (match-end USERNAME))
                   'default
                   (p4-make-action-closure 'p4-user (match-string USERNAME)))
    (let ((extent (p4-create-extent (match-beginning DESCRIPTION)
                                    (match-end DESCRIPTION))))
      (p4-set-extent-property extent 'invisible t)
      (p4-set-extent-property extent 'isearch-open-invisible t))))

(defun p4-activate-change-log-buffer (buffer)
  (p4-activate-client-pathnames buffer)
  (with-current-buffer-writeable buffer
    (p4-call-on-matches buffer '('p4-mark-change-log))
    (p4-activate-change-numbers buffer (point-min) (point-max))
    (use-local-map p4-change-log-map)
    (setq buffer-invisibility-spec nil)
    (goto-char (point-min))))

(defvar p4-active-change-numbers "\\(changes?\\|submit\\|p4\\)[:#]?[ \t\n]+")
(defun p4-active-change-numbers ()
  (let ((regexp (concat "\\([#@]\\|number\\|no\\.\\|\\)[ \t\n]*"
                        "\\([0-9]+\\)[-, \t\n]*"
                        "\\(and/or\\|and\\|&\\|or\\|\\)[ \t\n]*")))
  (while (looking-at regexp)
    (p4-set-action (p4-create-extent (match-beginning 2) (match-end 2))
                   'default
                   (p4-make-action-closure 'p4-describe (match-string 2)))
    (goto-char (match-end 0)))))

(defun p4-activate-change-numbers (buffer start end)
  "Make change number references from START to END in BUFFER clickable."
  (p4-call-on-matches buffer
                      '('p4-active-change-numbers)
                      start
                      end))

(defun p4-activate-diff-buffer (buffer)
  (p4-activate-client-pathnames buffer)
  (with-current-buffer-writeable buffer
    (when p4-colorized-diffs
      (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
      (p4-buffer-set-face-property "^[@*].*" 'p4-diff-head-face)
      (p4-buffer-set-face-property "^\\([+>].*\n\\)+" 'p4-diff-inserted-face)
      (p4-buffer-set-face-property "^\\([-<].*\n\\)+" 'p4-diff-deleted-face)
      (p4-buffer-set-face-property "^\\(!.*\n\\)+" 'p4-diff-changed-face))
    (goto-char (point-min))
    (while (p4-re-search "^\\(==== //\\).*\n")
      (let* ((link-client-name
              (get-char-property (match-end 1) 'link-client-name))
             (link-depot-name (get-char-property (match-end 1) 'link-depot-name))
             (start (match-beginning 0))
             (end (save-excursion
                    (if (p4-re-search "^==== ")
                        (match-beginning 0)
                      (point-max))))
             (extent (p4-create-extent start end)))
        (when link-client-name
          (p4-set-extent-property extent 'block-client-name link-client-name))
        (when link-depot-name
          (p4-set-extent-property extent 'block-depot-name link-depot-name))))
    (goto-char (point-min))
    (while (p4-re-search
            (concat "^[@0-9][^cad+]*\\([cad+]\\)\\([0-9]*\\).*\n"
                    "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)"))
      (let* ((first-line (string-to-number (match-string 2)))
             (start (match-beginning 3))
             (end (match-end 3))
             (extent (p4-create-extent start end)))
        (p4-set-extent-property extent 'first-line first-line)
        (p4-set-extent-property extent 'start start)))
    (goto-char (point-min))
    (let ((stop
           (if (p4-re-search "^\\(\\.\\.\\.\\|====\\)")
               (match-beginning 0)
             (point-max))))
      (p4-activate-change-numbers buffer (point-min) stop))
    (goto-char (point-min))
    (when (looking-at "^Change [0-9]+ by \\([^ @]+\\)@\\([^ \n]+\\)")
      (let ((USERNAME 1)
            (CLIENT 2)
            cur-user cur-client)
        (setq cur-user (match-string USERNAME))
        (setq cur-client (match-string CLIENT))
        (p4-create-active-link (match-beginning USERNAME)
                               (match-end USERNAME)
                               (list 'user cur-user))
        (p4-create-active-link (match-beginning CLIENT)
                               (match-end CLIENT)
                               (list 'client cur-client))))
    (use-local-map p4-diff-map)))

(defun p4-regexp-create-links (buffer-name regexp property)
  (with-current-buffer-writeable buffer-name
    (goto-char (point-min))
    (while (p4-re-search regexp)
      (let ((start (match-beginning 1))
            (end (match-end 1))
            (str (match-string 1)))
        (p4-create-active-link start end (list property str))))))

(defun p4-command-line-flags-p (&rest args)
  "Return non-nil if ARGS includes any command flags.
Flags are whitespace delimited words beginning with a hyphen."
  (let (result
        (head (car args))
        (tail (cdr args)))
    (while (and (not result)
                head)
      (if (string-match "^-" head)
          (setq result t)
        (setq head (car tail))
        (setq tail (cdr tail))))))

(defun p4-filter-out (list predicate)
  "Return a list containing all elements of LIST that do not satisfy PREDICATE.
PREDICATE is a function called with each element of LIST, in turn."
  (let (result)
    (while list
      (unless (funcall predicate (car list))
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (reverse result)))

(defun p4-check-status (status message command &rest args)
  "Report an error with MESSAGE if STATUS indicates p4 COMMAND ARGS failed.
MESSAGE is passed to `error', so it should start with a capital letter and *not*
end with a period."
  (apply 'p4-report-signal status command args)
  ;; if p4-report-signal doesn't call error, but status is a string, then status
  ;; describes a quit signal and there's no need to call error
  (and (numberp status)
       (not (zerop status))
       (error message)))

(defun p4-file-login (filename)
  "Log into P4 using the contents of FILENAME as the password."
  (let* ((command "login")
         (args "-a")
         (status (call-process (p4-get-p4-executable)
                               filename
                               nil
                               nil
                               command
                               args)))
    (p4-check-status status "Unable to log in" command args)))

(defun p4-password-login (password)
  "Log into P4 using PASSWORD."
  (let* ((command "login")
         (args "-a")
         (status))
    (with-temp-buffer
      (insert password)
      (setq status (call-process-region
                    (point-min)
                    (point-max)
                    (p4-get-p4-executable)
                    nil
                    nil
                    nil
                    command
                    args)))
    (p4-check-status status "Unable to log in" command args)))

;; comint-read-no-echo might be a good choice for reading the password without
;; echoing it, but can we count on it being available?  I certainly didn't want
;; to duplicate it here.
(defun p4-prompt-login (password)
  "Prompt for PASSWORD and then log into P4.
WARNING: The password is entered as clear text in the minibuffer."
  (interactive (list (read-from-minibuffer
                      (concat "P4 password for "
                              (p4-get-current-variable "P4USER")
                              " : "))))
  (p4-password-login password))

(defun p4-call-list-command (command &optional prompt initial buffer)
  "Run p4 COMMAND in an output buffer with active links.
PROMPT triggers prompting for user input for COMMAND args, if non-nil.
INITIAL is initial value of args when prompting.
BUFFER is the buffer to hold the output, if non-nil.  Otherwise, a buffer named
   for COMMAND will be created."
  (when (p4-available-p)
    (let* ((args (p4-get-args command prompt initial))
           (buffer (or buffer
                       (p4-make-command-buffer command))))
      (apply 'p4-call-command command buffer 'signal-errors nil args)
      (p4-check-for-command-output command buffer args)
      (and (buffer-live-p buffer)
           (progn
             (p4-make-basic-buffer buffer)
             (p4-call-on-matches buffer '('p4-must-sync-before)))))))

(defun p4-call-list-command-with-links (command property regexp
                                                &optional
                                                prompt initial)
  "Run p4 COMMAND with active links in an output buffer.
The output buffer is created by calling `p4-make-command-buffer' with COMMAND.

PROPERTY is the active link property to set to the first submatch found by
   REGEXP.
REGEXP is the regular expression to match when creating active links in the
   output buffer.
PROMPT triggers prompting for user input for COMMAND args, if non-nil.
INITIAL is initial value of args when prompting."
  (let ((buffer (p4-make-command-buffer command)))
    (p4-call-list-command command prompt initial buffer)
    (and (buffer-live-p buffer)
         (p4-regexp-create-links buffer regexp property))))

(defun p4-call-file-command (command prompt query callback &rest args)
  "Run p4 COMMAND on some file(s).
Create an output buffer named for COMMAND and run p4 COMMAND to populate it.
The output buffer will be marked unmodified and read only after invoking the
CALLBACK, if any.

ARGS is the default argument list for p4 COMMAND.
PROMPT means to prompt the user for the command arguments, if non-nil.  ARGS
   will be offered as the default.
QUERY means to prompt the user to confirm the action, if non-nil.  If QUERY is a
   string, then it is used to form a prompt of the form

      \"Really QUERY FILENAME?\"

   for each file.  If QUERY is a function, then it is called for each file.  If
   the user answers no or the function returns nil for any file, then the
   command is aborted.
CALLBACK as for `p4-call-command'."
  (let ((args (p4-get-args command prompt args args))
        (function query)
        buffer)
    (and query
         (stringp query)
         (setq function (lambda (filename)
                          (yes-or-no-p
                           (concat "Really " query " " filename "? ")))))
    (when (or (not query)
              (let ((arguments args)
                    (continue t)
                    filename)
                (while arguments
                  (setq filename (car arguments)
                        arguments (cdr arguments))
                  (when (file-exists-p filename)
                    (unless (funcall function filename)
                      (setq continue nil
                            arguments nil))))
                continue))
      (setq buffer (p4-make-command-buffer command args))
      (apply 'p4-call-command command buffer 'signal-errors nil args)
      (when (buffer-live-p buffer)
        (cond ((not callback)
               t)
              ((functionp callback)
               (funcall callback buffer))
              ((listp callback)
               (apply (car callback) buffer (cdr callback))))
        (p4-check-mode)
        (p4-refresh-p4-files)
        (p4-make-read-only buffer)))))

(defun p4-optional-file-command (command &optional callback)
  "Run p4 COMMAND, possibly on some file(s).
The list of files comes, by default, from `p4-get-file-names'.  If the prefix
argument is non-nil, prompt the user for the command arguments.

Setting the prefix arg means to prompt for files.

CALLBACK as for `p4-call-file-command'."
  (when (p4-available-p)
    (apply 'p4-call-file-command
           command current-prefix-arg nil callback (p4-get-file-names))))

(defun p4-file-command (command &optional wildcard query callback &rest args)
  "Run p4 COMMAND ARGS on some file(s).
The list of files comes, by default, from `p4-get-file-names'.  If the prefix
argument is set or `p4-get-file-names' finds no file(s), then prompt the user
for the command arguments.

Setting the prefix arg means to prompt for files.

WILDCARD, if non-nil, is the wildcard to add to client workspace pathnames, in
   the list of files, that are directories.
QUERY as for `p4-call-file-command'.
CALLBACK as for `p4-call-file-command'."
  (when (p4-available-p)
    (let* ((filenames (p4-get-file-names))
           (prompt (or current-prefix-arg
                       (not filenames))))
      (when wildcard
        (unless (string= "/" (substring wildcard 0 1))
          (setq wildcard (concat "/" wildcard)))
        (setq filenames (mapcar (function (lambda (filename)
                                            (if (p4-directory-p filename)
                                                (concat filename wildcard)
                                              filename)))
                                filenames)))
      (setq args (if args
                     (append args filenames)
                   filenames))
      (apply 'p4-call-file-command command prompt query callback args))))

(defun p4-create-or-edit-form (command &optional regexp &rest args)
  "Run p4 COMMAND ARGS to create a form or open one for editing.
Setting the prefix arg means to prompt for arguments.

REGEXP is the regular expression to match in the text, if non-nil.  Point will
follow the matching text."
  (when (p4-available-p)
    (setq args (p4-useful-list-p args))
    (let* ((buffer-name (p4-buffer-name command))
           (buffer (get-buffer buffer-name))
           prompt)
      (when buffer
        (switch-to-buffer-other-window buffer)
        (when (y-or-n-p (concat "Replace existing " command " form? "))
          (setq buffer nil)))
      (unless buffer
        (setq prompt (concat command " -o")
              args (p4-get-args prompt current-prefix-arg args args command))
        (if (apply 'p4-command-line-flags-p args)
            (apply 'p4-call-command command buffer-name 'signal-errors nil args)
          (apply 'p4-call-form-command command buffer-name regexp args))))))

(defun p4-edit-specification (command type &rest args)
  "Edit a p4 COMMAND ARGS specification.
If ARGS is nil or empty, signal error that \"TYPE must be specified!\"

TYPE is the description of the specification for the error message.
ARGS is the set of arguments to pass to p4 COMMAND."
  (when (p4-available-p)
    (if (or (null args)
            (equal args (list "")))
        (error "%s must be specified!" type)
      (if (apply 'p4-command-line-flags-p args)
          (apply 'p4-call-command
                 (p4-buffer-name command)
                 'signal-errors
                 nil
                 args)
        (apply 'p4-call-form-command
               command
               (p4-buffer-name command nil (car (reverse args)))
               "Description:\n\t"
               args)))))

(defp4cmd p4-annotate () "annotate"
  "Show file(s) with revision indicators in a new buffer.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (let ((options (p4-string-to-list p4-default-annotate-options)))
    (apply 'p4-file-command
           "annotate"
           "*"
           nil
           (function
            (lambda (buffer)
              (p4-call-on-matches buffer
                                  '((p4-match-all-but-trailing-cr
                                     . (p4-replace-with-first-capture nil))
                                    (p4-match-all-but-trailing-whitespace
                                     . (p4-replace-with-first-capture nil))))))
           options)))

(eval-and-compile
  (defp4cmd p4-edit () "edit"
    "Open the current depot file(s) for edit.
If in a Dired buffer, use the marked files or the current file if none are
marked.

Setting the prefix arg means to prompt for arguments.

SHOW as described for `p4-call-command'."
    (interactive)
    (p4-file-command "edit" "...")
    (when buffer-file-name
      (p4-revert-buffer)))

  (defp4cmd p4-revert () "revert"
    "Revert all changes in the opened file(s).
Setting the prefix arg means to prompt for arguments.

SHOW as described for `p4-call-command'."
    (interactive)
    (p4-file-command "revert" nil (and (p4-differences-p)
                                   "revert changes in"))
    (p4-revert-buffer 'preserve-modes))
  )

(defp4cmd p4-reopen () "reopen"
  "Change the type or changelist number of the opened file(s).
Setting the prefix arg means to prompt for arguments.

SHOW as described for `p4-call-command'."
  (interactive)
  (p4-file-command "reopen"))

(defp4cmd p4-lock () "lock"
  "Lock opened file(s) against changelist submission.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-optional-file-command "lock"))

(defp4cmd p4-unlock () "unlock"
  "Release, but leave open, locked file(s).
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-optional-file-command "unlock"))

(defp4cmd p4-users () "users"
  "Display list of known P4 users via p4 users.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-call-list-command-with-links "users"
                                   'user
                                   "^\\([^ ]+\\).*\n"
                                   current-prefix-arg
                                   "user"))

(defp4cmd p4-groups () "groups"
  "Display list of known groups via p4 groups.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-call-list-command-with-links "groups"
                                   'group
                                   "^\\(.*\\)\n"
                                   current-prefix-arg
                                   "group"))

(defp4cmd p4-jobs () "jobs"
  "Display list of jobs via p4 jobs.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-call-list-command "jobs" current-prefix-arg))

(defp4cmd p4-fix () "fix"
  "Mark jobs as being fixed by a changelist number."
  (interactive)
  (when (p4-available-p)
    (let ((args (p4-prompt-for-args "fix" nil "job")))
      (apply 'p4-call-command "fix" nil 'signal-errors nil args))))

(defp4cmd p4-fixes () "fixes"
  "List which changelists fix which jobs.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-call-list-command "fixes" current-prefix-arg))

(defp4cmd p4-where () "where"
  "Show how local file names map into depot names via p4 where.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-optional-file-command "where"))

(defp4cmd p4-diff () "diff"
  "Diff the current file and the depot revision opened for edit.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let* ((filename (p4-buffer-file-name-2)))
      (when filename
        (p4-save-some-files filename))
      (p4-call-diff-get-args p4-default-diff-options
                             "-f"
                             filename))))

(defp4cmd p4-diff2 (version1 version2) "diff2"
  "Display diff of two depot files.
Setting the prefix arg means to prompt for arguments."
  (interactive
   (let ((rev (get-char-property (point) 'rev))
         (filename (p4-buffer-file-name-2)))
     (when (and (not rev)
                filename)
       (let ((rev-num 0))
         (setq rev (p4-file-revision nil filename))
         (when rev
           (setq rev-num (string-to-number rev)))
         (if (> rev-num 1)
             (setq rev (number-to-string (1- rev-num)))
           (setq rev nil))))
     (list (p4-read-arg-string "First depot file or revision # to diff: " rev)
           (p4-read-arg-string "Second depot file or revision # to diff: "))))
  (when (p4-available-p)
    ;; look for revision number or depot file
    (let* ((filename (p4-buffer-file-name-2))
           (diff-revision1 (p4-get-file-revision filename version1))
           (diff-revision2 (p4-get-file-revision filename version2))
           (diff-options (p4-get-args "diff2 optional args"
                                      current-prefix-arg
                                      p4-default-diff2-options
                                      (p4-string-to-list
                                       p4-default-diff2-options))))
      (apply 'p4-call-diff2 diff-revision1 diff-revision2 diff-options))))

(defp4cmd p4-diff-head () "diff-head"
  "Display diff of current file against the head revision in the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let ((filename (p4-buffer-file-name-2)))
      (p4-save-some-files filename)
      (p4-call-diff-get-args p4-default-diff-options
                             (concat filename "#head")))))

(defp4cmd p4-diff-revision (revision) "diff"
  "Diff the current file and REVISION from the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive "sRevision to compare against: ")
  (when (p4-available-p)
    (let ((filename (p4-buffer-file-name-2)))
      (if (not filename)
          (message
           "This file is not in the depot; cannot compare it to another revision")
        (p4-save-some-files filename)
        (p4-call-diff-get-args p4-default-diff-options
                               "-f"
                               (format "%s#%s" filename revision))))))

(defp4cmd p4-add () "add"
  "Add file(s) to the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-file-command "add")
  (setq p4-mode " P4:+")
  (p4-force-mode-line-update))

(defp4cmd p4-delete () "delete"
  "Delete file(s) from the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-file-command "delete" "..."))

(defp4cmd p4-filelog () "filelog"
  "View a history of the changes made to the file(s).
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let* ((command "filelog")
           (filenames (p4-get-file-names))
           (args (p4-get-args command current-prefix-arg filenames filenames)))
      (apply 'p4-create-change-log command args))))

(defp4cmd p4-files () "files"
  "List file(s) in the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let* ((command "files")
           (filenames (p4-get-file-names))
           (args (p4-get-args command current-prefix-arg filenames filenames))
           (buffer (apply 'p4-make-command-buffer command args)))
      (apply 'p4-call-command command buffer 'signal-errors nil args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (p4-activate-change-numbers buffer (point-min) (point-max)))
        (p4-activate-client-pathnames buffer)))))

(defp4cmd p4-refresh () "sync"
  "Run p4 sync -f on a filespec, refreshing the contents if unopened.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let ((args (p4-buffer-file-name)))
      (setq args (append (list "-f")
                         (p4-get-args "sync -f" current-prefix-arg args)))
      (apply 'p4-call-simple-p4-command "sync" t args))))

(defp4cmd p4-have () "have"
  "List revisions last gotten.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-optional-file-command "have"))

(defp4cmd p4-changes () "changes"
  "List changes for the selected file(s) using p4 changes.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (let* ((files (p4-get-file-names))
         (args (append (p4-string-to-list p4-default-changes-options)
                       (or files
                           (list (concat default-directory "...")))))
         (command "changes"))
    (setq args (p4-get-args command current-prefix-arg args args))
    (apply 'p4-create-change-log command args)))

(defp4cmd p4-help (&optional command) "help"
  "Display output of p4 help COMMAND.
Setting the prefix arg means to prompt for arguments."
  (interactive (p4-string-to-list
                (p4-read-arg-string
                 "Help on which command (or none for general help): "
                 nil
                 "help")))
  (when (p4-available-p)
    (let ((general (not command))
          buffer)
      (setq buffer (p4-make-command-buffer "help" command))
      (p4-call-command "help" buffer 'signal-errors nil command)
      (when (buffer-live-p buffer)
        (unless general
          (p4-check-for-command-output "help" buffer command))
        (p4-make-basic-buffer buffer)))))

(defp4cmd p4-info () "info"
  "Display client/server information."
  (interactive)
  (when (p4-executable-p)
    (let ((buffer (p4-make-command-buffer "info")))
      (p4-call-command "info" buffer 'signal-errors)
      (and (buffer-live-p buffer)
           (p4-make-basic-buffer buffer)))))

(defp4cmd p4-integrate () "integrate"
  "Schedule integrations between branches."
  (interactive)
  (p4-call-list-command "integrate" 'prompt "-b "))
(defalias 'p4-integ 'p4-integrate)      ; backward compatibility

(defp4cmd p4-resolve () "resolve"
  "Merge open files with other revisions or files.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let* ((command "resolve")
           (buffer-name (p4-buffer-name command))
           (args (p4-get-args command current-prefix-arg))
           (buffer (get-buffer buffer-name)))
      (and (buffer-live-p buffer)
           (not (comint-check-proc buffer))
           (save-excursion
             (let ((current-directory default-directory))
               (set-buffer buffer)
               (cd current-directory)
               (goto-char (point-max))
               (insert "\n--------\n\n"))))
      (setq args (append (list "-d" default-directory command) args))
      (setq buffer (apply 'make-comint
                          "p4 resolve"
                          (p4-get-p4-executable)
                          nil
                          args))
      (set-buffer buffer)
      (comint-mode)
      (display-buffer buffer)
      (select-window (get-buffer-window buffer))
      (goto-char (point-max)))))

(defp4cmd p4-rename () "rename"
  "Rename file(s) in the depot.
Executes the p4 integrate and p4 delete commands automatically.
Commit the change(s) using `p4-submit'."
  (interactive)
  (when (p4-available-p)
    (let ((filenames (p4-get-file-names)))
      (if filenames
          (apply 'p4-rename-files filenames)
        (let* ((from (p4-read-arg-string "rename from: "))
               (to (p4-read-arg-string "rename to: ")))
          (p4-rename-one-file from to))))))

(defun p4-rename-files (&rest filenames)
  "Rename FILENAMES in the depot, prompting for new names for each.
Commit the changes using `p4-submit'."
  (let ((filename (car filenames))
        prompt
        to)
    (while filename
      (setq prompt (concat "rename " filename " to: ")
            to (p4-read-arg-string prompt filename))
      (condition-case nil
          (p4-rename-one-file filename to)
        (error (setq filenames nil)))
      (setq filenames (cdr filenames)
            filename (car filenames)))))

(defun p4-rename-one-file (from to)
  "Rename FROM to TO in the depot.
Commit the change using `p4-submit'."
  (if (string= from to)
      (error "Cannot rename \"%s\" to itself" from)
    (let ((buffer (p4-make-command-buffer "rename"))
          success)
      (p4-call-command "integrate"
                       (cons buffer 'keep)
                       'signal-errors
                       nil
                       from
                       to)
      ;; stupidly, p4 returns 0 even when it complains about no permissions
      (save-excursion
        (goto-char (point-min))
        (setq success
              (looking-at "^.+ - branch/sync from ")))
      (if success
          (p4-exec-p4 "delete" buffer from)
        (error "Rename failed from \"%s\" to \"%s\"" from to)))))

(defp4cmd p4-describe (&optional changelist) "describe"
  "Show the output of p4 describe CHANGELIST or prompt for a changelist number.
Setting the prefix arg or omitting CHANGELIST means to prompt for the changelist
number.

CHANGELIST is the changelist number to describe, if non-nil."
  (interactive)
  (let ((prompt (or current-prefix-arg
                    (not changelist)))
        (args p4-default-describe-options))
    (when changelist
      (when (numberp changelist)
        (setq changelist (number-to-string changelist)))
      (setq args (concat args " " changelist)))
    (setq args (p4-get-args "describe" prompt args args))
    (apply 'p4-call-describe args)))

(eval-and-compile
  (defp4cmd p4-branch (&rest args) "branch"
    "Run p4 branch ARGS."
    (interactive (p4-prompt-for-args "branch" nil "branch"))
    (apply 'p4-edit-specification "branch" "Branch" args))
  )

(defp4cmd p4-branches () "branches"
  "List all branches."
  (interactive)
  (p4-call-list-command-with-links "branches"
                                   'branch
                                   "^Branch \\([^ ]+\\).*\n"))

(defp4cmd p4-change () "change"
  "Create or edit a change specification.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-create-or-edit-form "change" "Description:\n\t"))

(eval-and-compile
  (defp4cmd p4-client (&optional client) "client"
    "Create or edit a client specification.
Setting the prefix arg means to prompt for arguments."
    (interactive)
    (p4-create-or-edit-form "client" "\\(Description\\|View\\):\n\t" client))
  )

(defun p4-client-delete (&optional client)
  "Delete a client specification.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (when (p4-available-p)
    (let ((args client)
          (command "client")
          buffer
          error-message)
      (setq args (list "-d"
                       (p4-get-args "client -d"
                                    (or (not client)
                                        current-prefix-arg)
                                    client
                                    client
                                    command))
            buffer (p4-make-command-buffer command args))
      (condition-case error-description
          (apply 'p4-call-command command buffer 'signal-errors nil args)
        (error
         (setq error-message (cadr error-description))))
      (when (buffer-live-p buffer)
        (p4-display-output buffer))
      (when error-message
        (error "%s" error-message)))))

(defp4cmd p4-clients () "clients"
  "List all clients."
  (interactive)
  (p4-call-list-command-with-links "clients" 'client "^Client \\([^ ]+\\).*\n"))

(eval-and-compile
  (defp4cmd p4-label (&rest args) "label"
    "Create or edit a p4 label specification."
    (interactive (p4-prompt-for-args "label" nil "label"))
    (apply 'p4-edit-specification "label" "Label" args))
  )

(defp4cmd p4-labels () "labels"
  "Display list of defined labels."
  (interactive)
  (p4-call-list-command-with-links "labels" 'label "^Label \\([^ ]+\\).*\n"))

(defp4cmd p4-labelsync () "labelsync"
  "Synchronize a label with the current client contents."
  (interactive)
  (let* ((args (p4-prompt-for-args "labelsync"))
         (buffer (apply 'p4-make-command-buffer "labelsync" args)))
    (apply 'p4-call-command "labelsync" buffer 'signal-errors nil args)
    (and (buffer-live-p buffer)
         (p4-activate-client-pathnames buffer))))

(defp4cmd p4-logout () "logout"
  "Log out of P4."
  (interactive)
  ;; Logging in before logging out prevents error messages and ensures any
  ;; outstanding tickets are cleared.
  (p4-call-login)
  (let* ((command "logout")
         (status (call-process (p4-get-p4-executable) nil nil nil command)))
    (p4-check-status status "Unable to log out" command)))

(defp4cmd p4-login () "login"
  "Log into P4.
The password is given by the P4PASSWD environment variable, the password file
named in `p4-password-file', or by user input."
  (interactive)
  (p4-call-login))

(defp4cmd p4-opened () "opened"
  "Display list of files opened for pending change.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (let* ((args (p4-get-args "opened" current-prefix-arg (p4-get-file-names)))
         (label (concat "-c "
                        (p4-current-client)))
         (buffer (apply 'p4-make-command-buffer label "opened" args)))
    (apply 'p4-call-opened buffer args)))

(eval-and-compile
  (defp4cmd p4-print (&optional depot-name) "print"
    "Print a depot file to a buffer.
Setting the prefix arg means to prompt for arguments.

DEPOT-NAME is the depot pathname of the file to print, if non-nil.  Otherwise,
   the current buffer's file is printed."
    (interactive)
    (when (p4-available-p)
      (let ((args (or depot-name
                      (p4-buffer-file-name-2))))
        (setq args (p4-get-args "print" current-prefix-arg args args))
        (apply 'p4-call-print nil t args))))
  )

(defp4cmd p4-submit (&optional changelist) "submit"
  "Submit a pending change to the depot or switch to existing submit buffer.
Setting the prefix arg means to prompt for arguments.

CHANGELIST is the change list number to submit, if non-nil."
  (interactive "P")
  (when (p4-available-p)
    (let* ((buffer-name (p4-buffer-name "submit"))
           (buffer (get-buffer buffer-name))
           args)
      (if buffer
          (switch-to-buffer-other-window buffer)
        (and current-prefix-arg
             (interactive-p)
             (or (listp changelist)
                 (and (numberp changelist)
                      (> 10 changelist)))
             (setq changelist nil))
        (when changelist
          (when (integerp changelist)
            (setq changelist (int-to-string changelist)))
          (setq args (list "-c" (int-to-string changelist))))
        (setq args (p4-get-args "submit" current-prefix-arg args args))
        (setq args (p4-filter-out args
                                  (function (lambda (x)
                                              (string= x "-c")))))
        (p4-save-opened-files)
        (and (or (not (and p4-check-empty-diffs
                           (p4-empty-diff-p)))
                 (progn
                   (ding t)
                   (yes-or-no-p
                    "File with empty diff opened for edit. Submit anyway? ")))
             (setq buffer (get-buffer-create buffer-name))
             ;; use p4 change to get console output to edit in a buffer
             (when (apply 'p4-call-form-command
                          "change"
                          buffer
                          "Description:\n\t"
                          args)
               (with-current-buffer buffer
                 ;; pretend it was p4 submit and not p4 change
                 (setq p4-form-command "submit"))))))))

(defp4cmd p4-group () "group"
  "Create or edit a group specification.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-create-or-edit-form "group"))

(defp4cmd p4-job () "job"
  "Create or edit a job.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-create-or-edit-form "job" "Description:\n\t"))

(defp4cmd p4-jobspec () "jobspec"
  "Edit the job template.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-create-or-edit-form "jobspec"
                          "# A Perforce Job Spec Specification.\n#\n"))

(defp4cmd p4-set () "set"
  "Reports all p4 settings currently in effect."
  (interactive)
  (and (p4-available-p)
       (p4-call-command "set" nil 'signal-errors)))

(defp4cmd p4-tickets () "tickets"
  "Displays any available p4 login tickets."
  (interactive)
  (and (p4-available-p)
       (p4-call-command "tickets" nil 'signal-errors)))

(defp4cmd p4-sync () "sync"
  "Synchronize the current workspace with the depot.
Setting the prefix arg means to prompt for arguments."
  (interactive)
  (p4-call-sync))
(defalias 'p4-get 'p4-sync)             ; backward compatibility

(eval-and-compile
  (defp4cmd p4-user (&optional user) "user"
    "Create or edit a user specification for USER.
Setting the prefix arg means to prompt for arguments.

USER is the P4 username, if non-nil.  Otherwise, use the current user."
    (interactive)
    (p4-create-or-edit-form "user" "Reviews:\n\t" user))
  )

(defun p4-branch-edit (&optional name)
  "Create or edit a branch specification named NAME.
Prompts the user for the branch name if not supplied.

NAME is the name of the branch to create or edit, if non-nil."
  (interactive (p4-prompt-for-args "branch name" nil "branch"))
  (p4-edit-specification "branch" "Branch" name))

(defun p4-list-opened-files (buffer &rest args)
  "List files opened on current client in BUFFER using p4 opened ARGS.
BUFFER as for `p4-get-buffer'.

Return non-nil if any files are listed in BUFFER."
  (let ((buffer (p4-get-buffer buffer))
        result)
    (apply 'p4-call-command
           "opened"
           (cons buffer 'keep)
           'signal-errors
           nil
           args)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (unless (and (= 1 (p4-count-lines))
                     (looking-at "File(s) not opened on this client."))
          (setq result t))))
    result))

(defun p4-open-client-file (pathname)
  "Open the client workspace file corresponding to the depot PATHNAME."
  (let ((filename (p4-map-depot-to-client pathname 'required)))
    (p4-find-file-other-window filename)))

(defun p4-diff-client-file (pathname)
  "Display p4 diff output between the head revision and client workspace file.
PATHNAME is the depot pathname of the client workspace file to compare."
  (p4-call-for-client-file 'p4-diff-head pathname))

(defun p4-ediff-client-file (pathname)
  "Ediff the the head revision and client workspace file for PATHNAME.
PATHNAME is the depot pathname of the client workspace file to compare."
  (let ((filename (p4-map-depot-to-client pathname 'required)))
    (p4-ediff-files filename (concat pathname "#head"))))

(defun p4-call-for-client-file (function pathname)
  "Call FUNCTION on the client workspace file corresponding to PATHNAME.
If there is no buffer visiting the client workspace file corresponding to
PATHNAME, then the file will be visited and then killed after running FUNCTION
with that buffer current.

PATHNAME is the depot pathname of the client workspace file to visit before
   calling FUNCTION."
  (let* ((filename (p4-map-depot-to-client pathname 'required))
         (buffer (find-buffer-visiting filename))
         (was-open buffer))
    (unless was-open
      (setq buffer (find-file filename)))
    (with-current-buffer buffer
      (funcall function))
    (unless was-open
      (kill-buffer buffer))))

(defun p4-open-client-or-depot-file (pathname)
  "Open the file corresponding to the depot PATHNAME.
If there is a client workspace file corresponding to the depot PATHNAME, visit
that file using `p4-find-file-other-window'.  Otherwise, print the depot file,
if the user wishes.

PATHNAME is the pathname of the file to visit."
  (let* ((filename (p4-map-depot-to-client pathname))
         buffer)
    (if filename
        (p4-find-file-other-window filename)
      (when (and pathname
                 (y-or-n-p (format "No client file for %s; display depot file? "
                                   pathname)))
        (p4-print pathname)))))

(defun p4-not-in-depot ()
  "List the files in the client workspace but not in the depot.
All files in the client workspace that are not in the depot are reported, by
default. `p4-not-in-depot-files' is a string with file globbing patterns of
files to check."
  (interactive)
  (when (p4-available-p)
    (let* ((command "files")
           (values (p4-string-to-list p4-not-in-depot-files))
           (buffer (apply 'p4-make-command-buffer command values))
           args
           value)
      ;; expand the wildcard patterns into a list of files
      (while values
        (setq args (append args (file-expand-wildcards (car values)))
              values (cdr values)))
      ;; remove directories, backup files, and duplicates
      (setq values args
            args nil)
      (while values
        (setq value (car values))
        (unless (or (equal "~" (substring value -1))
                    (member value args)
                    (not (not (car (file-attributes value)))))
          (setq args (append args (list value))))
        (setq values (cdr values)))
      ;; now call p4 files
      (apply 'p4-call-command
             command
             (cons buffer 'keep)
             (cons 'signal-errors 'merge-stderr)
             nil
             args)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((lines (p4-count-lines))
              end
              start)
          (when (< 1 lines)
            (while (p4-re-search "^//[^#]+#[^ ]+ - [^ ]+ change [1-9].*$")
              (setq start (match-beginning 0)
                    end (match-end 0))
              (if (< (point-min) start)
                  (setq start (1- start))
                (setq end (1+ end)))
              (delete-region start end))))
        (if (< 0 (p4-count-lines))
            (p4-display-output buffer)
          (kill-buffer buffer)
          (message "All files are in the depot"))))))

(defun p4-out-of-sync ()
  "List the files available in the depot but not in the client workspace."
  (interactive)
  (when (p4-available-p)
    (let* ((command "sync")
           (args (list "-n"))
           (buffer (apply 'p4-make-command-buffer command args))
           (current 't)
           (current-message "File(s) up-to-date."))
      (apply 'p4-call-command
             command
             (cons buffer 'keep)
             'signal-errors
             nil
             args)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((lines (p4-count-lines))
              end
              start)
          (when (or (> 1 lines)
                    (not (looking-at current-message)))
            (setq current nil)
            (while (p4-re-search "^\.\.\. .+$")
              (delete-region (1- (match-beginning 0)) (match-end 0)))
            (goto-char (point-min))
            (while (p4-re-search " - .*$")
              (setq start (match-beginning 0)
                    end (match-end 0))
              (unless (string-match " - is opened" (match-string 0))
                (delete-region start end)))
            (p4-display-output buffer))))
      (when current
        (message current-message)))))

(defun p4-sync-check ()
  "Run p4 sync -n.
Setting the prefix argument means to prompt for arguments."
  (interactive)
  (p4-call-sync "-n"))

(defun p4-sync-file ()
  "Synchronize the current buffer's file, if any, with the depot."
  (interactive)
  (p4-call-sync (p4-buffer-file-name))
  (revert-buffer 'ignore-auto))

(defun p4-sync-directory ()
  "Run p4 sync on everything in the current buffer's directory."
  (interactive)
  (p4-sync-directory-with "*"))

(defun p4-sync-directory-recursively ()
  "Run p4 sync on everything in and under the current buffer's directory."
  (interactive)
  (p4-sync-directory-with "..."))

(defun p4-sync-directory-with (wildcard)
  (let ((directory (p4-buffer-directory)))
    (if directory
        (p4-call-sync (concat directory wildcard))
      (error "No directory associated with this buffer"))))

(defalias 'p4-sync-workspace 'p4-sync)

(defun p4-old-revision-opened ()
  "List P4 files opened at a revision other than the current head."
  (interactive)
  (when (p4-available-p)
    (let ((buffer (p4-make-output-buffer "P4: Old Revisions Opened"))
          (prefix "")
          filename
          filespecs
          have
          head
          keyword
          show-output
          start)
      (with-temp-buffer
        (when (p4-list-opened-files (current-buffer))
          (goto-char (point-min))
          (while (p4-re-search "^\\(.+\\)#[0-9]+ - .+$")
            (setq filespecs (append filespecs (list (match-string 1)))))
          (setq start (point))
          (apply 'p4-depot-output "fstat" 'no-error "-Rn" filespecs)
          (goto-char start)
          (while (p4-re-search
                  "^\.\.\. \\(depotFile\\|haveRev\\|headRev\\) \\(.+\\)$")
            (setq keyword (match-string 1))
            (cond ((string= "depotFile" keyword)
                   (setq filename (match-string 2)))
                  ((string= "haveRev" keyword)
                   (setq have (match-string 2)))
                  ((string= "headRev" keyword)
                   (setq head (match-string 2))))
            (and filename
                 have
                 head
                 (with-current-buffer buffer
                   (insert prefix filename ": open=" have "; head=" head)
                   (setq filename nil
                         have nil
                         head nil
                         prefix "\n" ; insert newline before lines after 1st
                         show-output t)))))) ; flag logic at end of function
      (if show-output
          (p4-display-output buffer)
        (kill-buffer buffer)
        (p4-alert "No old files opened")))))

(defun p4-set-p4client (name)
  "Set the current value of P4CLIENT.
This will change the current client from the previous client to the new given
value.

Setting this value to nil will disable Emacs-P4 version checking unless there
is a P4CONFIG configuration file or until a new client name is set.

`p4-set-p4client' will complete any client names set using the function
`p4-set-my-clients'.  The strictness of completion will depend on the variable
`p4-strict-client-completion' (default is t).

Argument NAME is the new client name to set.  The default value is the current
client."
  (interactive
   (list (completing-read "Change client to: "
                          (if p4-my-clients
                              p4-my-clients
                            'p4-clients-completion)
                          nil p4-strict-client-completion (p4-current-client))))
  (if (or (null name)
          (equal name "nil"))
      (progn
        (setenv "P4CLIENT" nil)
        (p4-confirm-version-check-enabled-by-config "client name"))
    (setenv "P4CLIENT" name)
    (message "P4CLIENT changed to %s" name)
    (run-hooks 'p4-change-client-hooks)))

(defun p4-display-p4client ()
  "Display the current value of the environment variable P4CLIENT.
This will be the current client that is in use for access through Emacs-P4."
  (interactive)
  (p4-report-both-variable-values "P4CLIENT"
                                  (p4-current-client)
                                  (getenv "P4CLIENT")))

(defun p4-set-p4config (config)
  "Set P4CONFIG variable to CONFIG.
P4CONFIG is a flexible mechanism wherein p4 will find current configuration
settings automatically by checking the config file found in the current
buffer's directory or a parent thereof."
  (interactive "sP4CONFIG: ")
  (p4-set-p4-evar "P4CONFIG" config))

(defun p4-display-p4config ()
  "Display the current value of the environment variable P4CONFIG.
P4CONFIG names a file that p4 will look for in the current directory or one of
its parent directories to find overrides of configuration variables in the
environment."
  (interactive)
  (let ((config (getenv "P4CONFIG")))
    (if config
        (message "P4CONFIG is %s" config)
      (message "P4CONFIG is not set"))))

(defun p4-set-my-clients (client-list)
  "Set the client completion list used by `p4-set-p4client'.
CLIENT-LIST is the new client list to use.  Disables `p4-set-p4client' client
completion if nil."
  (setq p4-my-clients nil)
  (let (p4-tmp-client-var)
    (while client-list
      (setq p4-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq p4-my-clients (append p4-my-clients
                                  (list (list p4-tmp-client-var)))))))

(defun p4-display-p4port ()
  "Display the current and environment values of P4PORT."
  (interactive)
  (p4-report-current-and-environment-variable "P4PORT"))

(defun p4-set-p4port (port)
  "Display the current value of P4PORT.
This will change the current server from the previous server to the new
given value.

Argument PORT The new server:port to set to.  The default value is
the current value of P4PORT."
  (interactive (list (let
                         ((symbol (read-string "Change server:port to: "
                                               (getenv "P4PORT"))))
                       (if (equal symbol "")
                           (getenv "P4PORT")
                         symbol))))
  (if (or (null port)
          (equal port "nil"))
      (progn
        (setenv "P4PORT" nil)
        (p4-confirm-version-check-enabled-by-config "server:port"))
    (setenv "P4PORT" port)
    (message "P4PORT changed to %s" port)))

(defun p4-display-p4user ()
  "Display the current and environment values of P4USER.
The value is defined in the configuration file, if any, or the environment.  If
P4USER is not set, then check `USER' and `USERNAME' in the environment."
  (interactive)
  (p4-report-both-variable-values "P4USER"
                                  (p4-get-user)
                                  (getenv "P4USER")))

(defun p4-get-user ()
  "Return the current p4 user."
  (interactive)
  (let ((user (or (p4-get-current-variable "P4USER")
                  (getenv "USER")
                  (getenv "USERNAME"))))
    user))

(defun p4-set-p4user (user)
  "Set the value of P4USER to USER in the environment of this Emacs."
  (interactive "sP4USER: ")
  (p4-set-p4-evar "P4USER" user))

(defun p4-get-current-variable (name)
  "The current value of the NAME variable as recognized by p4."
  (let ((config-file (p4-config-file)))
    (if (not config-file)
        (getenv name)
      (p4-get-config-info config-file name))))

(defun p4-report-both-variable-values (name current environment)
  "Display the CURRENT and ENVIRONMENT values of NAME."
  (message "Current %s is %s; %s e-var is %s" name current name environment))

(defun p4-report-current-and-environment-variable (name)
  "Displays the current and environment values of NAME."
  (let ((current (p4-get-current-variable name))
        (environment (getenv name)))
    (p4-report-both-variable-values name current environment)))

(defun p4-set-p4-evar (name value)
  "Set and report setting NAME to VALUE in this Emacs's environment.
If VALUE is nil, the empty string, or the current value, NAME is not changed."
  (if (or (null value)
          (equal value "")
          (equal value (getenv name)))
      (message "%s not changed" name)
    (setenv name value)
    (message "%s changed to %s" name value)))

(defun p4-find-file-hook ()
  "Configure buffer for Emacs-P4 support if the file is controlled by P4.
If the file is controlled by P4 and `p4-use-p4config-exclusively' is nil, run
function `p4-check-mode'."
  ;; We require one of the e-vars, P4CONFIG or P4CLIENT, to be set for Emacs-P4
  ;; support.  If neither is set, the user doesn't intend to use P4 and there's
  ;; nothing for us to do.  It won't do to cache that those e-vars are set as
  ;; they can be changed while Emacs is running.
  (and p4-hook-find-file
       (or (getenv "P4CONFIG")
           (getenv "P4CLIENT"))
       (p4-check-mode)))
(add-hook 'find-file-hooks 'p4-find-file-hook)

(defsubst p4-track-p4-file-and-buffer (file buffer)
  "Tracks FILE and BUFFER as being controlled by Emacs-P4.
BUFFER is assumed to be a buffer."
  (add-to-list 'p4-all-p4-buffer-files (list file buffer)))

(defun p4-forget-p4-file-and-buffer (file buffer)
  "Stops tracking FILE and BUFFER as being controlled by Emacs-P4.
BUFFER as in `get-buffer'."
  (let ((buffer (get-buffer buffer)))
    (setq p4-all-p4-buffer-files
          (delete (list file buffer) p4-all-p4-buffer-files)))
  (unless p4-all-p4-buffer-files
    (p4-cancel-timer p4-file-refresh-timer)
    (setq p4-file-refresh-timer nil)))

(defun p4-set-p4notify (notify &optional supress-status)
  "Set the value of P4NOTIFY.
This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

NOTIFY is the new value of the notification list.
Optional argument SUPRESS-STATUS, if non-nil, means suppress display of the
status message."
  (interactive (list (let
                         ((symbol (read-string
                                   "Change Notification List to: "
                                   p4-notify-list)))
                       (if (equal symbol "")
                           nil
                         symbol))))
  (let ((p4-old-notify-list p4-notify-list))
    (setenv "P4NOTIFY" notify)
    (setq p4-notify-list notify)
    (setq p4-notify (not (null notify)))
    (or supress-status
        (message "Notification list changed from '%s' to '%s'"
                 p4-old-notify-list
                 p4-notify-list))))

(defun p4-display-p4notify ()
  "Display the current value of P4NOTIFY.
This will be the current notification list that is in use for mailing
change notifications through Emacs-P4."
  (interactive)
  (message "P4NOTIFY is %s" p4-notify-list))

(defun p4-notify (users)
  "Manually notify a list of users of a change submission.
The user list is USERS or, if nil, the current notification list.

To do auto-notification, set the notification list with `p4-set-p4notify'
and on each submission, the users in the list will be notified of the
change.

Since this uses the sendmail program, you must set the correct path to the
sendmail program in the variable `p4-sendmail-program' and the user's email
address in the variable `p4-user-email'."
  (interactive (list (let
                         ((symbol (read-string "Notify whom? "
                                               p4-notify-list)))
                       (if (equal symbol "")
                           nil
                         symbol))))
  (p4-set-p4notify users t)
  (if (and p4-sendmail-program
           p4-user-email)
      (save-excursion
        (if (and p4-notify-list
                 (not (equal p4-notify-list "")))
            (save-excursion
              (set-buffer (p4-make-output-buffer))
              (goto-char (point-min))
              (if (re-search-forward "[0-9]+.*submitted" (point-max) 'noerror)
                  (let ((matched-change (substring (match-string 0) 0 -10))
                        (notify-buffer (p4-make-command-buffer "notify")))
                    (set-buffer notify-buffer)
                    (delete-region (point-min) (point-max))
                    (call-process-region (point-min)
                                         (point-max)
                                         (p4-get-p4-executable)
                                         t
                                         t
                                         nil
                                         "-d"
                                         default-directory
                                         "describe"
                                         "-s"
                                         matched-change)
                    (switch-to-buffer notify-buffer)
                    (goto-char (point-min))
                    (let (subject)
                      (if (re-search-forward "^Change.*$" (point-max) 'noerror)
                          (setq subject (match-string 0))
                        (setq subject (concat
                                           "Notification of Change "
                                           matched-change)))
                      (goto-char (point-min))
                      (insert
                       "From: " p4-user-email "\n"
                       "To: P4 Notification Recipients:;\n"
                       "Subject: " subject "\n")
                      (call-process-region (point-min)
                                           (point-max)
                                           p4-sendmail-program
                                           t
                                           t
                                           nil
                                           "-odi"
                                           "-oi"
                                           p4-notify-list)

                      (kill-buffer nil)))
                (kill-buffer (p4-pop-output-buffer))
                (p4-alert "No change submissions found")))
          (p4-alert "Notification list (`p4-notify-list') not set")))
    (cond (p4-sendmail-program
           (error "Variable `p4-user-email' not set"))
          (p4-user-email
           (error "Variable `p4-sendmail-program' not set"))
          (t
           (error
            "Variables not set: `p4-sendmail-program' and `p4-user-email'")))))

(defun p4-emacs-version ()
  "Return the current Emacs-P4 Integration version."
  (interactive)
  (let ((version (format (concat (when p4-running-xemacs "X")
                                 "Emacs-P4 Integration v%s")
                         p4-emacs-version-number)))
    (if (interactive-p)
        (message version)
      version)))

(defun p4-config-file ()
  "Return the P4CONFIG configuration file, if any."
  (let ((p4config (getenv "P4CONFIG"))
        (directory (file-truename (p4-buffer-directory))))
    (when p4config
      (let (found
            at-root)
        (while (not (or found
                        at-root))
          (let ((parent-dir (file-name-directory
                             (directory-file-name
                              directory)))
                (filename (concat directory p4config)))
            (when (file-exists-p filename)
              (setq found filename))
            (setq at-root (string-equal parent-dir directory))
            (setq directory parent-dir)))
        found))))

(defun p4-confirm-version-check-enabled-by-config (missing)
  (unless (p4-config-file)
    (message (concat "Emacs-P4 version check disabled.  Set a valid "
                     missing
                     " to enable."))))

(defun p4-get-add-branch-files (&rest args)
  (with-temp-buffer
    (when (apply 'p4-depot-output "opened" t args)
      (let (files
            depot-map)
        (goto-char (point-min))
        (while (p4-re-search "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - add ")
          (setq files (cons (cons (match-string 1) "Add")
                            files)))
        (goto-char (point-min))
        (while (p4-re-search "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - branch ")
          (setq files (cons (cons (match-string 1) "Branch")
                            files)))
        (setq depot-map (p4-map-depot-files (mapcar 'car files)))
        (mapcar (function (lambda (x) (cons (cdr (assoc (car x) depot-map))
                                            (cdr x)))) files)))))

(defun p4-get-have-files (&rest args)
  (with-temp-buffer
    (when (and (apply 'p4-depot-output "have" 'no-error args)
               (p4-count-lines))
      (let (line
            result
            depot-map
            file)
        (while (setq line (p4-read-depot-output (current-buffer)))
          (when (string-match "^\\(//[^/@#]+/[^#\n]*\\)#\\([0-9]+\\) - " line)
            (setq result (cons (cons (match-string 1 line)
                                     (match-string 2 line))
                               result))))
        (setq depot-map (p4-map-depot-files (mapcar 'car result)))
        (when result
          (setq result (mapcar (function (lambda (x)
                                           (cons (cdr (assoc (car x) depot-map))
                                                 (cdr x)))) result))
          (while args
            (setq file (car args))
            (setq args (cdr args))
            (unless (assoc file result)
              (setq result (cons (cons file nil) result)))))
        result))))

(defun p4-file-revision (&optional file-mode-cache filename)
  "The P4 revision number of a file if it is controlled by Emacs-P4.
FILE-MODE-CACHE is an alist of files recently scanned and their revisions.
   Reusing such an alist avoids repeated checks of the depot.
FILENAME is the file to check, if non-nil.  Use the current buffer's filename,
   if nil.

Return nil if FILENAME is nil and there is no filename associated with the
   current buffer.
Otherwise, return the associated revision if the file is in FILE-MODE-CACHE.
Otherwise, call p4 have for the file and return the indicated revision.
If no revision is found by the foregoing, check whether p4 opened recognizes
   the filename."
  (let (result)
    (unless filename
      (setq filename (p4-buffer-file-name-2)))
    (when filename
      (let ((element (assoc filename file-mode-cache)))
        (if element
            (setq result (cdr element))
          (when filename
            (with-temp-buffer
              (when (p4-depot-output "have" 'no-error filename)
                (let ((line (p4-read-depot-output (current-buffer))))
                  (when (string-match "^//[^/@#]+/[^#\n]*#\\([0-9]+\\) - " line)
                    (setq result (match-string 1 line))))))
            (unless (or result
                        file-mode-cache)
              (setq file-mode-cache (p4-get-add-branch-files filename))
              (setq result (cdr (assoc filename file-mode-cache)))
              (unless (numberp result)
                (setq result nil)))))))
    result))

(defun p4-fstat (&optional filename)
  "Return an alist of fstat values for FILENAME.
FILENAME is the file to check, if non-nil.  Use the current buffer's filename,
   if nil.

Return an alist of p4 fstat FILENAME output.  The ellipses are ignored.  Boolean
values are given a non-nil value when set and omitted when not set.  All other
values are strings."
  (when (p4-available-p)
    (unless filename
      (setq filename (p4-buffer-file-name)))
    (when filename
      (with-temp-buffer
        (let (result)
          (when (p4-depot-output "fstat" 'no-error filename)
            (goto-char (point-min))
            (while (p4-re-search "^\\.\\.\\. \\([^ ]+\\) \\(.+\\)?$")
              (setq result (cons (if (match-string 2)
                                     (cons (match-string 1) (match-string 2))
                                   (cons (match-string 1) t))
                                 result))))
          result)))))

(defun p4-get-fstat-value (key &optional filename)
  "The p4 fstat FILENAME value named KEY.
FILENAME is the file to check, if non-nil.  Use the current buffer's filename,
   if nil.

Return nil if FILENAME is nil and there is no filename associated with the
   current buffer or the file isn't controlled by P4.
Otherwise, return the value as a string."
  (cdr (assoc key (p4-fstat filename))))

(defun p4-have-revision (&optional filename)
  "Report the P4 revision number of FILENAME's working file.
FILENAME is the file to check, if non-nil.  Use the current buffer's filename,
   if nil.

Return nil if FILENAME is nil and there is no filename associated with the
   current buffer or the file isn't controlled by P4.
Otherwise, return the revision number as a string."
  (interactive)
  (let ((revision (p4-get-fstat-value "haveRev" filename)))
    (when revision
      (message revision))))

(defun p4-head-revision (&optional filename)
  "Report the head P4 revision number of FILENAME.
FILENAME is the file to check, if non-nil.  Use the current buffer's filename,
   if nil.

Return nil if FILENAME is nil and there is no filename associated with the
   current buffer or the file isn't controlled by P4.
Otherwise, return the revision number as a string."
  (interactive)
  (let ((revision (p4-get-fstat-value "headRev" filename)))
    (when revision
      (message revision))))

(defun p4-check-mode (&optional file-mode-cache)
  "Check to see whether this buffer should be configured for Emacs-P4 support.
Adding Emacs-P4 support includes the following actions:
  - Attach the P4 menu.
  - Put the P4 revision number in the mode line.
  - Inhibit backups if p4-allow-backups is non-nil.
  - Call the hooks in the variable `p4-mode-hook' with no args.

FILE-MODE-CACHE is an alist of files recently scanned to avoid repeated checks
   of the depot."
  (setq p4-mode nil)
  (and (not (p4-dired-p))
       (p4-available-p)
       (setq p4-revision (p4-file-revision file-mode-cache))
       (progn
         (p4-menu-add)
         (setq p4-mode (concat " P4:" p4-revision))
         (p4-force-mode-line-update)
         (p4-track-p4-file-and-buffer (p4-buffer-file-name) (current-buffer))
         (and (not p4-file-refresh-timer)
              (< 0 p4-file-refresh-timer-period)
              (setq p4-file-refresh-timer
                    (p4-create-timer 'p4-refresh-p4-files
                                     p4-file-refresh-timer-period
                                     'repeat)))
         (unless p4-allow-backups
           (set (make-local-variable 'backup-inhibited) t))
         (run-hooks 'p4-mode-hook)
         p4-revision)))

(defun p4-refresh-buffer (buffer)
  "Refresh BUFFER if the file it is visiting has changed.
BUFFER as described for `set-buffer'."
  (with-current-buffer buffer
    (let ((file (p4-buffer-file-name)))
      (when file
        (if p4-auto-refresh
            (or (buffer-modified-p buffer)
                (verify-visited-file-modtime buffer)
                ;; buffer hasn't been modified or reflects the file it visits
                (if (file-readable-p file)
                    (p4-revert-buffer)
                  (p4-check-mode)))
          (if (file-readable-p file)
              (find-file-noselect file t)
            (p4-check-mode)))
        (toggle-read-only (if (file-writable-p file)
                              0
                            1))))))

(defun p4-refresh-p4-files (&optional unused)
  "Refresh buffers for all files under P4 version control."
  (interactive)
  (let ((refresh (function (lambda (file buffer)
                             (p4-refresh-buffer buffer)))))
    (p4-run-in-file-buffers refresh)))

(defun p4-check-mode-all-p4-buffers ()
  "Call `p4-check-mode' for all buffers under P4 version control."
  (let (cache
        refresh)
    (when p4-all-p4-buffer-files
      (let ((branch-files (p4-get-add-branch-files))
            (have-files (apply 'p4-get-have-files
                               (mapcar 'car p4-all-p4-buffer-files))))
        (when (or branch-files
                  have-files)
          (setq cache (append branch-files have-files)))))
    (setq refresh (function (lambda (file buffer modes)
                              (set-buffer buffer)
                              (p4-check-mode modes))))
    (p4-run-in-file-buffers refresh cache)))

(defun p4-run-in-file-buffers (function &rest args)
  "Call FUNCTION in all buffers for the files in the Emacs-P4 controlled buffers.
Each element in `p4-all-p4-buffer-files' is assumed to be of the form
\(filename buffer-or-name\).  For each filename/buffer-or-name pair, get the
buffer for buffer-or-name, and if that buffer is live, call FUNCTION with three
arguments: filename, buffer, and ARGS.  Otherwise pass filename and the buffer
to `p4-forget-p4-file-and-buffer' to remove them from the known files.

All processing is within a call to `save-excursion'."
  (let ((files  p4-all-p4-buffer-files)
        filename
        buffer
        element)
    (save-excursion
      (while files
        (setq element (car files)
              files (cdr files)
              filename (car element)
              buffer (get-buffer (cadr element)))
        (if (buffer-live-p buffer)
            (apply function filename buffer args)
          (p4-forget-p4-file-and-buffer filename buffer))))))

(defun p4-toggle-hook-find-file ()
  "Toggle the check for P4 control of files being visited.
Disable when the P4 server is not available or when working off-line."
  (interactive)
  (setq p4-hook-find-file (not p4-hook-find-file))
  (message (concat "Emacs-P4 find-file hook " (if p4-hook-find-file
                                              "enabled."
                                            "disabled."))))

(defun p4-toggle-universal-argument ()
  "Toggles the value of the prefix argument."
  (interactive)
  (if current-prefix-arg
      (setq prefix-arg nil)
    (universal-argument)))

;; Wrap C-x C-q to allow p4-edit/revert and to prevent interfering with
;; vc-toggle-read-only.
(defun p4-toggle-read-only (&optional arg)
  "Toggle read-only mode for current buffer and the depot file.
Enable `p4-offline-mode' and disable `p4-mode' if ARG is non-nil.  Then, toggle
between `p4-edit' and `p4-revert', if `p4-mode' is non-nil.  Finally, toggle
between making the file writable and read-only if `p4-offline-mode' is non-nil."
  (interactive "P")
  (when (and arg
             p4-mode)
    (setq p4-mode nil
          p4-offline-mode t))
  (cond (p4-mode
         (if buffer-read-only
             (p4-edit)
           (p4-revert)))
        (p4-offline-mode
         (toggle-read-only)
         (when buffer-file-name
           (let ((mode (file-modes buffer-file-name)))
             (if buffer-read-only
                 (setq mode (logand mode (lognot 128)))
               (setq mode (logior mode 128)))
             (set-file-modes buffer-file-name mode))))))

(defun p4-browse-web-page ()
  "Browse the p4.el web page."
  (interactive)
  (require 'browse-url)
  (browse-url p4-web-page))

(defun p4-bug-report ()
  "Compose a bug report e-mail.
Appends the most recent ten lines from Emacs' *Messages* (Xemacs' *Message-Log*)
buffer output."
  (interactive)
  (let ((boundary (concat (make-string 65 ?*) "\n"))
        (divider (concat (make-string 65 ?_) "\n"))
        (subject (concat "BUG REPORT: " (p4-emacs-version))))
    (p4-compose-mail p4-emacs-maintainer subject)
    (goto-char (point-min))
    (p4-re-search (concat "^" (regexp-quote mail-header-separator) "\n"))
    (insert
     "Consider posting this report at SourceForge rather than sending\n"
     "it.  Just copy and paste the message body after creating a new bug\n"
     "report on SourceForge.\n\n"
     boundary
     "Briefly describe the problem:\n\n  ")
    (save-excursion ; want point left here when finished
      (insert
       "\n\n"
       divider
       "List the steps to reproduce the problem (be as detailed and clear\n"
       "as you can):\n\n"
       "  1. \n\n"
       "  2. \n\n"
       divider
       divider
       (emacs-version))
      ;; append recent messages buffer output
      (let ((messages (get-buffer (if p4-running-xemacs
                                      " *Message-Log*"
                                    "*Messages*"))))
        (when messages
          (let (end
                start)
            (save-excursion
              (set-buffer messages)
              (setq end (point-max))
              (goto-char end)
              (forward-line -10)
              (setq start (point)))
            (insert "\n\n"
                    divider
                    "Recent message buffer output:\n\n")
            (insert-buffer-substring messages start end)
            (insert boundary)))))))

(defun p4-describe-bindings ()
  "List the key bindings for the p4 prefix map."
  (interactive)
  (save-excursion
    (let ((map (make-sparse-keymap))
          (buffer-name (p4-buffer-name "key bindings")))
      (get-buffer-create buffer-name)
      (cond
       (p4-running-xemacs
        (set-buffer buffer-name)
        (delete-region (point-min) (point-max))
        (insert "Key Bindings for Emacs-P4 Mode\n"
                "------------------------------\n")
        (describe-bindings-internal p4-prefix-map))
       (t
        (kill-buffer buffer-name)
        (describe-bindings "\C-xp")
        (set-buffer "*Help*")
        (rename-buffer buffer-name)))
      (define-key map "q" 'p4-quit-current-buffer)
      (use-local-map map)
      (display-buffer buffer-name))))

(defun p4-string-to-list (text)
  "Return TEXT as a list of words."
  (let (result)
    (when text
      (while (or (string-match "^ *\"\\([^\"]*\\)\"" text)
                 (string-match "^ *\'\\([^\']*\\)\'" text)
                 (string-match "^ *\\([^ ]+\\)" text))
        (setq result (append result (list (match-string 1 text))))
        (setq text (substring text (match-end 0)))))
    result))

(defun p4-buffer-file-name ()
  "The filename for the current buffer after following symlinks.
Whether symlinks are followed is controlled by function `p4-follow-link-name'."
  (when buffer-file-name
    (p4-follow-link-name buffer-file-name)))

(defun p4-buffer-file-name-2 ()
  "The filename associated with the current buffer.
If the real buffer filename doesn't exist, try special filename tags set in some
Emacs-P4 buffers and Dired's filename if in a Dired buffer."
  (cond ((p4-buffer-file-name))
        ((get-char-property (point) 'link-client-name))
        ((get-char-property (point) 'link-depot-name))
        ((get-char-property (point) 'block-client-name))
        ((get-char-property (point) 'block-depot-name))
        ((p4-get-dired-filename))))

(defun p4-buffer-directory (&optional buffer)
  "Return the current directory of BUFFER.
BUFFER is the buffer for which the current directory is requested, if non-nil.

Return the current directory of the current buffer if BUFFER is nil."
  (let (result
        (buffer (or buffer
                    (current-buffer))))
    (with-current-buffer buffer
      (let ((filename (p4-buffer-file-name)))
        (setq result (if filename
                         (file-name-directory filename)
                       default-directory))))
    result))

(defun p4-get-file-names ()
  "Return a list of filenames to process for the current buffer.
If the current buffer is a Dired buffer, the file(s) come from Dired.
Otherwise, the filename associated with the current buffer is used."
  (let ((result (p4-get-dired-marked-files)))
    (unless result
      (let ((filename (p4-buffer-file-name-2)))
        (when filename
          (setq result (list filename)))))
    result))

(defun p4-follow-link-name (name)
  "Return NAME or, if following symlinks, NAME's true name.
Whether to follow symlinks is controlled by `p4-follow-symlinks'."
  (let ((pathname (if p4-follow-symlinks
                      (file-truename name)
                    name)))
    (if (and p4-use-cygpath
             (memq system-type '(cygwin32)))
        (p4-replace-in-string
         (shell-command-to-string (format "%s -w %s" p4-cygpath-exec pathname))
         "\n"
         "")
      pathname)))

(defvar p4-depot-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion' for depot names.
An alist of filespecs and anwers.")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion' for branch names.
An alist of ?")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion' for client names.
An alist of ?")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion' for job names.
An alist of ?")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion' for label names.
An alist of ?")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion' for user names.
An alist of ?")

(defvar p4-groups-completion-cache nil
  "Cache for `p4-depot-completion' for group names.
An alist of ?")

(defvar p4-arg-string-history nil
  "History for p4 command arguments.")

(defun p4-depot-completion-search (filespec command)
  "Look for FILESPEC in the completion cache for COMMAND.
FILESPEC is the candidate for completion, so the exact file specification is
\"FILESPEC*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list of
matches.  If not found in cache, return nil.  Thus, the 'no match' answer is
different from 'not in cache'."
  (let ((l (cond
            ((equal command "branches") p4-branches-completion-cache)
            ((equal command "clients") p4-clients-completion-cache)
            ((equal command "dirs") p4-depot-completion-cache)
            ((equal command "jobs") p4-jobs-completion-cache)
            ((equal command "labels") p4-labels-completion-cache)
            ((equal command "users") p4-users-completion-cache)
            ((equal command "groups") p4-groups-completion-cache)))
        dir list)

    (and p4-clean-caches
         (not p4-timer)
         (setq p4-timer
               (p4-create-timer 'p4-cache-cleanup p4-clean-caches-period)))
    (while l
      (when (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
        ;; filespec is included in cache
        (if (string= (car (car l)) filespec)
            (setq list (cdr (car l)))
          (setq dir (cdr (car l)))
          (while dir
            (when (string-match (concat "^" filespec) (car dir))
              (setq list (cons (car dir) list)))
            (setq dir (cdr dir))))
        (setq l nil
              list (cons filespec list)))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional unused)
  "Clean up all the completion caches."
  (message "Cleaning up the p4 caches...")
  (setq p4-branches-completion-cache nil
        p4-clients-completion-cache nil
        p4-depot-completion-cache nil
        p4-jobs-completion-cache nil
        p4-labels-completion-cache nil
        p4-users-completion-cache nil
        p4-groups-completion-cache nil)
  (p4-cancel-timer p4-timer)
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches...done."))

(defun p4-partial-cache-cleanup (type)
  "Clean up the TYPE completion cache."
  (cond ((string= type "branch")
         (setq p4-branches-completion-cache nil))
        ((string= type "client")
         (setq p4-clients-completion-cache nil))
        ((or (string= type "submit")
             (string= type "change"))
         (setq p4-depot-completion-cache nil))
        ((string= type "job")
         (setq p4-jobs-completion-cache nil))
        ((string= type "label")
         (setq p4-labels-completion-cache nil))
        ((string= type "user")
         (setq p4-users-completion-cache nil))
        ((string= type "group")
         (setq p4-groups-completion-cache nil))))

(defun p4-read-depot-output (buffer &optional regexp)
  "Remove and return the first (matching?) line of BUFFER.
Locate the first line in BUFFER or the first line that matches REGEXP, remove
it from the buffer, and return it.

BUFFER is a writeable buffer from which to extract a line.
REGEXP is the regular expression to use to find a matching line, if non-nil."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)
    (let ((line (buffer-substring (point-min) (point))))
      (unless (string= "" line)
        (delete-region (point-min) (point))
        (and regexp
             (string-match regexp line)
             (setq line (substring line (match-beginning 1) (match-end 1))))
        ;; remove trailing newline
        (if (equal (substring line (1- (length line)) (length line)) "\n")
            (substring line 0 (1- (length line)))
          line)))))

(defun p4-completion-helper (filespec command var regexp)
  (message "Making %s completion list..." command)
  (let (line
        result)
    (with-temp-buffer
      (when (p4-depot-output command)
        (while (setq line (p4-read-depot-output (current-buffer) regexp))
          (setq result (cons line result)))
        (set var (cons (cons filespec result) (eval var)))))
    (message "Making %s completion list...done" command)
    result))

(defun p4-depot-completion-build (filespec command)
  "Ask P4 for a list of files and directories beginning with FILESPEC.
COMMAND selects the completion cache to use."
  (let (line
        list)
    (cond
     ((string= "branches" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-branches-completion-cache
                  "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((string= "clients" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-clients-completion-cache
                  "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((string= "dirs" command)
      (message "Making p4 completion list...")
      (let ((filespec* (list (concat filespec "*")))
            (output-buffer (current-buffer)))
        (with-temp-buffer
          (when (p4-depot-output command nil filespec*)
            (while (setq line (p4-read-depot-output output-buffer))
              (unless (string-match "no such file" line)
                (setq list (cons (concat line "/") list)))))
          (erase-buffer)
          (when (p4-depot-output "files" nil filespec*)
            (while (setq line (p4-read-depot-output output-buffer))
              (when (string-match "^\\(.+\\)#[0-9]+ - " line)
                (setq list (cons (match-string 1 line) list)))))
          (setq p4-depot-completion-cache
                (cons (cons filespec list) p4-depot-completion-cache))))
      (message "Making p4 completion list...done"))
     ((string= "jobs" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-jobs-completion-cache
                  "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$")))
     ((string= "labels" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-labels-completion-cache
                  "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((string= "users" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-users-completion-cache
                  "^\\([^ ]+\\).*$")))
     ((string= "groups" command)
      (setq list (p4-completion-helper
                  filespec command 'p4-groups-completion-cache
                  "^\\(.*\\)$"))))
    (message nil)
    (cons filespec list)))

(eval-and-compile
  (defun p4-completion-builder (type)
    `(lambda (string predicate action)
       ,(concat "Completion function for P4 " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")
       (let (list1)
         ,(when (string= type "dirs")
            ;; when testing for an exact match, remove trailing /
            `(when (and (eq action 'lambda)
                        (eq (aref string (1- (length string))) ?/))
               (setq string (substring string 0 (1- (length string))))))
         ;; first, look in cache
         (setq list1 (p4-depot-completion-search string ,type))
         ;; if not found in cache, build list
         (or list1
             (setq list1 (p4-depot-completion-build string ,type)))
         (cond ((null action)
                (try-completion string (mapcar 'list (cdr list1)) predicate))
               ((eq t action)
                (let ((list2
                       (all-completions string
                                        (mapcar 'list (cdr list1))
                                        predicate)))
                  ,(when (string= type "dirs")
                     `(setq list2 (mapcar (function
                                           (lambda (s)
                                             (if (string-match ".*/\\(.+\\)" s)
                                                 (match-string 1 s)
                                               s)))
                                          list2)))
                  list2))
               (t                       ; exact match
                (and (<= 2 (length list1))
                     (member (car list1) (cdr list1))))))))

  (defalias 'p4-branches-completion (p4-completion-builder "branches"))
  (defalias 'p4-clients-completion (p4-completion-builder "clients"))
  (defalias 'p4-depot-completion (p4-completion-builder "dirs"))
  (defalias 'p4-groups-completion (p4-completion-builder "groups"))
  (defalias 'p4-jobs-completion (p4-completion-builder "jobs"))
  (defalias 'p4-labels-completion (p4-completion-builder "labels"))
  (defalias 'p4-users-completion (p4-completion-builder "users"))
  )

(defvar p4-completion-function-alist
  '(("branch" 'p4-branch-string-completion)
    ("client" 'p4-client-string-completion)
    ("group"  'p4-group-string-completion)
    ("job"    'p4-job-string-completion)
    ("label"  'p4-label-string-completion)
    ("user"   'p4-user-string-completion)
    (nil      'p4-arg-string-completion)))

(defun p4-read-arg-string (prompt &optional initial type)
  "Display PROMPT and await user input.
INITIAL is the initial value to supply in the user input area.
TYPE is the completion function to use with `completing-read' as found in the
   `p4-completion-function-alist' alist."
  (let ((minibuffer-local-completion-map
         (copy-keymap minibuffer-local-completion-map))
        (completion-function (cdr (assoc type p4-completion-function-alist))))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
                     completion-function
                     nil
                     nil
                     initial
                     'p4-arg-string-history)))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (when (string-match "^\\(.* +\\)\\(.*\\)" string)
      (setq first-part (match-string 1 string))
      (setq string (match-string 2 string)))
    (cond ((string-match "-b +$" first-part)
           (setq completion (p4-branches-completion string predicate action)))
          ((string-match "-t +$" first-part)
           (setq completion (p4-list-completion
                             string (list "text " "xtext " "binary "
                                          "xbinary " "symlink ")
                             predicate action)))
          ((string-match "-j +$" first-part)
           (setq completion (p4-jobs-completion string predicate action)))
          ((string-match "-l +$" first-part)
           (setq completion (p4-labels-completion string predicate action)))
          ((string-match "\\(.*status=\\)\\(.*\\)" string)
           (setq first-part (concat first-part (match-string 1 string)))
           (setq string (match-string 2 string))
           (setq completion (p4-list-completion
                             string (list "open " "closed " "suspended ")
                             predicate action)))
          ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
               (string-match "\\(.*@\\)\\(.*\\)" string))
           (setq first-part (concat first-part (match-string 1 string)))
           (setq string (match-string 2 string))
           (setq completion (p4-labels-completion string predicate action)))
          ((string-match "\\(.*#\\)\\(.*\\)" string)
           (setq first-part (concat first-part (match-string 1 string)))
           (setq string (match-string 2 string))
           (setq completion (p4-list-completion
                             string (list "none" "head" "have")
                             predicate action)))
          ((string-match "^//" string)
           (setq completion (p4-depot-completion string predicate action)))
          ((string-match "\\(^-\\)\\(.*\\)" string)
           (setq first-part (concat first-part (match-string 1 string)))
           (setq string (match-string 2 string))
           (setq completion (p4-list-completion
                             string (list "a " "af " "am " "as " "at " "ay "
                                          "b " "c " "d " "dc " "dn "
                                          "ds " "du " "e " "f " "i " "j "
                                          "l " "m " "n " "q " "r " "s " "sa "
                                          "sd " "se " "sr " "t " "v ")
                             predicate action)))
          (t
           (setq completion (p4-file-name-completion string
                                                     predicate action))))
    (cond ((null action)                ; try-completion
           (if (stringp completion)
               (concat first-part completion)
             completion))
          ((eq action t)                ; all-completions
           completion)
          (t                            ; exact match
           completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
           (try-completion string collection predicate))
          ((eq action t)
           (all-completions string collection predicate))
          (t
           (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (when (string-match "//\\(.*\\)" string)
    (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (p4-follow-link-name (expand-file-name string)))
  (let ((dir-path "") completion)
    (when (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
      (setq dir-path (match-string 1 string))
      (setq string (match-string 2 string)))
    (cond ((not action)
           (setq completion (file-name-completion string dir-path))
           (if (stringp completion)
               (concat dir-path completion)
             completion))
          ((eq action t)
           (file-name-all-completions string dir-path))
          (t
           (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (when (string-match "^\\(.* +\\)\\(.*\\)" string)
         (setq first-part (match-string 1 string))
         (setq string (match-string 2 string)))
       (cond ((string-match "^-" string)
              (setq completion nil))
             (t
              (setq completion
                    (,completion-function string predicate action))))
       (cond ((null action)             ; try-completion
              (if (stringp completion)
                  (concat first-part completion)
                completion))
             ((eq action t)             ; all-completions
              completion)
             (t                         ; exact match
              completion)))))

(defalias 'p4-branch-string-completion
  (p4-string-completion-builder 'p4-branches-completion))

(defalias 'p4-client-string-completion
  (p4-string-completion-builder 'p4-clients-completion))

(defalias 'p4-job-string-completion
  (p4-string-completion-builder 'p4-jobs-completion))

(defalias 'p4-label-string-completion
  (p4-string-completion-builder 'p4-labels-completion))

(defalias 'p4-user-string-completion
  (p4-string-completion-builder 'p4-users-completion))

(defalias 'p4-group-string-completion
  (p4-string-completion-builder 'p4-groups-completion))

(defun p4-prompt-for-args (command &optional initial type)
  "Prompt user for COMMAND args via minibuffer.
INITIAL is the initial (sequence) value in the minibuffer.
TYPE is the completion type for function `p4-read-arg-string'.

Returns a list of words in the string the user entered."
  (when initial
    (when (listp initial)
      (setq initial (p4-list-to-string initial)))
    ;; ensure it ends with a space
    (if (= 0 (length initial))
        (setq initial " ")
      (unless (string= " " (substring initial -1))
        (setq initial (concat initial " ")))))
  (p4-string-to-list
   (p4-read-arg-string (concat "p4 " command ": ") initial type)))

(defun p4-get-args (command prompt &optional initial default type)
  "Prompt for COMMAND args if PROMPT is non-nil.
INITIAL is the initial value when prompting.
DEFAULT is the result if PROMPT is nil.
TYPE is the completion type used by function `p4-read-arg-string'.

Return a list formed from the words in the user's input if prompting, else
DEFAULT (if DEFAULT is not a list and is non-nil, it will be put into a list)."
  (if prompt
      (p4-prompt-for-args command initial type)
    (when default
      (if (listp default)
          default
        (if (stringp default)
            (p4-string-to-list default)
          (list default))))))

(defun p4-depot-find-file (filespec)
  "Run p4 print FILESPEC to display a file."
  (interactive (list (completing-read "Enter filespec: "
                                      'p4-depot-completion
                                      nil
                                      nil
                                      p4-default-depot-completion-prefix
                                      'p4-depot-history)))
  (let ((files (p4-map-depot-to-client filespec)))
    (if files
        (find-file files)
      (if (get-file-buffer filespec)
          (switch-to-buffer-other-window filespec)
        (p4-call-print filespec t filespec)))))

(defun p4-get-config-info (filename token)
  "Search the config file, FILE-NAME, for the value of TOKEN."
  (with-temp-buffer
    (let ((result (getenv token)))
      (insert-file-contents filename)
      (goto-char (point-min))
      (when (p4-re-search (concat "^" (regexp-quote token) "=\\(.+\\)"))
        (setq result (match-string 1)))
      result)))

(defun p4-current-client ()
  "Get the current local client or, if nil, the global client."
  (let ((result (p4-get-current-variable "P4CLIENT")))
    (or result
        (with-temp-buffer
          (when (zerop (p4-call-p4 "info" t nil))
            (goto-char (point-min))
            (when (p4-re-search "^Client name:[ \t]+\\(.*\\)$")
              (setq result (match-string 1))))))
    result))

(defun p4-current-server-port ()
  "Get the current server:port address."
  (p4-get-current-variable "P4PORT"))

(defun p4-save-opened-files ()
  (let (opened)
    (with-temp-buffer
      (when (p4-depot-output "opened" t)
        (goto-char (point-min))
        (while (p4-re-search "^\\(.*\\)#[0-9]+ - ")
          (setq opened (cons (match-string 1) opened)))))
    (setq opened (mapcar 'cdr (p4-map-depot-files opened)))
    (save-some-buffers nil
                       (function
                        (lambda ()
                          (let ((filename (buffer-file-name (current-buffer))))
                            (and filename
                                 (member filename opened))))))))

(defun p4-empty-diff-p ()
  "Return t if there exists a file opened for edit with an empty diff."
  (when (p4-executable-p)
    (with-temp-buffer
      (let (opened
            empty-diff)
        (p4-exec-p4 "opened" t)
        (goto-char (point-min))
        (while (p4-re-search "^\\(.*\\)#[0-9]* - edit.*")
          (setq opened (cons (match-string 1) opened)))
        (when opened
          (erase-buffer)
          (p4-exec-p4 "diff" t)
          (goto-char (point-max))
          (insert "====\n")
          (goto-char (point-min))
          (while (p4-re-search "^==== \\([^#\n]+\\)#.*\n====")
            (when (member (match-string 1) opened)
              (setq empty-diff t)
              (goto-char (point-max)))))
        empty-diff))))

;; this next chunk is not currently used, but RV's plan was to
;; reintroduce it as configurable bury-or-kill-on-q behaviour:

(defvar p4-blame-2ary-disp-method) ; temporarily eliminate free variable warning
;; (p4-defcustom p4-blame-2ary-disp-method 'default
;;   "Method to use when displaying p4-blame secondary buffers
;;    (currently change and rev buffers)

;;    new-frame   --  pop a new frame for the buffer
;;    new-window  --  create a new window for the buffer
;;    default     --  just do what `display-buffer' would do

;;    Any other value is equivalent to default."
;;   :type '(radio (const default) (const  new-frame) (const new-window))
;;   :group 'p4)

;; RAS: This is a dangerous function.  It purports to kill the blame buffer,
;; but it doesn't verify it is operating on the blame buffer.  Since it is
;; interactive, the user can use it to kill any buffer without question.
(defun p4-blame-kill-blame ()
  "Don\'t ask any questions, just kill the current buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun p4-blame-secondary-buffer-cleanup ()
  "Attempt to clean up a` p4-blame' secondary buffer neatly.
Deletes windows or frames when thought necessary."
  (let* ((buffer (current-buffer))
         (this-window (get-buffer-window buffer t)))
    (cond
     ;; in new-frame mode, delete the frame
     ((eq p4-blame-2ary-disp-method 'new-frame)
      (if (one-window-p 'ignore-minibuffer 'just-this-frame)
          (delete-frame (window-frame this-window))
        (delete-window this-window)) t)
     ;; in new-window mode, just zap the window,
     ;; provided it is not the only one:
     ((eq p4-blame-2ary-disp-method 'new-window)
      (or (one-window-p 'ignore-minibuffer 'just-this-frame)
          (delete-window this-window)) t)
     ;; any other mode, nothing special need be done
     (t
      t))))

;; Menu definition and helper functions
(eval-and-compile
  (defvar p4-menu-def
    '(
      ("Open"
       ["Open for Add" p4-add
        :active (p4-menu-buffer-file-new-p)
        :help "Open the current buffer's file for adding to the depot"]
       ["Open for Edit" p4-edit
        :active (p4-menu-buffer-openable-p)
        :help "Open the current buffer's file for editing"]
       ["Open for Delete" p4-delete
        :active (p4-menu-buffer-openable-p)
        :help "Open the current buffer's file for deletion from the depot"]
       ["Revert" p4-revert
        :active (p4-menu-buffer-opened-p)
        :help "Revert the current buffer's file to the depot's revision"]
       ["Reopen" p4-reopen
        :active (p4-menu-buffer-openable-p)
        :help "Reopen the current buffer's file"]
       )
      ("Synchronize"
       ["Synchronize Entire Workspace" p4-sync-workspace
        :active (p4-available-p)
        :help "Synchronize the client workspace with the depot"]
       ["Synchronize File(s)" p4-sync
        :active (p4-menu-buffer-openable-p)
        :help "Synchronize the current buffer's file with the depot"]
       ["Synchronize Current Directory" p4-sync-directory
        :active (p4-available-p)
        :help "Synchronize all files in the current directory with the depot"]
       ["Synchronize Current Directory and Subdirectories"
        p4-sync-directory-recursively
        :active (p4-available-p)
        :help "Synchronize all files in and under the current directory with the depot"]
       )
      ("Actions"
       ["Edit Changes" p4-change
        :active (p4-available-p)
        :help "Create or edit a changelist"]
       ["Submit Changes" p4-submit
        :active (p4-available-p)
        :help "Submit pending changes to the depot"]
       ["Integrate" p4-integrate
        :active (p4-available-p)
        :help "Schedule integrations from one file to another"]
       ["Resolve Conflicts" p4-resolve
        :active (p4-available-p)
        :help "Resolve conflicts found when trying to submit changes"]
       ["Rename Depot File" p4-rename
        :active (p4-menu-buffer-openable-p)
        :help "Rename the current buffer's file in the depot"]
       )
      "--"
      ("Differences using p4 diff/diff2"
       ["Differences Between Current and Base" p4-diff
        :active (p4-menu-buffer-opened-p)
        :help "Compare the client workspace file against the depot revision opened for edit using p4 diff"]
       ["Differences Between Current and Revision" p4-diff-revision
        :active (p4-menu-buffer-opened-p)
        :help "Compare a specified depot revision and the client workspace file using p4 diff"]
       ["Differences Between 2 Depot Revisions" p4-diff2
        :active (p4-available-p)
        :help "Compare two depot revisions of a file using p4 diff2"]
       ["Differences For All Opened Files" p4-diff-all-opened
        :active (p4-available-p)
        :help "Compare the depot and client workspace files for all open files using p4 diff"]
       )
      ("Differences using Ediff"
       ["Differences Between Current and Base" p4-ediff
        :active (p4-menu-buffer-opened-p)
        :help "Compare the client workspace file against the depot revision opened for edit using Ediff"]
       ["Differences Between Current and Revision" p4-ediff-revision
        :active (p4-menu-buffer-opened-p)
        :help "Compare the depot and client workspace file using Ediff"]
       ["Differences Between 2 Depot Revisions" p4-ediff2
        :active (p4-available-p)
        :help "Compare two depot file revisions using Ediff"]
       )
      ("Status"
       ["Display Specific Revision" p4-print-revision
        :active (and (p4-available-p)
                     p4-mode)
        :help "Displays the specified revision of the current file in another buffer"]
       ["Show Opened Files" p4-opened
        :active (p4-available-p)
        :help "Display a list of files opened for a pending changelist"]
       ["Display Changes" p4-changes
        :active (p4-available-p)
        :help "Displays a list of pending and submitted changelists"]
       ["Describe Change" p4-describe
        :active (p4-available-p)
        :help "Diplay a changelist description"]
       ["Filelog" p4-filelog
        :active (p4-menu-buffer-file-p4-name-p)
        :help "Display the revision history of a file or files"]
       )
      ("Print"
       ["Print Depot Revision" p4-print
        :active (p4-menu-buffer-file-p4-name-p)
        :help "Display a depot file in a buffer"]
       ["Print Depot Revision with History" p4-blame
        :active (p4-menu-buffer-file-p4-name-p)
        :help "Display a depot file with its revision history in a buffer"]
       ["Print Using Depot File Specification" p4-depot-find-file
        :active (p4-available-p)
        :help ""]
       )
      "--"
      ("Specifications"
       ["Branch Specification" p4-branch-edit
        :active (p4-available-p)
        :help "Display a branch specification form"]
       ["Label Specification" p4-label
        :active (p4-available-p)
        :help "Display a label specification form"]
       ["Client Specification" p4-client
        :active (p4-available-p)
        :help "Display the current client workspace specification form"]
       ["User Specification" p4-user
        :active (p4-available-p)
        :help "Display the current user's specification form"]
       )
      ("Notification"
       ["Notify Users" p4-notify
        :active p4-notify
        :help "Prompt for a list of user to notify via e-mail"]
       ["Show the Current Notification List" p4-display-p4notify
        :active p4-notify
        :help "Display the value of P4NOTIFY"]
       ["Set Emacs-P4 Notification List" p4-set-p4notify
        :active p4-mode
        :help "Prompt for a new value for the P4NOTIFY environment variable"]
       )
      ("Login Management"
       ["Log In" p4-login
        :active (p4-available-p)
        :help "Logs the current user into P4"]
       ["Log Out" p4-logout
        :active (p4-available-p)
        :help "Logs the current user out of P4"]
       ["Display Login Tickets" p4-tickets
        :active (p4-available-p)
        :help "Displays any available P4 login tickets"]
       )
      "--"
      ("Settings"
       ["Display Current Settings" p4-set
        :active (p4-available-p)
        :help "Displays the P4 settings currently in effect"]
       ["Set the P4CONFIG environment variable" p4-set-p4config
        :active t
        :help "Prompt for a new value for the P4CONFIG environment variable"]
       ["Report Current Value of P4CONFIG" p4-display-p4config
        :active t
        :help "Display the current value of the P4CONFIG"]
       ["Set the P4CLIENT Environment Variable" p4-set-p4client
        :active t
        :help "Prompt for a new value for P4CLIENT environment variable"]
       ["Report current value of P4CLIENT" p4-display-p4client
        :active t
        :help "Display the current value of P4CLIENT"]
       ["Set the P4PORT Environment Variable" p4-set-p4port
        :active t
        :help "Prompt for a new value for the P4PORT environment variable"]
       ["Report Current Value of P4PORT" p4-display-p4port
        :active t
        :help "Display the current value of P4PORT"]
       ["Set the P4USER Environment Variable" p4-set-p4user
        :active t
        :help "Prompt for a new value for the P4USER environment variable"]
       ["Report Current P4 User" p4-display-p4user
        :active t
        :help "Display the current value of P4USER"]
       "--"
       ["Check Newly Visited Files for P4 Control" p4-toggle-hook-find-file
        :selected p4-hook-find-file
        :style toggle
        :help "Determines whether to check files against the P4 server when visited"]
       ["Allow Emacs-P4 Commands" p4-toggle-enabled
        :selected p4-enabled
        :style toggle
        :help "Disable all Emacs-P4 commands in the current frame"]
       )
      ("About Emacs-P4"
       ["Describe Key Bindings" p4-describe-bindings
        :active t
        :help "Display a list of p4 key bindings"]
       ["Report P4-Emacs Integration Version" p4-emacs-version
        :active t
        :help "Report the version of p4.el"]
       ["Visit Emacs-P4 on the Internet" p4-browse-web-page
        :active t
        :help "Browse the Emacs-P4 home page"]
       ["Report a Bug in Emacs-P4" p4-bug-report
        :active t
        :help "Create a mail message to report a bug"]
       )
      ["Prompt for Arguments" p4-toggle-universal-argument
       :active (p4-available-p)
       :selected prefix-arg
       :style toggle
       :help "Use this to set a numeric argument for the next command"]
      )
    "The Emacs-P4 menu definition")

  (defun p4-menu-buffer-openable-p ()
    "Current buffer's file can be opened by P4 and p4 is available."
    (and (p4-available-p)
         (p4-get-file-names)
         (or (p4-dired-p)
             (not p4-mode)
             buffer-read-only)))

  (defun p4-menu-buffer-opened-p ()
    "Current buffer's file is opened by P4 and p4 is available."
    (and (p4-available-p)
         (p4-get-file-names)
         (or (p4-dired-p)
             (and p4-mode
                  (not buffer-read-only)))))

  (defun p4-menu-buffer-file-new-p ()
    "Current buffer's file is not under P4 control and p4 is available."
    (and (p4-available-p)
         (p4-buffer-file-name)
         (not p4-mode)
         (not buffer-read-only)))

  (defun p4-menu-buffer-file-p4-name-p ()
    "Current buffer has a P4 filename and p4 is available."
    (and (p4-available-p)
         (p4-buffer-file-name-2)))

  (cond (p4-running-xemacs              ; Menu Support for XEmacs
         (defun p4-remove-menu-help-strings (item)
           (cond ((listp item)
                  (mapcar 'p4-remove-menu-help-strings item))
                 ((stringp item)
                  item)
                 ((vectorp item)
                  (let ((length (length item))
                        (i 0)
                        (result item)
                        element)
                    (while (< i length)
                      (setq element (aref item i))
                      (when (eq :help element)
                        (setq result (make-vector (- length 2) nil)
                              i 0) ; visit all elements
                        (while (< i length)
                          (setq element (aref item i))
                          (if (eq :help element)
                              (setq i (+ i 2))
                            (aset result i element)
                            (setq i (1+ i)))))
                      (setq i (1+ i)))
                    result))
                 (t
                  (message "Unrecognized item type: %S" item))))
         (setq p4-menu-def (funcall
                            (symbol-function 'p4-remove-menu-help-strings)
                            p4-menu-def)))
        (t                              ; Menu support for Emacs
         (or (lookup-key global-map [menu-bar])
             (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
         (defvar menu-bar-p4-menu (easy-menu-create-menu "P4" p4-menu-def))
         (set 'menu-bar-final-items
              (cons 'p4-menu (symbol-value 'menu-bar-final-items)))
         (define-key global-map [menu-bar p4-menu]
           (cons "P4" menu-bar-p4-menu))))
  )
;; End menu definition and helper functions

(or (getenv "P4PORT")
    (setenv "P4PORT" p4-default-port))

(provide 'p4)

;;; p4.el ends here
