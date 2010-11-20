;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.
(require 'assoc)
(setq command-list
      '(( ?d . kill-region)
        ( ?c . copy-region-as-kill)))

(setq movement-list
      '(( ?w . forward-word)
        ( ?d . end-of-line)
        ( ?s . forward-sexp)))

(defun perform-command-movement (command movement)
  (interactive "c\nc")
  (save-excursion
    (let ((start (point)))
      (call-interactively (aget movement-list movement))
      (funcall (aget command-list command) start (point)))))


(defmacro foobar (str)
  (if (not (stringp str))
      (error "bad")))

(foobar 'foo)

(setq foo "five")
(foobar foo)


665 newall