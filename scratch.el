;; This buffer is for notes you don't want to save, and for Lisp evaluation.  If you want to create a file, visit that
;; file with C-x C-f, then enter the text in that file's own buffer.

(setq command-list
      '( "d" . 'kill-region))

(setq movement-list
      '( "f" . 'forward-word))

(defun perform-command-movement (command movement)
  (interactive "c\nc")
  
  )
