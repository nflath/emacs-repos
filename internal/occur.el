;;Occur is a function that essentially greps within a file.  You input a regular expression, and a window with all lines
;;matching that expression will be displayed.  Normally, the last regex used for occur is the default; this modification
;;makes the default be the current word.
(defun occur-read-primary-args ()
  (list (read-regexp "List lines matching regexp"
                     (current-word))
        (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))
