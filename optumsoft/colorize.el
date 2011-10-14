(defun uncolorize () (mapcar (lambda (e) (when (extent-property e 'colorize)
                                           (delete-extent e)))
                             (extent-list)))

(defun colorize (point mark color)
    (interactive 
     "r
SColor:")
    (let ((e (make-extent point mark)))
      (set-extent-face e color)
      (set-extent-property e 'colorize t)))

(set-face-foreground (make-face 'magenta) "magenta")
(set-face-foreground (make-face 'purple) "purple")
(set-face-foreground (make-face 'darkgreen) "darkgreen")
(set-face-foreground (make-face 'darkblue) "darkblue")
(set-face-foreground (make-face 'darkred) "darkred")
