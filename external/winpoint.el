;; When two windows view the same buffer at the same time, and one window is
;; switched to another buffer and back, point is now the same as in the other
;; window, not as it was before we switched away.  This mode tries to work
;; around this problem by storing and restoring per-window positions for each
;; buffer.  It has some issues with dired; specifically, if calling dired on a
;; buffer already opened, it sets point to 0.

;; FixMe: submit to winpoint.el
(require 'winpoint)
(defun winpoint-restore (win)
  "Restore point in the window WIN."
  (with-selected-window win
    (let ((point (winpoint-get win (current-buffer))))
      (when (and point
                 (not (eq major-mode 'dired-mode)))
        (goto-char point)))))

(winpoint-mode t)
