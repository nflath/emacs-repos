
(defun start-or-end-kbd-macro ()
  (interactive)
  (if defining-kbd-macro 
      (end-kbd-macro nil) (start-kbd-macro nil)))

(defun end-if-needed-and-run-kbd-macro (arg)
  (interactive "p")
  (if defining-kbd-macro (end-kbd-macro nil))
  (call-last-kbd-macro arg))


(defun ++ ()
  "Increment the number (in base 10 representation) at point."
  (interactive)
  (add 1))

(defun add (amt) 
  "Increment the number (in base 10 representation) at point."
  (interactive "nAmount to add: ")
  (if (looking-at "[-0-9.]+")
      (progn
        (let ((num (car (read-from-string (buffer-substring (match-beginning 0) (match-end 0))))))
          (delete-region (match-beginning 0) (match-end 0))
          (setq num (+ num amt))
          (insert (format (if (< (abs (- num (round num))) 0.001) "%.0f" "%.2f") num))))))

(defun mul (amt) 
  "Multiply the number (in base 10 representation) at point."
  (interactive "nAmount to multiply by: ")
  (if (looking-at "[-0-9.]+")
      (progn
        (let ((num (car (read-from-string (buffer-substring (match-beginning 0) (match-end 0))))))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (format "%f" (* num amt)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Default bindings

(global-set-key "\ee" 'start-or-end-kbd-macro)
(global-set-key "\er" 'end-if-needed-and-run-kbd-macro)

