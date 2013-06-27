;; FixMe: Use hook-utils
;;Use the common-lisp library.
(require 'cl)

(defun add-hook-to-all (hooks fn)
  "Add a function to a list of hooks."
  (mapcar (lambda (hook) (add-hook hook fn)) hooks))

(defun remove-hook-from-all (hooks fn)
  "Removes a function from a list of hooks."
  (mapcar (lambda (hook) (remove-hook hook fn)) hooks))
