;;member-function is a package used for automatically inserting stubs for functions defined in a header file but not
;;implemented.  The following snippet will hevaluate it whenever a C++ file is entered.

(require 'member-function)

;;expand member functions automatically when entering a cpp file
(defun c-file-enter ()
  "Expands all member functions in the corresponding .h file"
  (let* ((c-file (buffer-file-name (current-buffer)))
         (h-file-list (list (concat (substring c-file 0 -3 ) "h")
                            (concat (substring c-file 0 -3 ) "hpp")
                            (concat (substring c-file 0 -1 ) "h")
                            (concat (substring c-file 0 -1 ) "hpp"))))
    (if (or (equal (substring c-file -2 ) ".c")
            (equal (substring c-file -4 ) ".cpp"))
        (mapcar (lambda (h-file)
                  (if (file-exists-p h-file)
                      (expand-member-functions h-file c-file)))
                h-file-list))))
(add-hook 'c++-mode-hook 'c-file-enter)