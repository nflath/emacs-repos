(defun after-last (regexp string)
  "Returns the part of the string after the last occurrence of regexp."
  (let ((index (string-match regexp string)))
    (if index
        (after-last regexp (substring string (match-end 0) (length string)))
      string)))

(defun insert-java-class-template ()
  "Insert a template for a java class."
  (interactive)
  (end-of-buffer)
  (let ((start (string-match "src/\\(.*\\)" default-directory)))
    (when start
      (insert (concat "package "
                      (replace-regexp-in-string "/" "." (substring default-directory
                                                                   (+ 4 start)
                                                                   (1- (length default-directory))))
                      ";\n\n")))
    (skeleton-insert '(nil "public class " str " { \n\n}")
                     nil
                     (replace-regexp-in-string "\\.java" "" (after-last "/" (buffer-file-name))))))

(add-hook 'java-mode-hook (lambda ()  (if (not (string-match "class" (buffer-string)))
                                     (insert-java-class-template))))
