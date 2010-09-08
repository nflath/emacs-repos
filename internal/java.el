(defun insert-java-class-template ()
  "Insert a template for a java class."
  (interactive)
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

(add-hook 'java-mode-hook (lambda ()  (if (string-equal "" (buffer-string)) (insert-java-class-template))))