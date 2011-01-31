;;Lookup javadoc

(require 'javadoc-help)
(javadoc-set-predefined-urls '("http://download.oracle.com/javase/1.5.0/docs/api/"))

(define-key java-mode-map (kbd "C-x j") 'javadoc-lookup)