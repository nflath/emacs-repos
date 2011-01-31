;;Lookup javadoc

(require 'javadoc-help)
(javadoc-set-predefined-urls '("http://download.oracle.com/javase/1.5.0/docs/api/"))
(jdh-process-predefined-urls *jdh-predefined-urls*)
(jdh-refresh-url "http://download.oracle.com/javase/1.5.0/docs/api/")
(define-key java-mode-map (kbd "C-x j") 'javadoc-lookup)