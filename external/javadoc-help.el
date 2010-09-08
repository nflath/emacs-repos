;;Lookup javadoc

(require 'javadoc-help)
(jdh-javadocs-add (jdh-javadoc-new "http://download-llnw.oracle.com/docs/cd/E17476_01/javase/1.5.0/docs/api/" t t nil))
(define-key java-mode-map (kbd "C-x j") 'javadoc-lookup)