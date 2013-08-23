;;Lookup javadoc
;; FixMe: Cache this locally (so can be used without a network connection)
 (require 'javadoc-help)
 (javadoc-set-predefined-urls '("http://download.oracle.com/javase/1.5.0/docs/api/"))
 (jdh-process-predefined-urls *jdh-predefined-urls*)
 (condition-case nil
     (jdh-refresh-url "http://download.oracle.com/javase/1.5.0/docs/api/")
   (error nil))
