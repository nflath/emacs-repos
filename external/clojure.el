(defun slime-fuzzy-init ())
(require 'slime)
(require 'slime-fuzzy)
(require 'slime-repl)
(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'durendal)

(add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)

(slime-setup)

