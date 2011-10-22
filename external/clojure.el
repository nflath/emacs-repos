(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'slime)
(require 'slime-fuzzy)
(require 'slime-repl)
(require 'durendal)

(add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)

;;(require 'swank-cdt)
(slime-setup)

