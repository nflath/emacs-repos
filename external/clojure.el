(setq clojure-src-root "~/.emacs.d/")
(setq package-activated-list nil)
(require 'clojure-mode)

;;Start a REPL
(when (file-exists-p "/home/nflath/.emacs.d/clojure.jar")
    (setq inferior-lisp-program "java -cp /home/nflath/.emacs.d/clojure.jar clojure.main")
    (inferior-lisp inferior-lisp-program))
