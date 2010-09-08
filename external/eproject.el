;;Eproject gives you the ability to define projects based on somei criteria, the most common one being all files of
;;certain types in all directories under a build file.  It adds a hook that runs whenever you visit a project of a
;;certain type, allowing you to do things such as set *compile-command* on entering a file belonging to a project.
;;eproject-extras defines some additional commands, most notably *eproject-find-file* and *eproject-ibuffer*.

(require 'eproject)
(require 'eproject-extras)

(define-project-type Java (generic)
  (look-for "build.xml")
  :relevant-files ("\\.xml$" "\\.java$"))

(define-project-type c (generic)
  (look-for "Makefile")
  :relevant-files ("\\.cpp" "\\.c" "\\.hpp" "\\.h"))