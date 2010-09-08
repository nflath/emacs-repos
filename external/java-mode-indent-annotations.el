;;In the current version of [[cc-mode]], lines with annotations are indented poorly - if part of a statement includes an
;;annotation, any parts of the statement on a seperate line are indented again.  Java-mode-indent-annotations will
;;prevent this from occurring.

(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)