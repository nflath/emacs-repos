;;hideshow-org enhances the hideshow package that provides code folding to make it behave like org-mode's
;;folding. It provides a much better interface for folding code than the default hideshow functions and keybindings.
;;TAB will cycle folding the foldable region point is in.

(setq hs-org/trigger-keys-block (list [C-tab]))

(require 'hideshow-org)

(add-hook-to-all programming-major-mode-hooks (lambda () (hs-org/minor-mode 1)))