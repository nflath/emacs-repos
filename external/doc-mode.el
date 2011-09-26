(when (featurep 'semantic)
  (require 'doc-mode)
  (add-hook 'c-mode-common-hook 'doc-mode))








