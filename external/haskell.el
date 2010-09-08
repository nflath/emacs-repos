;;haskell-mode is a mode providing syntax highlighting, indentation, help, and a REPL for editing Haskell code.

(when (= 0 (shell-command "ghci"))
  (require 'haskell-doc)
  (require 'haskell-mode)
  (require 'haskell-ghci)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
  (add-hook 'haskell-mode-hook 'haskell-doc-show-global-types)
  (setq haskell-doc-search-distance 4000)
  (setq haskell-doc-idle-delay 0)
  (haskell-ghci-start-process nil))