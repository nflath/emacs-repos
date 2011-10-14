(setq-default c-offsets-alist '((innamespace . 0)))
(setq auto-mode-alist
      (append '(
                ("\\.tat$"                    . python-mode)
                ("\\.tac$"                    . tacc-mode)
                ("\\.tin$"                    . c++-mode)
                ("\\.itin$"                   . c++-mode)
                )
              auto-mode-alist))

