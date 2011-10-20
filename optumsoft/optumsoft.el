;; General configurations for OptumSoft

(setq auto-mode-alist
      (append '(
                ("\\.tat$"                    . python-mode)
                ("\\.tac$"                    . tac-mode)
                ("\\.tin$"                    . c++-mode)
                ("\\.itin$"                   . c++-mode)
                )
              auto-mode-alist))

