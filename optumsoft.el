(setq c-basic-offset 3)

(setq auto-mode-alist
      (append '(
                ("\\.c$"                      . c-mode)
                ("\\.cc$"                     . c++-mode)
                ("\\.C$"                      . c++-mode)
                ("\\.CC$"                     . c++-mode)
                ("\\.h$"                      . c-mode)
                ("\\.hh$"                     . c++-mode)
                ("\\.H$"                      . c-mode)
                ("\\.HH$"                     . c++-mode)
                ("\\.cpp$"                    . c++-mode)
                ("\\.CPP$"                    . c++-mode)
                ("\\.hpp$"                    . c++-mode)
                ("\\.HPP$"                    . c++-mode)
                ("\\.\\([pP][Llm]\\|al\\)$"   . perl-mode)
                ("\\`/var/tmp/"               . text-mode)
                ("\\.tac$"                    . c++-mode)
                ("\\.tin$"                    . c++-mode)
                ("\\.itin$"                    . c++-mode)
                )
              auto-mode-alist))

(setq completion-ignored-extensions
      (cons ".class" completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".exe"   completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".o"     completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".dvi"   completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".ps"    completion-ignored-extensions))
