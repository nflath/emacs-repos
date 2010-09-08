;;context-sensitive completion for C and C++

(require 'gccsense)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-gccsense)))