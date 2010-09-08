(require 'hungry-delete)
(add-hook-to-all programming-major-mode-hooks 'turn-on-hungry-delete-mode)
(add-hook 'org-mode-hook 'turn-on-hungry-delete-mode)