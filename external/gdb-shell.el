;;gdb-shell will invoke the emacs 'gdb' command when you run gdb in a shell buffer, and turn on
;;compilation-shell-minor-mode when you run 'make', 'valgring', or 'ant'.
(require 'gdb-shell)
(add-hook 'shell-mode-hook 'gdb-shell-minor-mode)

