(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources (append ac-sources '(ac-source-etags)))
(add-hook 'prog-mode-hook 'auto-complete-mode)
(ac-flyspell-workaround)

