(require 'semantic/ia)
(require 'cedet)

(semantic-add-system-include "/obs/nflath/local/nflath/opt/usr/include/" 'c++-mode)
(global-set-key [f12] 'semantic-ia-fast-jump)
(add-hook 'c-mode-common-hook (lambda () (semantic-mode 1)))
(semantic-load-enable-code-helpers)
