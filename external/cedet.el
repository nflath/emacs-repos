(require 'semantic/ia)
(semantic-add-system-include "/obs/nflath/local/nflath/opt/usr/include/" 'c++-mode)
(global-set-key [f12] 'semantic-ia-fast-jump)

(semantic-mode 1)