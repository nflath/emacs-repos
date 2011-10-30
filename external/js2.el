(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-allow-keywords-as-property-names nil)
(setq js2-auto-indent-p t)
(setq js2-basic-offset 4)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-global-externs (quote ("algjs" "$" "goog" "YAHOO" "jQuery")))
(setq js2-highlight-level 3)
(setq js2-idle-timer-delay 0.2)
(setq js2-indent-on-enter-key nil)
(setq js2-mirror-mode t)



