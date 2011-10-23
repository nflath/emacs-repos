;;; Various utility packages

(require 'htmlize)
(setq htmlize-html-major-mode 'html-mode)

(require 'pastebin)

(require 'p4)
(setq p4-verbose nil)

(require 'magit)
(require 'magithub)


(require 'member-function)

(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)

(require 'fuzzy-match)

(require 'macro-math)

(require 'loccur)

(require 'w32-browser)
