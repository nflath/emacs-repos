;;Markdown-mode is a major mode for editing Markdown-formatted text.  It provides syntax highlighting, indentation, and
;;org-mode-like visibility cycling between atx and hash-tyle headers.  Additionally, it defines commands for compiling
;;your markdown file, as well as a few editing commands.

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq markdown-enable-math t)
