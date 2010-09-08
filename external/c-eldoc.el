;;Adds support for the C programming language to [[eldoc]].  Eldoc displays the function signature of the call point is
;;in to the message area.

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)