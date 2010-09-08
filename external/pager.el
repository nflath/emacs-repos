;;pager.el defines alternative commands for scrolling the screen.  These commands, unlike the emacs builtins, will
;;restore point if done symetrically(for example, page down followed by page up).

(require                  'pager)
(global-set-key "\C-v"	'pager-page-down)
(global-set-key [next]	'pager-page-down)
(global-set-key "\ev"	    'pager-page-up)
(global-set-key [prior]	'pager-page-up)
(global-set-key '[M-up]	'pager-row-up)
(global-set-key '[M-kp-8] 'pager-row-up)
(global-set-key '[M-down] 'pager-row-down)
(global-set-key '[M-kp-2] 'pager-row-down)