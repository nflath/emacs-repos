
(global-set-key "\eo" 'other-window)
(global-set-key "\e0" 'delete-window)
(global-set-key "\e1" 'delete-other-windows)
(global-set-key "\e2" 'split-window)
(global-set-key "\e3" 'split-window-horizontally)
(global-set-key "\e5" 'query-replace-regexp)
(global-set-key "\C-cb" 'bury-buffer)
(global-set-key "\C-c\C-f" 'font-lock-mode)
(global-set-key "\C-xp" 'repeat-complex-command)
(global-set-key "\ep" 'backward-paragraph)
(global-set-key "\en" 'forward-paragraph)
(global-set-key "\ei" 'gid)

;; meta-Enter on the IBM thinkpad in Arora 6 generates meta-return
;; when in an x window, and meta-linefeed when a terminal
(global-set-key '[(meta return)]  'dabbrev-expand)
(global-set-key '[(meta linefeed)]  'dabbrev-expand)

;; Make it so that "C-x h" in text mode views as HTML 
;; (by opening a web browser)
(add-hook 'text-mode-hook 
          (lambda nil 
            (local-set-key "\C-xh" 'browse-url-of-buffer)))

(when (featurep 'xemacs)
  (autoload 'filladapt-mode "filladapt" "A better way of indenting text")
  (add-hook 'text-mode-hook 'filladapt-mode))

