
;; Makes TAB do the right thing for various languages.
(load-library "arastra-indent")

;; Integration with a4, for checking files out and for
;; compiling/running/debugging within emacs.
(load-library "a4")

;; Makes C-v and M-v do what they should, rather than what Stallman wanted;
;; specifically, makes C-v and M-v inverses of each other so pressing one
;; and then the other is always a no-op.
(if (featurep 'xemacs)
    (load-library "scroll-in-place"))

;; Makes M-k switch to a recently used buffer, with lightning
;; completion.  Wrapper for iswitchb
(load-library "nifty-buffer")

;; Gives you "C-x f" to find file at point.  Useful for jumping to
;; errors, etc.
(load-library "nifty-file")

;; Gives you C-z and M-z for running shells inside emacs.  Emacs is good
;; for you.
(load-library "ashell")

;; Gives you M-e and M-r for sane defining and running of keyboard
;; macros.
(load-library "keyboard-macros")

;; Some little useful functions like C-ct for a timestamp
(load-library "arastra-utils")

;; Dynamic completion of words with Meta-return
(autoload 'dabbrev-expand "dabbrev" "Expand previous word \"dynamically\".")

;; Support for gnu id-utils
(autoload 'gid "id-utils" "Lookup a string in a mkid database" t)

;; Support for coloring buffer regions
(autoload 'colorize "colorize" "Supporting for coloring regions" t)

;; Other improvements (IMO) over default emacs keybindings.
(load-library "misc-bindings")

;; Have gnuclient always use the same frame if not running in X
;; emacsclient does this by default.
(if (featurep 'xemacs)
    (if (eq (frame-type) 'tty)
        (setq gnuserv-frame t)))
