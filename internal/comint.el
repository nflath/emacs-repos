;; Comint is a mode used for mcommunicating with external processes such as
;; shells or REPLs.  You normally use a derived mode of comint instead of just
;; it.  The following makes it easier to use by not displaying garbage, making
;; the prompt read-only, and showing the maximum amount of output and scrolling
;; properly.

(setq-default comint-process-echoes t)
(setq comint-prompt-read-only t)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq comint-scroll-show-maxiumum-output t)
(setq comint-scroll-to-bottom-on-input t)

;; Many times in a shell you perform the same action over and over again.  The
;; following will make repeated commands only add one item to the history of the
;; process, making it easier to access previous commands.
(setq-default comint-input-ignoredups t)
