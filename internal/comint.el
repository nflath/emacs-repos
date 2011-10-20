;; Comint is a mode used for mcommunicating with external processes such as
;; shells or REPLs.  You normally use a derived mode of comint instead of just
;; it.  The following makes it easier to use by not displaying garbage, making
;; the prompt read-only, and showing the maximum amount of output and scrolling
;; properly.

(setq-default comint-process-echoes t)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(setq comint-prompt-read-only t)

(setq comint-scroll-show-maxiumum-output t)
(setq comint-scroll-to-bottom-on-input t)

(setq comint-password-prompt-regexp
      "\\(\\(?: SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: same\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|login\\|new\\|old\\)\\)? ?\\(?:[Pp]ass\\(?: phrase\\|phrase\\|word\\)\\|pass\\(?: phrase\\|phrase\\|word\\)\\) ?\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for [^:]+\\)?:\\s *\\'")
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Many times in a shell you perform the same action over and over again.  The
;; following will make repeated commands only add one item to the history of the
;; process, making it easier to access previous commands.
(setq-default comint-input-ignoredups t)
