;; The following gives a visual indication of a disconnected channel by making
;; the header line red.
(add-to-list 'erc-modules 'log)
(erc-update-modules)

(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
   (cond ((erc-server-process-alive) 'erc-header-line)
         (t 'erc-header-line-disconnected))))
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; I sometimes want to look through the logs of my conversations.  The
;; following makes sure to always keep logs up-to-date and saved.
(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part t)

(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

;; Hide unimportant messages from being displayed.
(setq erc-hide-list '("MODE"))

;; When possible, notifies me by use of notify-send(and thus outside emacs)
;; when I am mentioned in a message.
(when (shell-command "notify-send --version")
  (defun erc-notify-on-msg (msg)
    (if (string-match "nflath:" msg)
        (shell-command (concat "notify-send \"" msg "\""))))
  (add-hook 'erc-insert-pre-hook 'erc-notify-on-msg))
