;;; flyguess.el --- Guess language/dictionary for a buffer

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: strings

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use `flyguess-guess' to guess the dictionary to use to spell check a
;; buffer. When there will be enough data to guess, according to
;; `flyguess-mininum-buffer-size', the hook `flyguess-guessed-hook' will be
;; called with the dictionary guessed at first argument.
;;
;; For example, to use the guessed dictionary while writing a mail in message-mode:
;;
;; ;; Activate Flyguess when composing a mail
;; (add-hook 'message-mode-hook 'flyguess-guess)
;; ;; When the language is guessed, change the dictionary, activate flyspell
;; ;; and recheck the buffer
;; (add-hook 'flyguess-guessed-hook (lambda (dictionary)
;;                                     (ispell-change-dictionary dictionary)
;;                                     (flyspell-mode 1)
;;                                     (flyspell-buffer)))

;;; Code:

(require 'ispell)
(require 'flyspell)

(defcustom flyguess-dictionary-list
  '("francais" "english")
  "List of dictionary to test.")

(defcustom flyguess-mininum-buffer-size 300
  "Minimum buffer size needed to guess.")

(defcustom flyguess-try-every 3
  "Seconds to wait for Emacs to be idle between before trying to
guess.")

(defvar flyguess-guessed-hook nil
  "Hook run when the dictionary has been guessed for a buffer.")

(defvar flyguess-timer nil
  "Timer used to detect the dictionary language.")
(make-variable-buffer-local 'flyguess-timer)

(defvar flyguess-incorrect-words-count nil
  "Hash mapping dictionary name to number of incorrect words.")
(make-variable-buffer-local 'flyguess-incorrect-words-count)

(defun flyguess-on-incorrect (start end doublon-or-correction)
  "Function called via `flyspell-incorrect-hook' to count incorrect words.
It stores number of errors for current dictionary in
`flyguess-incorrect-words-count'."
  (puthash ispell-local-dictionary
           (+ 1 (gethash ispell-local-dictionary flyguess-incorrect-words-count 0))
           flyguess-incorrect-words-count)
  ;; Return nil, we do not want to make flyspell believe a word is correct
  nil)

(defun flyguess-count-incorrect-words (buffer)
  "Count incorrect words for dictionary in `flyguess-dictionary-list' for BUFFER.
Return `flyguess-incorrect-words-count', a hash table with dictionary as key
and number of errors as value."
  (with-temp-buffer
    (insert-buffer buffer)
    (setq flyguess-incorrect-words-count (makehash))
    (add-hook 'flyspell-incorrect-hook 'flyguess-on-incorrect)
    ;; Use Flyspell mode predicate
    (setq flyspell-generic-check-word-predicate
          (get major-mode 'flyspell-mode-predicate))
    (dolist (dict flyguess-dictionary-list)
      (ispell-change-dictionary dict)
      (flyspell-buffer))
    (remove-hook 'flyspell-incorrect-hook 'flyguess-on-incorrect)
    flyguess-incorrect-words-count))

(defun flyguess-guess-dictionary (buffer)
  "Count incorrect words for BUFFER and returns which dictionary
fits better."
  (let (dict incorrect)
    (maphash
     (lambda (key value)
       (when (or (not dict)
                 (< value incorrect))
         (setq dict key)
         (setq incorrect value)))
     (flyguess-count-incorrect-words buffer))
    dict))

(defun flyguess-cancel-timer ()
  (when (timerp flyguess-timer)
    (cancel-timer flyguess-timer)
    (setq flyguess-timer nil)))

(defun flyguess-polling-guess (buffer)
  "Try to guess if the buffer size is enough."
  (with-current-buffer buffer
    ;; Enough data?
    (let ((buffer-size (- (point-max) (point-min))))
      (when (>= buffer-size flyguess-mininum-buffer-size)
        (flyguess-cancel-timer)
        (run-hook-with-args 'flyguess-guessed-hook
                            (flyguess-guess-dictionary buffer))))))

(defun flyguess-guess ()
  "Guess and the dictionary for the current buffer."
  (interactive)
  (unless flyguess-timer
    (add-hook 'kill-buffer-hook 'flyguess-cancel-timer)
    (setq flyguess-timer
          (run-with-idle-timer
           flyguess-try-every t
           'flyguess-polling-guess
           (current-buffer)))))

(provide 'flyguess)
