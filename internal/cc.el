(require 'cc-mode)

;;When pressing ENTER inside a string, continue it.
(define-key c-mode-base-map [remap newline-and-indent]
  (lambda ()
    (interactive)
    (if (or
         (eq (get-text-property (point) 'face) font-lock-string-face)
         (eq (c-in-literal) 'string))
        (progn
          (insert "\"")
          (newline-and-indent)
          (insert "+ \""))
      (newline-and-indent))))

(defun c-least-enclosing-brace (paren-state)
  ;; Return the bufpos of the outermost enclosing open paren, or nil
  ;; if none was found.
  (save-excursion
    (let (pos elem)
      (while paren-state
        (setq elem (car paren-state)
              paren-state (cdr paren-state))
        (when (integerp elem)
          (setq pos elem)
          (goto-char pos)
          (c-skip-ws-backward)
          (let ((syntax (c-guess-basic-syntax)))
            (while syntax
              (when (eq (caar syntax) 'topmost-intro)
                (setq syntax nil)
                (setq paren-state nil))
              (if syntax (setq syntax (cdr syntax)))))))
      pos)))

(define-key c-mode-map (kbd "C-M-a") 'c-beginning-of-defun)
(define-key c++-mode-map (kbd "C-M-a") 'c-beginning-of-defun)