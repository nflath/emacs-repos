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

(define-key c-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)
(define-key c++-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)