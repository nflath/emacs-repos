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

(define-key c-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)
(define-key c++-mode-map (kbd "C-M-a") 'ido-beginning-of-defun)

(c-add-style "Optumsoft" '("user" 
                           (c-basic-offset . 3)
                           (c-offsets-alist
                            (innamespace . 0)
                            (access-label . -2)
                            (arglist-intro . 6)
                            (case-label . 1)
                            (statement-case-intro . 2)
                            (statement-case-open . 2)
                            (member-init-intro . 6)
                            )) nil)

(defun optumsoft-c-mode-common-hook ()
  (c-set-style "Optumsoft")
  ;; Automatically insert copyright notice.
  (if (< (buffer-size) 10) 
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert (format "// Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "// OptumSoft, Inc. Confidential and Proprietary.\n\n")
          (if (string-match "\\([a-zA-Z0-9_]+\\)[/\\\\]\\([a-zA-Z0-9_]+\\)\\.h$" (buffer-file-name))
              (let* ((dir-name (substring (buffer-file-name) (match-beginning 1) (match-end 1)))
                     (file-name (substring (buffer-file-name) (match-beginning 2) (match-end 2)))
                     (symbol (upcase (format "%s_%s_H" dir-name file-name))))
                (insert (format "#ifndef %s\n#define %s\n\n\n#endif // %s\n" symbol symbol symbol)))))
        (set-buffer-modified-p mod))))