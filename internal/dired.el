;; Dired is a mode for navigating and editing directories.  It shows a display
;; like ls -l, and you can mark files and delete them or perform other
;; operations en masse.  Wdired allows editing the listing as text; the changes
;; you make will be saved when you execute save-buffer.  dired-x gives some
;; extra commands that you can use.

(setq wdired-allow-to-change-permissions 'advanced)
(setq dired-auto-revert-buffer t)

(setq dired-omit-files
      (concat
       dired-omit-files
       "\\\\|"
      (rx (or (seq bol (? ".") "#")          ;; emacs autosave files
              (seq bol "." (not (any ".")))   ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              (seq ".class" eol)            ;; Compiled java files
              (seq ".pyc" eol)              ;; Compiled python files
              (regexp "TAGS")
              ))))

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; FixMe: Export to package

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; FixMe: End package
