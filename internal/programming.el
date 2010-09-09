(require 'flymake)
(defun flymake-post-syntax-check (exit-status command)
  (setq flymake-err-info flymake-new-err-info)
  (setq flymake-new-err-info nil)
  (setq flymake-err-info
        (flymake-fix-line-numbers
         flymake-err-info 1 (flymake-count-lines)))
  (flymake-delete-own-overlays)
  (flymake-highlight-err-lines flymake-err-info)
  (let (err-count warn-count)
    (setq err-count (flymake-get-err-count flymake-err-info "e"))
    (setq warn-count  (flymake-get-err-count flymake-err-info "w"))
    (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
                 (buffer-name) err-count warn-count
                 (- (flymake-float-time) flymake-check-start-time))
    (setq flymake-check-start-time nil)

    (if (and (equal 0 err-count) (equal 0 warn-count))
        (if (or (equal 0 exit-status)
                (equal 2 exit-status))
            (flymake-report-status "" "")	; PASSED
          (if (not flymake-check-was-interrupted)
              (flymake-report-fatal-status "CFGERR"
                                           (format "Configuration error has occured while running %s" command))
            (flymake-report-status nil ""))) ; "STOPPED"
      (flymake-report-status (format "%d/%d" err-count warn-count) ""))))

;;If there is a tags file in a directory above this one, use it; otherwise create one in this directory.  I don't want
;;to be prompted for a TAGS file.
(defadvice find-tag (before create-tags activate)
  (make-variable-buffer-local 'tags-file-name)
  (let ((tf (locate-dominating-file default-directory "TAGS")))
    (unless tf
      (shell-command "etags *")
      (setq tf "TAGS"))
    (setq tags-file-name tf)))

(require 'imenu)
(require 'cl)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist)
      (if (not symbol-names)
          (ido-goto-symbol)
        (let* ((symbol-at-point (symbol-name (symbol-at-point)))
               (selected-symbol (ido-completing-read
                                 "Symbol? "
                                 (if (member symbol-at-point symbol-names)
                                     (cons symbol-at-point (remove-if
                                                            (lambda (x) (string-equal x symbol-at-point))
                                                            symbol-names))
                                   symbol-names)))
               (position (cdr (assoc selected-symbol name-and-pos))))
          (if (markerp position)
              (goto-char position) (goto-char (overlay-start position))))))))
(global-set-key (kbd "M-s") 'ido-goto-symbol)

(defun ido-beginning-of-defun ()
  "Will update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist)
      (let ((pt (point))
            (closest 0))
        (dolist (symbol symbol-names)
          (let* ((pos (cdr (assoc symbol name-and-pos)))
                 (position (if (markerp pos) pos (overlay-start pos))))
            (if (and (> pos closest) (< pos pt))
                (setq closest pos))))
        (goto-char closest)))))

;;Compiling always asks to save my buffers; I want it to instead do nothing, so I have save-some-buffers only do stuff if passed a filter.
(defadvice save-some-buffers (around save-buffers-ignore activate)
  (if (ad-get-arg 1)
      ad-do-it))

;;I want compilation buffers to scroll to the bottom, since this is in general where errors are.
(setq compilation-scroll-output t)

;;These advices cause copy-pasted code to be properly indented.
(defadvice yank (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice kill-line (after fixup-whitespace activate)
  "Call fixup whitespace after killing line."
  (when (not (eq major-mode 'python-mode))
    (if (not (eq major-mode 'shell-mode))
        (funcall indent-line-function))
    (if (not (looking-at "$"))
        (fixup-whitespace))
    (indent-according-to-mode)
    (if (looking-at "^")
        (back-to-indentation))))

(defadvice kill-line (after fixup-comments activate)
  "Don't leave comment characters after killing a line."
  (let* ((pt (point))
         (only (progn (back-to-indentation) (eq (point) pt))))
    (goto-char pt)
    (let ((start (point)))
      (when (and comment-start
                 only
                 (looking-at (concat "[ \t]*" (regexp-opt (list (string-trim comment-start))))))
        (let ((len (- (match-end 0) (match-beginning 0))))
          (back-to-indentation)
          (when (and (looking-at (regexp-opt (list (string-trim comment-start))))
                     (not (= (point) start)))
            (goto-char start)
            (delete-char len)))))))

(setq comment-auto-fill-only-comments t)

;(add-hook-to-all programming-major-mode-hooks 'flyspell-prog-mode)

(add-hook-to-all programming-major-mode-hooks
                 (lambda ()
                   (font-lock-add-keywords
                    nil
                    '(("\\(FIXME\\|TODO\\)" 1 font-lock-warning-face)))))