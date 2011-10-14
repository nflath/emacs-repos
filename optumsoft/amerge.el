;; amerge.el: support for processing 3-way merge files as created by amerge.py.

;; Define some more colored faces.  Xemacs only has blue red green and
;; a couple others built in.
(set-face-foreground (make-face 'amerge-ab-face) "purple")
(set-face-foreground (make-face 'amerge-a-face) "blue")
(set-face-foreground (make-face 'amerge-b-face) "red")

(let ((xemacs (featurep 'xemacs)))
  (defalias 'amerge-make-extent
    (if xemacs 'make-extent 'make-overlay))
  (if xemacs
      (defalias 'amerge-set-extent-face 'set-extent-face)
    (defun amerge-set-extent-face (extent face)
      (overlay-put extent 'face face)))
  (defalias 'amerge-set-extent-property 
    (if xemacs 'set-extent-property 'overlay-put))
  (defalias 'amerge-extent-property 
    (if xemacs 'extent-property 'overlay-get))
  (if xemacs
      (defalias 'amerge-extent-at 'extent-at)
    (defun amerge-extent-at (point)
      (car (overlays-at point)))))


;; server-done-hook

;; Called by code in the appendix; creates extents
(defun amerge-region (point mark type)
  (amerge-set-extent-face
   (amerge-make-extent point mark) 
   (cond ((eq type 'a)
          'amerge-a-face)
         ((eq type 'b)
          'amerge-b-face)
         ((eq type 'ab)
          'amerge-ab-face)
         ((eq type 'marker)
          'highlight)
         )))

;; Called by code in the appendix; creates extents
(defun amerge-diff (from to oldfile at type)
  (let ((e (amerge-make-extent from to)))
    (amerge-set-extent-property e 'oldtext (list oldfile at at))
    (amerge-set-extent-face e (cond ((eq type 'a)
                                     'amerge-a-face)
                                    ((eq type 'b)
                                     'amerge-b-face)
                                    ((eq type 'ab)
                                     'amerge-ab-face)))))


;; Font-locking interferes with amerge coloring pretty badly in most cases
(when (boundp 'font-lock-mode-disable-list)
  (unless (member 'amerge-mode font-lock-mode-disable-list)
    (setq font-lock-mode-disable-list
          (append font-lock-mode-disable-list (list 'amerge-mode)))))

;; gnuserv-done function that kills off dependent .diff2 buffers
(defun amerge-gnuserv-done-function (buffer)
  (mapcar 'kill-buffer (mapcar 'get-file-buffer amerge-diff2-buffers))
  (kill-buffer buffer))

;; Marker that separates the appendix from the actual merge3 conflict region
(defvar amerge-appendix-marker (concat "<<" "<<amerge>>" ">>"))

(define-derived-mode 
  amerge-mode 
  c++-mode "Amerge"
  "Major mode for Merge processing"
  (if (boundp 'font-lock-fontified)
      (font-lock-mode 0))
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward amerge-appendix-marker nil t)
        (progn
          (save-excursion
            (forward-line)
            (eval-region (point) (point-max)))
          (delete-region (point) (point-max))
          (set-buffer-modified-p nil)
          (make-local-variable 'amerge-diff2-buffers)
          (setq amerge-diff2-buffers nil)
          (make-local-variable 'gnuserv-done-function)
          (setq gnuserv-done-function 'amerge-gnuserv-done-function)
          )
      (message "No amerge commands found"))))

(define-key amerge-mode-map [(meta l)] 'amerge-show-old)

(defun amerge-add-regex-to-auto-mode-alist (regex mode)
  "Add regex and mode to auto-mode-alist unless regex is already present"
  (unless (assoc regex auto-mode-alist)
    (setq auto-mode-alist
          (append (list (cons regex mode))
                  auto-mode-alist))))

;; .merge3 and .diff2 files use amerge-mode.
(amerge-add-regex-to-auto-mode-alist "\\.merge3$" 'amerge-mode)
(amerge-add-regex-to-auto-mode-alist "\\.mine.diff2$" 'amerge-mode)
(amerge-add-regex-to-auto-mode-alist "\\.theirs.diff2$" 'amerge-mode)

;; Little wrapper around find-file-noselect 
(defun amerge-find-file-noselect (name)
  (let ((b (find-file-noselect (car old))))
    (setq amerge-diff2-buffers
          (append (list name) amerge-diff2-buffers))
    (save-excursion 
      (set-buffer b)
      (setq buffer-read-only t))
    b))

;; Warp to the corresponding location in the proper diff2 file in a
;; new window.  Try to guess a good way to split the window.
(defun amerge-show-old ()
  (interactive)
  (let* ((e (amerge-extent-at (point)))
         (old (and e (amerge-extent-property e 'oldtext))))
    (when old
      (let ((b (or (get-file-buffer (car old))
                   (amerge-find-file-noselect (car old)))))
        (let ((w (get-buffer-window b)))
          (when (not w)
            (if (= (length (window-list)) 1 )
                (if (> (frame-width) 140)
                    (split-window-horizontally)
                  (split-window-vertically)))
            (setq w (get-lru-window))
            (set-window-buffer w b))
          (select-window w))
        (switch-to-buffer b)
        (goto-char (cadr old))
        ))))
