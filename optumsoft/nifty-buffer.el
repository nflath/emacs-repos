(require 'iswitchb)

(defun nifty-iswitchb-hook ()
  "Bind c-n and c-p to find next and prev match"
  (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
  (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match))

(add-hook 'iswitchb-define-mode-map-hook 'nifty-iswitchb-hook)
(if (fboundp 'iswitchb-mode)
    (iswitchb-mode t))


(defun last-buffer-matching (string)
  (let* ((bl (buffer-list)))
    (block x
      (while bl
        (if (string-match string (buffer-name (first bl)))
            (progn (switch-to-buffer (first bl)) (return-from x))
          (setq bl (cdr bl)))))
    (unless bl
      (message "no matching buffer found"))))

(defun last-gdb-buffer () (interactive) 
  (last-buffer-matching "^\*gdb"))

(defun last-compilation-buffer () (interactive) 
  (last-buffer-matching "^\*compilation"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Default bindings

(global-set-key "\ek" 'iswitchb-buffer)
(global-set-key "\C-xb" 'iswitchb-buffer)
(global-set-key "\C-cg" 'last-gdb-buffer)
(global-set-key "\C-ck" 'last-compilation-buffer)
