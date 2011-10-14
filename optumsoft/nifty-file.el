
(defvar nifty-file-name-chars "-=+a-zA-Z0-9_./~\\\\")

(defvar nifty-shell-working-directory-regexp
  "^\\([~/][^~ \n]*\\) +@[a-zA-Z0-9]+[#$%>] *")

(defvar nifty-ffnp-path-alist 
  (list (cons 'compilation-mode "[Ee]ntering directory .\\(.+\\).")
        (cons 'shell-mode nifty-shell-working-directory-regexp))
  "An alist whose car is a mode symbol and whose cdr is 
a regexp to match to find a path in a buffer of that mode")

(defun nifty-ffnp-directory ()
  "Look in the current buffer to see if there is some text before the
current point that would tell you what the cwd was at that point in the file
(e.g., a prompt in a shell window or an 'Entering Directory' 
string in a compilation window)."
  (let ((regexp (cdr (assoc major-mode nifty-ffnp-path-alist))))
    (if regexp
        (save-excursion
          (and (re-search-backward regexp nil t)
               (buffer-substring (match-beginning 1) (match-end 1)))))))

(defun nifty-find-file-name ()
  (save-excursion
    (skip-chars-backward nifty-file-name-chars)
    (let ((b (+ (point) (if (and (> (point) (+ 2 (point-min)))
                                 (save-excursion
                                   (backward-char 2)
                                   (looking-at "[a-zA-Z]:"))) -2 0))))
      (skip-chars-forward nifty-file-name-chars)
      (cons b (point)))))

(defun nifty-find-file-near-point ()
  (interactive)
  (let ((f (nifty-find-file-name)))
    (goto-char (cdr f))
    (let ((linenum 
           (cond
            ((or (looking-at ":\\([0-9]+\\)")
                 (looking-at "[,;:\"]* line \\([0-9]+\\)")
                 (looking-at "[,;:\"]* at line \\([0-9]+\\)") ;; Expecthook.py format
                 (looking-at " [-a-zA-Z0-9_<>]* \\([0-9]+\\)")
                 (looking-at "(\\([0-9]+\\))")) ; pdb format
             (string-to-int 
              (buffer-substring (match-beginning 1) (match-end 1))))
            (t nil))))

      (let ((filename (buffer-substring (car f) (cdr f))))
        (let ((dir (nifty-ffnp-directory)))
          ;; Don't slap dir on the front if we have a fully-specified path
          (find-file (if (and (not (string-match "^/" filename))
                              dir)
                         (concat dir "/" filename)
                       filename))
          (if linenum (goto-line linenum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Default bindings

(global-set-key "\C-xf" 'nifty-find-file-near-point)
