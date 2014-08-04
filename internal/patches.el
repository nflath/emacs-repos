;;;;; Things I really wish were patched in Emacs.

;; FixMe: Why isn't this

(defun ff-find-the-other-file (&optional in-other-window)
  "Find the header or source file corresponding to the current file.
Being on a `#include' line pulls in that file, but see the help on
the `ff-ignore-include' variable.

If optional IN-OTHER-WINDOW is non-nil, find the file in another window."

  (let (match           ;; matching regexp for this file
        suffixes        ;; set of replacing regexps for the matching regexp
        action          ;; function to generate the names of the other files
        fname           ;; basename of this file
        pos             ;; where we start matching filenames
        stub            ;; name of the file without extension
        alist           ;; working copy of the list of file extensions
        pathname        ;; the pathname of the file or the #include line
        default-name    ;; file we should create if none found
        format          ;; what we have to match
        found           ;; name of the file or buffer found - nil if none
        dirs            ;; local value of ff-search-directories
        no-match)       ;; whether we know about this kind of file

    (run-hooks 'ff-pre-find-hook 'ff-pre-find-hooks)

    (message "Working...")

    (setq dirs
          (if (symbolp ff-search-directories)
              (ff-list-replace-env-vars (symbol-value ff-search-directories))
            (ff-list-replace-env-vars ff-search-directories)))

    (setq fname (ff-treat-as-special))

    (cond
     ((and (not ff-ignore-include) fname)
      (setq default-name fname)
      (setq found (ff-get-file dirs fname nil in-other-window)))

     ;; let's just get the corresponding file
     (t
      (setq alist (if (symbolp ff-other-file-alist)
                      (symbol-value ff-other-file-alist)
                    ff-other-file-alist)
            pathname (if (buffer-file-name)
                         (buffer-file-name)
                       "/none.none"))

      (setq fname (file-name-nondirectory pathname)
            no-match nil
            match (car alist))

      ;; find the table entry corresponding to this file
      (setq pos (ff-string-match (car match) fname))
      (while (and match (if (and pos (>= pos 0)) nil (not pos)))
        (setq alist (cdr alist))
        (setq match (car alist))
        (setq pos (ff-string-match (car match) fname)))

      ;; no point going on if we haven't found anything
      (if (not match)
          (setq no-match t)

        ;; otherwise, suffixes contains what we need
        (setq suffixes (car (cdr match))
              action (car (cdr match))
              found nil)

        ;; if we have a function to generate new names,
        ;; invoke it with the name of the current file
        (if (and (atom action) (fboundp action))
            (progn
              (setq suffixes (funcall action (buffer-file-name))
                    match (cons (car match) (list suffixes))
                    stub nil
                    default-name (car suffixes)))

          ;; otherwise build our filename stub
          (cond

           ;; get around the problem that 0 and nil both mean false!
           ((= pos 0)
            (setq format "")
            (setq stub "")
            )

           (t
            (setq format (concat "\\(.+\\)" (car match)))
            (string-match format fname)
            (setq stub (substring fname (match-beginning 1) (match-end 1)))
            ))

          ;; if we find nothing, we should try to get a file like this one
          (setq default-name
                (concat stub (car (car (cdr match))))))

        ;; do the real work - find the file
        (setq found
              (ff-get-file dirs
                           stub
                           suffixes
                           in-other-window)))))

    (cond
     (no-match                     ;; could not even determine the other file
      (message ""))

     (t
      (cond

       ((not found)                ;; could not find the other file

        (run-hooks 'ff-not-found-hook 'ff-not-found-hooks)

        (cond
         (ff-always-try-to-create  ;; try to create the file
          (let (name pathname)

            (setq name (expand-file-name default-directory))

            (setq pathname
                  (if (file-directory-p name)
                      (concat (file-name-as-directory name) default-name)
                    (setq found name)))

            (ff-find-file pathname in-other-window t)))

         (t                        ;; don't create the file, just whinge
          (message "No file found for %s" fname))))

       (t                          ;; matching file found
        nil))))

    found))                        ;; return buffer-name or filename
