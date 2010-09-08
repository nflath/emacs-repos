;;A package that automatically saves the list of opened files and opens every previously-opened file on startup.

(eval-after-load 'init-finished
  '(progn
     (require 'save-visited-files)
     (setq save-visited-files-auto-restore t)
     (save-visited-files-mode t)))