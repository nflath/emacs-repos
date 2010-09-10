(setq c-basic-offset 3)
(setq-default c-offsets-alist '((innamespace . 0)))
(setq auto-mode-alist
      (append '(
                ("\\.c$"                      . c-mode)
                ("\\.cc$"                     . c++-mode)
                ("\\.C$"                      . c++-mode)
                ("\\.CC$"                     . c++-mode)
                ("\\.h$"                      . c-mode)
                ("\\.hh$"                     . c++-mode)
                ("\\.H$"                      . c-mode)
                ("\\.HH$"                     . c++-mode)
                ("\\.cpp$"                    . c++-mode)
                ("\\.CPP$"                    . c++-mode)
                ("\\.tat$"                    . python-mode)
                ("\\.hpp$"                    . c++-mode)
                ("\\.HPP$"                    . c++-mode)
                ("\\.java$"                   . c++-mode)
                ("\\.\\([pP][Llm]\\|al\\)$"   . perl-mode)
                ("\\`/var/tmp/"               . text-mode)
                ("\\.tac$"                    . c++-mode)
                ("\\.tin$"                    . c++-mode)
                ("\\.itin$"                    . c++-mode)
                )
              auto-mode-alist))

;;(require 'xcscope)
;;(require 'doxymacs)
;;(add-hook 'c-mode-common-hook 'doxymacs-mode)
;;(semantic-load-enable-minimum-features)
;(semantic-load-enable-gaudy-code-helpers)
;;(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-code-helpers)
(require 'semantic/ia)
(semantic-add-system-include "/obs/nflath/local/nflath/opt/usr/include/" 'c++-mode)
(global-set-key [f12] 'semantic-ia-fast-jump)


;; ASCII-TABLE
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d 0x%02x %c\n" i i i))))
  (beginning-of-buffer)
  )

(setq completion-ignored-extensions
      (cons ".class" completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".exe"   completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".o"     completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".dvi"   completion-ignored-extensions)
      completion-ignored-extensions
      (cons ".ps"    completion-ignored-extensions))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(c-basic-offset 3)
;;  '(doxymacs-doxygen-dirs "/obs/baiyu/local/baiyu/jason/src/tacc/tacc/")
;;  '(doxymacs-doxygen-style "C++")
;;  '(ecb-options-version "2.32")
;;  '(global-semantic-decoration-mode nil nil (semantic-decorate-mode))
;;  '(global-semantic-highlight-edits-mode nil nil (semantic-util-modes))
;;  '(global-semantic-highlight-func-mode t nil (semantic-util-modes))
;;  '(global-semantic-idle-completions-mode t nil (semantic-idle))
;;  '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
;;  '(global-semantic-idle-summary-mode t nil (semantic-idle))
;;  '(global-semantic-idle-tag-highlight-mode t nil (semantic-idle))
;;  '(global-semantic-mru-bookmark-mode t nil (semantic-util-modes))
;;  '(global-semantic-show-parser-state-mode nil nil (semantic-util-modes))
;;  '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
;;  '(global-semantic-stickyfunc-mode nil nil (semantic-util-modes))
;;  '(global-senator-minor-mode t nil (senator))
;;  '(org-src-lang-modes (quote (("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist) ("asymptote" . asy) ("dot" . fundamental) ("C++" . c++-mode) ("C" . c-mode))))
;;  '(semanticdb-global-mode t nil (semanticdb))
;;  '(which-function-mode t))