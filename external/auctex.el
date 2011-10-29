(require 'tex)
(require 'tex-fold)
(require 'preview)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
(setq-default TeX-electric-sub-and-superscript t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(defadvice LaTeX-insert-item (before latex-item-add-newline activate)
  (newline-and-indent))

(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))
