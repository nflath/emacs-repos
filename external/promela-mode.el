(setq auto-mode-alist
      (append
       (list (cons "\\.promela$"  'promela-mode)
		     (cons "\\.spin$"     'promela-mode)
		     (cons "\\.pml$"      'promela-mode)
             )
       auto-mode-alist))