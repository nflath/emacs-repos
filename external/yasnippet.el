;;Yasnippet is a snippet expansion package based on TextMate's snippet features.  You type the trigger text, call
;;yas/expand, and the larger snippet of text is inserted into the buffer.  Yasnippet also allows you to define locations
;;whether point will be located once expansion occurs, and what locations point will cycle among once you are filing out
;;the snippet.  Yasnippet comes with a large number of predefined snippets.
;;
;;yas/minor-mode is the minor-mode for yasnippet.  It does not have to be enabled to call yas/expand, but it sets
;;yas/trigger-key, which will call yas/expand when pressed.  If there is no snippet to expand, it falls back to what
;;would be called if yasnippet were not enabled.
;;
;;Yasnippet and all of the snippets it comes with can be downloaded from http://code.google.com/p/yasnippet/.

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory (concat emacs-repos-dir "snippets"))

(yas/load-directory yas/root-directory)
(yas/global-mode t)
(add-hook 'prog-mode-hook (lambda () (yas/minor-mode 1)))

;;Killing lines doesn't work while in snippets - unless you do this.
(defadvice kill-line (before yas-protect activate)
  (if (yas/snippets-at-point)
      (yas/commit-snippet (car (yas/snippets-at-point)))))
