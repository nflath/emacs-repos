;;HTMLize.el will convert a buffer or file into HTML, preserving the appearance of the buffer in Emacs.  This means that
;;the outputted web page will look exactly like the buffer does in your Emacs window at the time you call htmlize.
;;HTMLize works by defining several commands which you can use to convert buffers, files, or regions to HTML.

(require 'htmlize)
(setq htmlize-html-major-mode 'html-mode)