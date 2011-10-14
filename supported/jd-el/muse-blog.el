;;; muse-blog.el --- keep and publish a blog

;; Copyright (C) 2010 Julien Danjou <julien@danjou.info>

;; This file is not part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse Blog is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.

;; Emacs Muse Blog is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The module facilitates the keeping and publication of a blog.
;;
;; The input format for each entry is as follows:
;;
;;   * 20040317-12:34:23: Title of entry
;;
;;   Text for the entry.
;;
;; The plurality of "div" tags makes it possible to display the
;; entries in any form you wish, using a CSS style.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Blog Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-html)
(require 'muse-journal)

(defgroup muse-blog nil
  "Rules for transforming a blog into its final form."
  :group 'muse-publish)

(defcustom muse-blog-heading-regexp
  "\\([0-9:-]+[0-9]\\)\\(?:: \\)?\\(.+\\)?"
  "A regexp that matches a blog heading.
Paren groups 1 is the mandatory ISO date (that will be matched
against `muse-blog-date-regexp') and group 2 is the optional
heading for the entry."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-date-regexp
  "\\`\\([1-9][0-9][0-9][0-9]\\)[./]?\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)\\(?:[-:]?\\([0-2][0-9]\\)[:]?\\([0-5][0-9]\\)[:]?\\([0-5][0-9]\\)?\\)?"
  "A regexp that matches the date in the blog heading.
Paren group 1 is the year, group 2 is the month, group 3 is the
date. Optional group 4 is hour, group 5 is minutes and group 6 is
seconds."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-make-index-files-regexp
  "^[0-9]+"
  "A regexp that matches the files that we should include in the
index of the blog. Note that this regexp should probably not
include the index page, or it may loop forever."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-html-date-format "%a, %e %b %Y %k:%M:%S"
  "Date format to use for blog entries."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-html-date-format-notime "%a, %e %b %Y"
  "Date format to use for blog entries when there's no time
specified."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-muse-heading-regexp
  (concat "^\\* " muse-blog-heading-regexp "$")
  "A regexp that matches a blog heading from a Muse document.
Paren groups 1 is the ISO date and group 2 is the optional heading
for the entry."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-html-heading-regexp
  (concat "^<h2[^>\n]*>" muse-blog-heading-regexp "</h2>$")
  "A regexp that matches a blog heading from an HTML document.
Paren groups 1 is the ISO date and group 2 is the optional heading
for the entry."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-muse-footer-regexp
  "^----+"
  "A regexp that matches the start of a footer from a Muse
document."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-html-footer-regexp
  "^<hr\\( /\\)?>$"
  "A regexp that matches the start of a footer from an HTML
document."
  :type 'regexp
  :group 'muse-blog)

(defcustom muse-blog-html-entry-template
  "<div class=\"entry\">
  <a name=\"%anchor%\" style=\"text-decoration: none\">&nbsp;</a>
  <div class=\"entry-body\">
    <div class=\"entry-head\">
      <div class=\"entry-date\">
        <span class=\"date\">%date%</span>
      </div>
      <div class=\"entry-title\">
        <h2>%title%</h2>
      </div>
    </div>
    <div class=\"entry-text\">
%text%
    </div>
  </div>
</div>\n\n"
  "Template used to publish individual blog entries as HTML.
This may be text or a filename."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-extension ".xml"
  "Default file extension for publishing RSS 2.0 files."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-base-url ""
  "The base URL of the website referenced by the RSS file."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-header
  "<\?xml version=\"1.0\" encoding=\"<lisp>
  (muse-html-encoding)</lisp>\"?>
<rss version=\"2.0\">
  <channel>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <link><lisp>(concat (muse-style-element :base-url)
                        (concat (muse-page-name)
                                muse-html-extension))</lisp></link>
    <description><lisp>(muse-publishing-directive \"desc\")</lisp></description>
    <language><lisp>(or (muse-publishing-directive \"lang\") \"en-us\")</lisp></language>
    <generator>Emacs Muse</generator>\n\n"
  "Header used for publishing RSS 2.0 files. This may be text or
a filename."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-footer
  "\n\n  </channel>
</rss>\n"
  "Footer used for publishing RSS 2.0 files. This may be text or
a filename."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-date-format
  "%a, %d %b %Y %H:%M:%S %Z"
  "Date format to use for RSS 2.0 entries."
  :type 'string
  :group 'muse-blog)

(defcustom muse-blog-rss-entry-template
  "\n    <item>
      <title>%title%</title>
      <link>%link%#%anchor%</link>
      <description><![CDATA[%text%]]></description>
      <author><lisp>(muse-publishing-directive \"author\")</lisp></author>
      <pubDate>%date%</pubDate>
      <guid isPermaLink=\"true\">%link%#%anchor%</guid>
    </item>\n"
  "Template used to publish individual blog entries as RSS 2.0.
This may be text or a filename."
  :type 'string
  :group 'muse-blog)

(defun muse-blog-encode-date (date)
  "Transform DATE string into Unix timestamp. Return nil if DATE
does not match `muse-blog-date-regexp'."
  (save-match-data
    (when (and date
               (string-match muse-blog-date-regexp date))
      (let ((year (match-string 1 date))
            (month (match-string 2 date))
            (day (match-string 3 date))
            ;; Time is optional
            (hour (match-string 4 date))
            (minute (match-string 5 date))
            (second (match-string 6 date)))
        (format-time-string
         (if hour
             (or
              (muse-publishing-directive "date-format")
              (muse-style-element :date-format)
              muse-blog-html-date-format)
           (or
            (muse-publishing-directive "date-format-notime")
            (muse-style-element :date-format-notime)
            muse-blog-html-date-format-notime))
         (encode-time
          (string-to-number (or second "0"))
          (string-to-number (or minute "0"))
          (string-to-number (or hour "0"))
          (string-to-number day)
          (string-to-number month)
          (string-to-number year)
          nil))))))

(defun muse-blog-replace-keyword (keyword value)
  "Replace a %KEYWORD% with its VALUE."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (concat "%" keyword "%") nil t)
      (replace-match value nil t))))

(defun muse-blog-clean-markups (string)
  "Remove HTML markup from a STRING."
  (when string
    (save-match-data
      (while (string-match "\\(^<[^>]+>\\|<[^>]+>$\\)" string)
        (setq string (replace-match "" nil nil string)))))
  string)

(defun muse-blog-anchor (title)
  (muse-journal-anchorize-title (muse-blog-clean-markups title)))

(defun muse-blog-make-title (date title)
  "Build the title string based on DATE and TITLE."
  (or title (muse-blog-encode-date date)))

(defun muse-blog-do-entry (heading-regexp
                           on-entry
                           &optional
                           on-non-entry
                           footer-regexp)
  "Call function ON-ENTRY for each blog entry. The buffer is
narrowed to this entry. Each blog entry is separated by
HEADING-REGEXP. Also, call optional ON-NON-ENTRY function on each
part of the buffer which is not a blog entry (eventual header and
footer). Footer should be separated by FOOTER-REGEXP."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward heading-regexp nil t)
        (let* ((date (or (match-string 1) "19700101")) ; Seriously, no date? WTF!
               (title (muse-blog-make-title date (match-string 2)))
               (anchor (muse-blog-anchor title)))
          (save-restriction
            ;; Narrow buffer…
            (narrow-to-region
             ;; …from entry start…
             (match-beginning 0)
             ;; …to next heading start…
             (if (re-search-forward heading-regexp nil t)
                 (match-beginning 0)
               ;; …or to the end of the buffer.
               (point-max)))
            ;; If we handle footer
            (when footer-regexp
              ;; Go back at the beginning!
              (goto-char (point-min))
              ;; Now, narrow…
              (narrow-to-region
               ;; …from buffer (entry) start…
               (point-min)
               ;; …to footer…
               (if (and footer-regexp (re-search-forward footer-regexp nil t))
                   (match-beginning 0)
                 ;; …or to the end of the buffer
                 (point-max))))
            (goto-char (point-min))
            (funcall on-entry date title anchor))))
      ;; Go back to the beginning
      (goto-char (point-min))
      ;; Look for first entry
      (when on-non-entry
        (when (re-search-forward heading-regexp nil t)
          ;; Call `on-non-entry' on header part
          (save-restriction
            ;; Narrow to (point-min) to the start of the next entry
            (narrow-to-region (point-min) (match-beginning 0))
            (goto-char (point-min))
            (funcall on-non-entry))))
      ;; Look for all non-entry things, i.e. things between
      ;; `footer-regexp' and `heading-regexp'.
      (when (and on-non-entry footer-regexp)
        (while (re-search-forward footer-regexp nil t)
          (narrow-to-region
           (match-beginning 0)
           (if (re-search-forward heading-regexp nil t)
               (match-beginning 0)
             (point-max)))
          (goto-char (point-min))
          (funcall on-non-entry))))))

(defun muse-blog-munge-buffer (&optional fixup
                                         on-non-entry)
  (muse-blog-do-entry
   muse-blog-html-heading-regexp
   (lambda (date title anchor)
     (let (text)
       ;; Remove entry heading
       (when (re-search-forward muse-blog-html-heading-regexp nil t)
         (delete-region (match-beginning 0) (match-end 0)))
       (setq text (buffer-string))
       (delete-region (point-min) (point-max)) 
       (muse-insert-file-or-string (muse-style-element :entry-template))
       (when fixup
         (funcall fixup))
       (muse-blog-replace-keyword "date" date)
       (muse-blog-replace-keyword "title" title)
       (muse-blog-replace-keyword "text" text)
       (muse-blog-replace-keyword "anchor" anchor)
       (muse-blog-replace-keyword "link"
                                  (concat (muse-style-element :base-url)
                                          (concat (muse-page-name)
                                                  muse-html-extension)))
       (muse-publish-mark-read-only (point-min) (point-max))))
   on-non-entry
   muse-blog-html-footer-regexp)
  ;; indicate that we are to continue the :before-end processing
  nil)

(defun muse-blog-delete-buffer-content ()
  "Delete the content of a buffer."
  (delete-region (point-min) (point-max)))

(defun muse-blog-rss-fixup ()
  ;; Reencode date in a format that RSS readers can handle
  (setq title (muse-blog-clean-markups title))
  (let ((system-time-locale "C"))
    (setq date (muse-blog-encode-date date))))

(defun muse-blog-html-fixup ()
  (setq date (muse-blog-encode-date date)))

(defun muse-blog-html-munge-buffer ()
  (muse-blog-munge-buffer
   'muse-blog-html-fixup))

(defun muse-blog-rss-munge-buffer ()
  (muse-blog-munge-buffer
   'muse-blog-rss-fixup
   'muse-blog-delete-buffer-content))

(defun muse-blog-make-index (&optional start end regexp)
  "Build index file, including number from START to END. Articles
number start at 0. Only files matching REGEXP will be included. If
REGEXP is not specified, it defaults to
`muse-blog-make-index-files-regexp'."
  ;; Get file list that match `regexp'.
  (let* ((regexp (or regexp muse-blog-make-index-files-regexp))
         (files (mapcar 'cdr
                        (remove-if-not (lambda (entry)
                                         (string-match regexp (car entry)))
                                       (reverse (muse-project-file-alist)))))
         (result "")
         (article-number 0))
    ;; Insert buffer contents
    (dolist (file files result)
      ;; Only treat file if we did not reach the maximum number of articles
      ;; already.
      (unless (and end (>= article-number end))
        ;; Update result
        (setq
         result
         (concat
          result
          (with-temp-buffer
            ;; Insert file content
            (insert-file-contents file)
            ;; For each entry, build link like [[anchor][title]], or remove
            ;; it if we reach the maximum number of article.
            (muse-blog-do-entry
             muse-blog-muse-heading-regexp
             (lambda (date title anchor)
               (if (or
                    (and end (>= article-number end))
                    (and start (< article-number start)))
                   ;; Outside range, delete!
                   (muse-blog-delete-buffer-content)
                 (when (re-search-forward muse-blog-muse-heading-regexp nil t)
                   (let ((new-title (concat "[["
                                            (file-name-nondirectory
                                             (file-name-sans-extension file))
                                            muse-html-extension
                                            "#" anchor "][" title "]]")))
                     ;; Replace only the title part if there was one
                     (if (match-string 2)
                         (replace-match new-title nil t nil 2)
                       (insert new-title)))))
               (incf article-number))
             'muse-blog-delete-buffer-content
             muse-blog-muse-footer-regexp)
            ;; Return the buffer content
            (buffer-string))))))))

;;; Register the Muse Blog Publishers

(muse-derive-style "blog-html" "html"
                   :date-format 'muse-blog-html-date-format
                   :entry-template 'muse-blog-html-entry-template
                   :before-end 'muse-blog-html-munge-buffer)

(muse-derive-style "blog-xhtml" "xhtml"
                   :date-format 'muse-blog-html-date-format
                   :entry-template 'muse-blog-html-entry-template
                   :before-end 'muse-blog-html-munge-buffer)

(muse-derive-style "blog-rss" "html"
                   :suffix         'muse-blog-rss-extension
                   :before-end     'muse-blog-rss-munge-buffer
                   :header         'muse-blog-rss-header
                   :footer         'muse-blog-rss-footer
                   :date-format    'muse-blog-rss-date-format
                   :entry-template 'muse-blog-rss-entry-template
                   :base-url       'muse-blog-rss-base-url)

(provide 'muse-blog)

;;; muse-blog.el ends here
