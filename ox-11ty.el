;;; ox-11ty.el --- Eleventy export for Emacs Org Mode  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Version: 2.17.0
;; Package-Requires: ((emacs "27"))
;; Keywords: org, eleventy, 11ty
;; Homepage: https://github.com/sachac/ox-11ty

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A very rough starting point for exporting to 11ty from Org Mode.
;;

;;; Code:

(require 'ox-html)

(defun org-11ty--front-matter (info)
  "Return front matter for INFO."
  (let* ((date (plist-get info :date))
         (title (plist-get info :title))
         (modified (plist-get info :modified))
         (permalink (plist-get info :permalink))
         (categories (plist-get info :categories))
         (collections (plist-get info :collections)))
    (list :permalink permalink
          :date (if (listp date) (car date) date)
          :modified (if (listp modified) (car modified) modified)
          :title (if (listp title) (car title) title)
          :categories (if (stringp categories) (split-string categories) categories)
          :tags (if (stringp collections) (split-string collections) collections))))

(defun org-11ty-template (contents info)
  (format
   "module.exports = class {
  data() {
    return %s;
  }
  render() {
    return %s;
  }
}"
   (json-encode (org-11ty--front-matter info))
   (json-encode-string contents)))

(defun org-11ty-export-as-11ty (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as 11ty file."
  (interactive)
  (org-export-to-buffer '11ty "*org 11ty export*" async subtreep visible-only body-only ext-plist))

(defun org-11ty--get-info (subtreep visible-only)
  (let ((info (org-combine-plists
               (org-export--get-export-attributes '11ty subtreep visible-only)
               (org-export--get-buffer-attributes)
               (org-export-get-environment '11ty subtreep))))
    (unless (plist-get info :categories)
      (plist-put info :categories (let ((org-use-tag-inheritance t)) (org-get-tags))))
    (unless (plist-get info :permalink)
      (plist-put info :permalink (concat "/" (plist-get info :file-name))))
    info))

(defun org-11ty-export-to-11ty (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (base-file-name (concat (or
                                  (and (plist-get info :file-name)
                                       (if (string= (file-name-base (plist-get info :file-name)) "")
                                           (concat (plist-get info :file-name) "index")
                                         (plist-get info :file-name)))
                                  (org-export-output-file-name "" subtreep))
                                 ".11ty.js"))
         (file
          (if (plist-get info :base-dir)
              (expand-file-name base-file-name (plist-get info :base-dir))
            base-file-name)))
    (plist-put info :file file)
    (when (file-name-directory file)
      (make-directory (file-name-directory file) :parents))
    (org-export-to-file '11ty file
      async subtreep visible-only body-only ext-plist)))

(defun org-11ty--copy-files-and-replace-links (info text)
  (let ((file-regexp "<img src=\"\\(.*?\\)\"")
        (destination-dir (file-name-directory (plist-get info :file-path)))
        file-all-urls file-name beg)
    (save-excursion
      (while (string-match file-regexp text beg)
        (setq file-name
              (if (match-beginning 1)
                  (substring text (match-beginning 1) (match-end 1))
                (substring text (match-beginning 2) (match-end 2)))
              beg (match-end 1))
        (setq file-name (save-match-data (if (string-match "^file:" file-name)
                                             (substring file-name 7)
                                           file-name)))
        (unless (file-exists-p (expand-file-name (file-name-nondirectory file-name) destination-dir))
          (copy-file file-name destination-dir))
        (when (file-exists-p (expand-file-name (file-name-nondirectory file-name) destination-dir))
          (setq file-all-urls
                (cons (cons file-name (concat (plist-get info :permalink)
                                              (file-name-nondirectory file-name)))
                      file-all-urls)))
        (mapc (lambda (file)
                  (setq text (replace-regexp-in-string
                              (concat "\\(<a href=\"\\|<img src=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                              (concat "\\1" (cdr file)) text)))
              file-all-urls)))
    text))

(defun org-11ty--base-file-name (subtreep visible-only)
  "Return the path to the output file, sans extension."
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (base-file-name
          (or
           (and (plist-get info :file-name)
                (if (string= (file-name-base (plist-get info :file-name)) "")
                    (concat (plist-get info :file-name) "index")
                  (plist-get info :file-name)))
           (org-export-output-file-name "" subtreep))))
    (if (plist-get info :base-dir)
        (expand-file-name base-file-name (plist-get info :base-dir))
      base-file-name)))

(defun org-11ty-export-to-11tydata (&optional async subtreep visible-only body-only ext-plist)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only)))
    (plist-put info :file-path file)
    (when (file-name-directory file)
      (make-directory (file-name-directory file) :parents))
    (with-temp-file (concat file ".11tydata.json")
      (insert (json-encode (org-11ty--front-matter info))))))

(defun org-11ty-export-to-11tydata-and-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only)))
    (plist-put info :file-path file)
    (when (file-name-directory file) (make-directory (file-name-directory file) :parents))
    (with-temp-file (concat file ".11tydata.json") (insert (json-encode (org-11ty--front-matter info))))
    (let ((body (org-11ty--copy-files-and-replace-links info (org-export-as '11ty subtreep visible-only t ext-plist))))
      (with-temp-file (concat file ".html")
        (insert body)))))

(defun org-11ty-export-to-org (&optional async subtreep visible-only body-only ext-plist)
  "Export this post as an Org file in the output directory."
  (interactive)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only)))
    (plist-put info :file-path file)
    (when (file-name-directory file) (make-directory (file-name-directory file) :parents))
    (with-temp-file (concat file ".11tydata.json") (insert (json-encode (org-11ty--front-matter info))))
    (let ((body (org-export-as 'org subtreep visible-only t ext-plist)))
      (with-temp-file (concat file ".org")
        (save-excursion
          (insert body))))))

(defun org-11ty-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to 11ty.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (or (string= (org-element-property :type export-block) "11TY")
            (string= (org-element-property :type export-block) "HTML"))
    (org-remove-indentation (org-element-property :value export-block))))

(defun org-11ty-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to 11ty.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (member (org-export-snippet-backend export-snippet) '(11ty html))
    (org-element-property :value export-snippet)))

(defun org-11ty-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (or (org-export-custom-protocol-maybe link desc '11ty info)
      (org-html-link link desc info)))

(org-export-define-derived-backend '11ty 'html
  :menu-entry
  '(?1 "Export to 11ty JS"
       ((?1 "To 11tydata.json and HTML file" org-11ty-export-to-11tydata-and-html)
        (?j "To 11ty.js file" org-11ty-export-to-11ty)
        (?o "To Org file" org-11ty-export-to-org)
        (?b "As buffer" org-11ty-export-as-11ty) ))
  :translate-alist '((export-block . org-11ty-export-block)
                     (export-snippet . org-11ty-export-snippet)
                     (link . org-11ty-link))
  ;; '((template . org-11ty-template))
  :options-alist
  '((:permalink "ELEVENTY_PERMALINK" nil nil)
    (:categories "ELEVENTY_CATEGORIES" nil nil split)
    (:base-dir "ELEVENTY_BASE_DIR" nil nil)
    (:modified "MODIFIED" nil nil)
    (:file-name "ELEVENTY_FILE_NAME" nil nil)
    (:collections "ELEVENTY_COLLECTIONS" nil nil split)))

(provide 'ox-11ty)
;;; ox-11ty.el ends here
