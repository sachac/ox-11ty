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
         (collections (plist-get info :collections))
				 (extra (if (plist-get info :extra) (json-parse-string
																						 (plist-get info :extra)
																						 :object-type 'plist))))
		(append
		 extra
     (list :permalink permalink
           :date (if (listp date) (car date) date)
           :modified (if (listp modified) (car modified) modified)
           :title (if (listp title) (car title) title)
           :categories (if (stringp categories) (split-string categories) categories)
           :tags (if (stringp collections) (split-string collections) collections)))))

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
      (error "No permalink set")
      ;; (plist-put info :permalink (concat "/" (plist-get info :file-name)))
      )
    info))

(defun org-11ty--copy-files-and-replace-links (info)
  (let ((file-regexp "\\(?:src\\|href\\|poster\\)=\"\\(\\(file:\\)?.*?\\)\"")
        (destination-dir (file-name-directory (plist-get info :file-path)))
        file-all-urls file-name beg
				new-file file-re
				unescaped)
    (save-excursion
			(goto-char (point-min))
      (while (re-search-forward file-regexp nil t)
        (setq file-name (or (match-string 1) (match-string 2)))
				(setq file-name (save-match-data (if (string-match "^file:" file-name)
																						 (substring file-name 7)
																					 file-name)))
				(setq unescaped (replace-regexp-in-string
												 "%23" "#"
												 file-name))
				(setq new-file (concat
												(plist-get info :permalink)
												(file-name-nondirectory unescaped)))
				(unless (or (string-match "^[/#]" file-name) (org-url-p file-name))
					(condition-case err
							(copy-file unescaped destination-dir t)
						(error nil))
					(when (file-exists-p (expand-file-name (file-name-nondirectory
																									unescaped)
																								 destination-dir))
						(save-excursion
							(goto-char (point-min))
							(setq file-re (concat "\\(?: src=\"\\| href=\"\\| poster=\"\\)\\(\\(?:file://\\)?" (regexp-quote file-name) "\\)"))
							(while (re-search-forward file-re nil t)
								(replace-match (save-match-data (replace-regexp-in-string "#" "%23" new-file))
															 t t nil 1)))))))))

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

(defvar org-11ty-process-export-functions '(org-11ty--copy-files-and-replace-links)
	"List of functions to run after exporting.
They will be called with the org-11ty--get-info info.")

(defun org-11ty-export-to-11tydata-and-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((info (org-11ty--get-info subtreep visible-only))
         (file (org-11ty--base-file-name subtreep visible-only)))
    (plist-put info :file-path file)
    (when (file-name-directory file) (make-directory (file-name-directory file) :parents))
    (with-temp-file (concat file ".11tydata.json") (insert (json-encode (org-11ty--front-matter info))))
    (let ((body (org-export-as '11ty subtreep visible-only t ext-plist)))
      (with-temp-file (concat file ".html")
        (insert body)
				(run-hook-with-args 'org-11ty-process-export-functions info)))))

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
			;; handle file links
			(org-html-link link desc info)))

(defun org-11ty-src-block (src-block contents info)
	"Wrap source blocks in raw directives."
	(format "{%% raw %%}\n%s\n{%% endraw %%}\n"
					(org-html-src-block src-block contents info)))

(org-export-define-derived-backend '11ty 'html
  :menu-entry
  '(?1 "Export to 11ty JS"
       ((?1 "To 11tydata.json and HTML file" org-11ty-export-to-11tydata-and-html)
        (?o "To Org file" org-11ty-export-to-org)
        (?b "As buffer" org-11ty-export-as-11ty) ))
  :translate-alist '((export-block . org-11ty-export-block)
                     (export-snippet . org-11ty-export-snippet)
										 (src-block . org-11ty-src-block)
                     (link . org-11ty-link))
  ;; '((template . org-11ty-template))
  :options-alist
  '((:permalink "ELEVENTY_PERMALINK" nil nil)
    (:categories "ELEVENTY_CATEGORIES" nil nil split)
    (:base-dir "ELEVENTY_BASE_DIR" nil nil)
    (:base-url "ELEVENTY_BASE_URL" nil nil)
    (:modified "MODIFIED" nil nil)
    (:file-name "ELEVENTY_FILE_NAME" nil nil)
    (:extra "ELEVENTY_EXTRA" nil nil)
    (:collections "ELEVENTY_COLLECTIONS" nil nil split)))

(provide 'ox-11ty)
;;; ox-11ty.el ends here
