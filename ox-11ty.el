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

(defun org-11ty-template (contents info)
  (let* ((date (org-export-data (plist-get info :date) info))
         (title (org-export-data (plist-get info :title) info))
         (permalink (org-export-data (plist-get info :permalink) info))
         (categories (org-export-data (plist-get info :categories) info))
         (collections (org-export-data (plist-get info :collections) info))
         (front-matter (json-encode
                        (list :permalink permalink
                              :date date
                              :title title
                              :categories (split-string categories)
                              :tags (split-string collections)))))
    (format
     "module.exports = class {
  data() {
    return %s;
  }
  render() {
    return %s;
  }
}"
     front-matter
     (json-encode-string contents))))

(defun org-11ty-export-as-11ty (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as 11ty file."
  (interactive)
  (org-export-to-buffer '11ty "*org 11ty export*" async subtreep visible-only body-only ext-plist))

(defun org-11ty-export-to-11ty (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((info
          (org-combine-plists
           (org-export--get-export-attributes '11ty subtreep visible-only)
           (org-export--get-buffer-attributes)
           (org-export-get-environment '11ty subtreep)))
         (base-file-name (concat (or
                                  (if (string= (file-name-base (plist-get info :file-name)) "")
                                      (concat (plist-get info :file-name) "index")
                                    (plist-get info :file-name))
                                  (org-export-output-file-name "" subtreep))
                                 ".11ty.js"))
         (file
          (if (plist-get info :base-dir)
              (expand-file-name base-file-name (plist-get info :base-dir))
            base-file-name)))
    (make-directory (file-name-directory file) :parents)
    (org-export-to-file '11ty file
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend '11ty 'html
  :menu-entry
  '(?1 "Export to 11ty JS"
       ((?b "As buffer" org-11ty-export-as-11ty) 
        (?1 "To file" org-11ty-export-to-11ty)))
  :translate-alist
  '((template . org-11ty-template))
  :options-alist
  '((:permalink "PERMALINK" nil nil)
    (:categories "CATEGORIES" nil 'split)
    (:base-dir "ELEVENTY_BASE_DIR" nil nil)
    (:file-name "FILE_NAME" nil nil)
    (:collections "ELEVENTY_COLLECTIONS" nil 'split)))

;;; ox-11ty.el ends here
