;;; bm-bookmarks.el --- A simple URL bookmark manager for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Tom Klaver
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, bookmarks, url

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; bm-bookmarks is a simple bookmark manager for Emacs that allows you to save
;; and quickly access URL bookmarks.  It provides functions to save bookmarks
;; with descriptions and search through them using completion interfaces.

;; Features:
;; - Save URLs with descriptions
;; - Search bookmarks with completion
;; - Customizable default action (defaults to browse-url)
;; - JSON storage format

;;; Code:

(require 'json)

;;; Customization

(defgroup bm-bookmarks nil
  "A simple bookmark manager for Emacs."
  :group 'convenience)

(defcustom bm-default-file (concat user-emacs-directory "bookmarks.json")
  "Default file to store URL bookmarks."
  :type 'file
  :group 'bm-bookmarks)

(defcustom bm-default-action #'browse-url
  "Default action to perform when a bookmark is selected.
This should be a function that takes a URL as its argument."
  :type 'function
  :group 'bm-bookmarks)

;;; Internal Functions

(defun bm--current-timestamp ()
  "Return the current timestamp as a string."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun bm--read-bookmarks ()
  "Read bookmarks from the JSON file.
Returns an empty vector if the file doesn't exist."
  (if (file-exists-p bm-default-file)
      (let ((json-array-type 'vector))
        (json-read-file bm-default-file))
    []))

(defun bm--write-bookmarks (bookmarks)
  "Write BOOKMARKS to the JSON file."
  (with-temp-file bm-default-file
    (insert (json-encode bookmarks))))

(defun bm--format-fixed-width (str width)
  "Format STR to have a fixed WIDTH.
If STR is longer than WIDTH, it will be truncated and '...' added."
  (if (> (length str) width)
      (concat (substring str 0 (- width 3)) "...")
    (format (format "%%-%ds" width) str)))

(defun bm--format-bookmark-entry (bookmark)
  "Format a single bookmark entry for display.
BOOKMARK is an alist containing URL and text fields.
Returns a cons cell (display-string . bookmark-data)."
  (let-alist bookmark
    (cons 
     (format "%s | %s" 
             (bm--format-url .url)
             (bm--format-description .text))
     bookmark)))

(defun bm--format-url (url)
  "Format URL with fixed width and highlighting.
Returns a propertized string limited to 30 characters."
  (propertize 
   (bm--format-fixed-width url 60)
   'face 'font-lock-default-face))

(defun bm--format-description (text)
  "Format bookmark description with fixed width and highlighting.
Returns a propertized string limited to 40 characters."
  (propertize 
   (bm--format-fixed-width text 40)
   'face 'font-lock-string-face))

(defun bm--create-completion-candidates (bookmarks)
  "Create completion candidates from BOOKMARKS list.
Returns a list of (display-string . bookmark-data) pairs."
  (mapcar #'bm--format-bookmark-entry bookmarks))

(defun bm--handle-selection (selected candidates)
  "Handle user's bookmark selection.
SELECTED is the user's choice from completion.
CANDIDATES is the list of completion candidates."
  (when selected
    (let* ((bookmark (alist-get selected candidates nil nil #'equal))
           (url (alist-get 'url bookmark)))
      (bm-perform-action url))))

;;; Public Functions

;;;###autoload
(defun bm-save-bookmark (url text)
  "Save a bookmark with URL and TEXT."
  (interactive "sEnter URL: \nsEnter Description: ")
  (let* ((existing-bookmarks (bm--read-bookmarks))
         (new-bookmark `((url . ,url)
                        (text . ,text)
                        (timestamp . ,(bm--current-timestamp))))
         (bookmarks (vconcat existing-bookmarks `[,new-bookmark])))
    (bm--write-bookmarks bookmarks)
    (message "Bookmark saved to %s!" bm-default-file)))

(defun bm-perform-action (url)
  "Perform the configured action on the selected URL."
  (funcall bm-default-action url))

;;;###autoload
(defun bm-search-bookmarks ()
  "Search bookmarks interactively.
Presents a completion interface for searching through bookmarks,
displaying both URLs and descriptions in a formatted view."
  (interactive)
  (let* ((bookmarks (append (bm--read-bookmarks) nil))
         (candidates (bm--create-completion-candidates bookmarks))
         (selected (completing-read "Search bookmarks: " candidates nil t)))
    (bm--handle-selection selected candidates)))

(provide 'bm-bookmarks)

;;; bm-bookmarks.el ends here
