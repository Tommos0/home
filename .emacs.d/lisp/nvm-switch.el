;;; nvm-switch.el --- Description Switch nvm node version
;;
;; Copyright (C) 2022 Tom Klaver
;;
;; Author: Tom Klaver <https://github.com/tommos0>
;; Maintainer: Tom Klaver
;; Created: januari 17, 2022
;; Modified: januari 17, 2022
;; Version: 0.0.1
;; Keywords: convenience nvm node version
;; Homepage: https://github.com/tommos0/nvm-switch
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;;  This adds the interactive command `nvm-switch-select-node-version'
;;  that selects a node version from `nvm' and update `exec-path' and `PATH' in
;;  `process-environment'.
;;
;;; Code:

;;;###autoload
(defcustom nvm-switch-directory
  (file-name-as-directory
   (or (getenv "NVM_DIR")
       (expand-file-name "~/.nvm")))
  ".nvm directory"
  :type 'directory
  :group 'nvm-switch)

(defun nvm-switch-versions-folder ()
  (concat nvm-switch-directory
        (file-name-as-directory "versions")
        (file-name-as-directory "node")))

(defun nvm-switch-node-version-to-path (node-version)
  (concat (nvm-switch-versions-folder)
          (file-name-as-directory node-version)
          "bin"))

(defun nvm-switch-assert-node-version (node-version)
  (let ((node-path (nvm-switch-node-version-to-path node-version)))
    (unless (file-directory-p node-path)
      (error "%s %s" "No such directory" node-path))))

(defun nvm-switch-node-versions ()
  "List of available node versions"
  (sort (seq-remove (apply-partially #'string-prefix-p "." )
                    (directory-files (nvm-switch-versions-folder)))
        #'string-lessp))

(defun nvm-switch-filter-dir (list)
  "Filter list; remove strings including 'nvm-switch-directory"
  (seq-remove (apply-partially #'string-match-p nvm-switch-directory)
              list))

(defun nvm-switch-add-node-version-to-path (current-path node-version)
  (cons (nvm-switch-node-version-to-path node-version)
        (nvm-switch-filter-dir current-path)))

(defun nvm-switch-get-env-path ()
  (split-string (getenv "PATH")
                ":"))

(defun nvm-switch-update-env-path (node-version)
  (setenv "PATH"
          (string-join (nvm-switch-add-node-version-to-path (nvm-switch-get-env-path)
                                                            node-version)
                       ":")))

(defun nvm-switch-update-exec-path (node-version)
  (setq exec-path
        (nvm-switch-add-node-version-to-path exec-path node-version)))

;;;###autoload
(defun nvm-switch-select-node-version ()
  (interactive)
  (let ((node-version (completing-read
                       "Select NodeJS version: " (nvm-switch-node-versions))))
    (nvm-switch-assert-node-version node-version)
    (nvm-switch-update-env-path node-version)
    (nvm-switch-update-exec-path node-version)))

(provide 'nvm-switch)

;;; nvm-switch.el ends here
