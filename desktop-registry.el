;; -*- lexical-binding: t -*-
;;; desktop-registry.el --- Keep a central registry of desktop files

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: convenience
;; Version: 1.0.0

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

;; This module provides functions and a global minor mode that lets
;; you track a central registry of desktop files.  This is useful when
;; you use desktop files as project files and want to be able to
;; switch between them quickly.

(require 'desktop)

;;; Code:

(defgroup desktop-registry nil
  "Customization group for desktop-registry."
  :group 'desktop
  :prefix 'desktop-registry)

(defcustom desktop-registry-registry nil
  "The registry of desktop files."
  :group 'desktop-registry
  :type '(repeat (cons string directory)))

(defvar desktop-registry--history nil
  "History variable for `desktop-registry'.")

;;;###autoload
(defun desktop-registry-add-directory (dir)
  "Add DIR to the desktop registry."
  (interactive "DDirectory: ")
  (let* ((clean-dir (directory-file-name (expand-file-name dir)))
         (label (file-name-base clean-dir)))
    (unless (assoc label desktop-registry-registry)
      (customize-save-variable
       'desktop-registry-registry
       (cons (cons label clean-dir) desktop-registry-registry)))))

;;;###autoload
(defun desktop-registry-add-current-desktop ()
  "Add the currently opened desktop file to `desktop-registry-registry'."
  (interactive)
  (unless desktop-dirname
    (error "No desktop loaded"))
  (desktop-registry-add-directory desktop-dirname))

;;;###autoload
(defun desktop-registry-change-desktop (name)
  "Change to the desktop named NAME."
  (interactive
   (list (completing-read "Directory: " desktop-registry-registry nil nil
                          nil 'desktop-registry--history)))
  (desktop-change-dir (cdr (assoc name desktop-registry-registry))))

;;;###autoload
(define-minor-mode desktop-registry-auto-register
  "Automatically add saved desktops to the registry."
  :global t
  (if desktop-registry-auto-register
      (add-hook 'desktop-save-hook
                'desktop-registry-add-current-desktop)
    (remove-hook 'desktop-save-hook
                 'desktop-registry-add-current-desktop)))

(provide 'desktop-registry)
;;; desktop-registry.el ends here