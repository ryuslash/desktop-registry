;;; desktop-registry.el --- Keep a central registry of desktop files -*- lexical-binding: t -*-

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: convenience
;; Version: 1.1.0

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

(defcustom desktop-registry-list-switch-buffer-function
  #'switch-to-buffer-other-window
  "The function to use to switch to the desktop list buffer."
  :group 'desktop-registry
  :type 'function)

(defvar desktop-registry--history nil
  "History variable for `desktop-registry'.")

(defun desktop-registry--canonicalize-dir (dir)
  "Canonicalize DIR for use."
  (directory-file-name (expand-file-name dir)))

;;;###autoload
(defun desktop-registry-current-desktop (&optional default)
  "Get the name of the currently loaded desktop.

Returns DEFAULT when `desktop-dirname' is nil."
  (if desktop-dirname
      (let ((canonical
             (desktop-registry--canonicalize-dir desktop-dirname)))
        (car (cl-find-if (lambda (d) (equal (cdr d) canonical))
                         desktop-registry-registry)))
    default))

;;;###autoload
(defun desktop-registry-add-directory (dir &optional name)
  "Add DIR to the desktop registry, possibly using NAME."
  (interactive (list (read-directory-name "Directory: ")
                     (if (equal current-prefix-arg '(4))
                         (read-string "Name: "))))
  (let* ((clean-dir (desktop-registry--canonicalize-dir dir))
         (label (or name (file-name-base clean-dir))))
    (cond
     ((cl-find clean-dir desktop-registry-registry
               :key 'cdr :test 'equal)
      (message "Directory %s already registered" clean-dir))
     ((cl-find label desktop-registry-registry :key 'car :test 'equal)
      (error "Name %s already used" label))
     (t (customize-save-variable
         'desktop-registry-registry
         (cons (cons label clean-dir) desktop-registry-registry))))))

;;;###autoload
(defun desktop-registry-add-current-desktop (&optional name)
  "Add the currently opened desktop file to `desktop-registry-registry'.

If NAME is specified use that as the name for the registry entry."
  (interactive (list (if (equal current-prefix-arg '(4))
                         (read-string "Name: "))))
  (unless desktop-dirname
    (error "No desktop loaded"))
  (desktop-registry-add-directory desktop-dirname name))

(defun desktop-registry--completing-read (&optional prompt
                                                    default-current)
  "Ask the user to pick a desktop directory.

PROMPT specifies the prompt to use when asking, which defaults to
\"Desktop: \". DEFAULT-CURRENT specifies whether to use the
current desktop as default value."
  (let ((prompt (or prompt "Desktop: "))
        (default (and default-current
                      (desktop-registry-current-desktop))))
    (completing-read prompt desktop-registry-registry nil nil nil
                     'desktop-registry--history default)))

;;;###autoload
(defun desktop-registry-remove-desktop (desktop)
  "Remove DESKTOP from the desktop registry."
  (interactive (list (desktop-registry--completing-read "Remove: " t)))
  (let ((spec (assoc desktop desktop-registry-registry)))
    (if spec
        (customize-save-variable
         'desktop-registry-registry
         (delete spec desktop-registry-registry))
      (error "Unknown desktop: %s" desktop))))

;;;###autoload
(defun desktop-registry-rename-desktop (old new)
  "Rename desktop OLD to NEW."
  (interactive (list (desktop-registry--completing-read "Rename: " t)
                     (read-string "to: ")))
  (let ((spec (assoc old desktop-registry-registry)))
    (if (not spec)
        (error "Unknown desktop: %s" old)
      (setf (car spec) new)
      (customize-save-variable 'desktop-registry-registry
                               desktop-registry-registry))))

;;;###autoload
(defun desktop-registry-change-desktop (name)
  "Change to the desktop named NAME."
  (interactive (list (desktop-registry--completing-read "Switch to: ")))
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

(defun desktop-registry--prepare-row (data)
  "Format a row of DATA for `tabulated-list-entries'."
  (let* ((name (car data))
         (dir (cdr data))
         (existsp (and (file-exists-p dir)
                       (file-directory-p dir))))
    (list name (vector name (if existsp "yes" "no") dir))))

(defun desktop-registry--refresh-list ()
  "Fill `tabulated-list-entries' with registered desktops."
  (setq tabulated-list-entries
        (mapcar #'desktop-registry--prepare-row
                desktop-registry-registry)))

(define-derived-mode desktop-registry-list-mode tabulated-list-mode
  "Desktop Registry"
  "Major mode for listing registered desktops.

\\<desktop-registry-list-mode-map>
\\{desktop-registry-list-mode-map}"
  (setq tabulated-list-format [("Label" 30 t)
                               ("Exists" 6 nil)
                               ("Location" 0 t)]
        tabulated-list-sort-key '("Label"))
  (add-hook 'tabulated-list-revert-hook #'desktop-registry--refresh-list)
  (tabulated-list-init-header))

;;;###autoload
(defun desktop-registry-list-desktops ()
  "Display a list of registered desktops."
  (interactive)
  (let ((buffer (get-buffer-create "*Desktop Registry*")))
    (with-current-buffer buffer
      (desktop-registry-list-mode)
      (desktop-registry--refresh-list)
      (tabulated-list-print))
    (funcall desktop-registry-list-switch-buffer-function buffer))
  nil)

(provide 'desktop-registry)
;;; desktop-registry.el ends here
