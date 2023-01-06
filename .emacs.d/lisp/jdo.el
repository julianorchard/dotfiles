;;; jdo.el --- My Stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Julian Orchard

;; Author: Julian Orchard <git@julianorchard.co.uk>

;;; Code

(defvar gplv3-preambles
    "This file is part of NAME_OF_SOFTWARE.

NAME_OF_SOFTWARE is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

NAME_OF_SOFTWARE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with NAME_OF_SOFTWARE. If not, see <https://www.gnu.org/licenses/>.")

  (defun replace-in-string (what with in)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

  (defun insert-gplv3-preamble ()
    "Adds the GPL-v3 preamble text to a file."
    (interactive)
    (insert (replace-in-string "NAME_OF_SOFTWARE" (read-string "Enter software name: ") gplv3-preambles)))

(defun jdo/insert-license ()
  "Check if there's a LICENSE file defined in root and, if so, 
               get the text from the file as a commented out region."
  (interactive)
  (if (jdo/license-exist)
      (insert-file-contents (jdo/license-path))
    (insert "There is no LICENSE file for this project.")))

(defun jdo/license-path ()
  "Path to LICENSE file."
  (concat (jdo/shell-command-to-string "git rev-parse --show-toplevel") "/LICENSE"))

(defun jdo/license-exist ()
  "Is there a LICENSE file in this Git repository?"
  (if (file-exists-p (jdo/license-path))
      t nil))

(defun jdo/is-this-a-repo ()
  "Here, we use `git rev-parse --is-inside-work-tree` to see
                 if we're in a Git repository or not."
  (if (string-match "true" (shell-command-to-string "git rev-parse --is-inside-work-tree"))
      t nil))

(defun desc-date-format ()
  "Returns date in the right format for use by the 'desc' functions."
  (format-time-string "%Y-%m-%d" (current-time)))

(defun desc-update-date-line ()
  "Formats the replacement line nicely for the desc-update function."
  (concat "Date " "Updated: " (desc-date-format)))

(defun desc-add ()
  "Adds a little description to the top of a file."
  (interactive)
  (insert
   (concat "File Name:    "    (buffer-name) "\n"
           "Author:       "    "Julian Orchard <hello@julianorchard.co.uk>\n"
           "Date Added:   "    (desc-date-format) "\n"
           "Date " "Updated: " (desc-date-format) "\n" ; same initial date
           "Description:  "    (jdo/if-evil-insert-state))))

(defun desc-update ()
  "Looks for 'Date  Updated: ' string and replaces the date on save."
  (interactive)
  ;; This is very, very useful.
  ;; Set point in file to return to:
  (set-mark-command nil)
  (goto-char (point-min))
  (while (re-search-forward (concat "Date " "Updated:.*$") nil t)
    (replace-match (desc-update-date-line)))
  ;; Return to the point, here:
  (pop-to-mark-command))

;; desc-update Hook
(add-hook 'before-save-hook 'desc-update)

(defun jdo/shell-command-to-string (c)
  "Take a shell command as an argument and remove newline chars."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string c)))

(defun jdo/test ()
  "Custom testing function."
  (interactive)
  (insert "Testing! "))

(defun jdo/if-evil-insert-state ()
  "Check if we're using Evil Mode and go into Insert State if so."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (evil-insert-state)))

(defun jdo/org-subheading ()
  "A custom insert-subheading for Org Mode."
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-insert-subheading t))
  (jdo/if-evil-insert-state))

;; Bind this to something using emacs bindings too! Could be very useful!
(defun jdo/basic-time-date-stamp ()
  "A simple time stamp binding for Org Mode."
  (interactive)
  (insert (format-time-string "\[%Y-%m-%d %a %H:%M\]")))

(defun jdo/time-date-stamp ()
  "A custom time/date stamp mainly used in Org Mode for updating notes."
  (interactive)
  (jdo/org-subheading)
  (insert "Note ")
  (jdo/basic-time-date-stamp)
  (insert ": \n")
  (jdo/if-evil-insert-state))

(defun jdo/org-copy-under-heading ()
  "Copies text under Org subtree."
  (interactive)
  (org-mark-subtree)
  (next-line 1)
  (kill-ring-save
   (region-beginning)
   (region-end))
  (deactivate-mark))

(defun jdo/create-snippet ()
  (interactive)
  (yas-new-snippet)
  (if (not file-directory-p "~/config/snippets/")
      (copy-directory (locate-user-emacs-file "snippets") "~/config/snippets")))

(provide 'jdo)
