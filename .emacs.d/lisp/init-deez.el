;;;   deez.el  ---  Custom functions. -*- lexical-binding: t; -*-

;; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Author:       Julian Orchard <jorchard@pm.me>
;; Keywords:     lisp, functions
;; Date Created: 2022-11-02
;; Date Updated: 2023-07-07

;;; Description:

;; Contains custom functions I use occasionally for various things.

;; Also for more Elisp learning than my main `init.el` file.

;;; License:

;; See /LICENSE file in the root of this repository.

;;; Code:

;; * TODO: Add MIT License as snippet
;;   SCHEDULED: <2023-01-09>
;;   Julian <jorchard@pm.me>


(defun replace-in-string (what with in)
   (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun custom/insert-license ()
  "Check if there's a LICENSE file defined in root and, if so,
               get the text from the file as a commented out region."
  (interactive)
  (if (custom/license-exist)
      (insert-file-contents (custom/license-path))
    (insert "There is no LICENSE file for this project.")))

(defun custom/license-path ()
  "Path to LICENSE file."
  (concat (custom/shell-command-to-string "git rev-parse --show-toplevel") "/LICENSE"))

(defun custom/license-exist ()
  "Is there a LICENSE file in this Git repository?"
  (if (file-exists-p (custom/license-path))
      t nil))

(defun custom/is-this-a-repo ()
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
           "Description:  "    (custom/if-evil-insert-state))))

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

(defun custom/shell-command-to-string (c)
  "Take a shell command as an argument and remove newline chars."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string c)))

(defun custom/test ()
  "Custom testing function."
  (interactive)
  (insert "Testing! "))

(defun custom/if-evil-insert-state ()
  "Check if we're using Evil Mode and go into Insert State if so."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (evil-insert-state)))

(defun custom/org-subheading ()
  "A custom insert-subheading for Org Mode."
  (interactive)
  (org-only-function)
  (org-insert-subheading t)
  (custom/if-evil-insert-state))

;; Bind this to something using emacs bindings too! Could be very useful!
(defun custom/basic-time-date-stamp ()
  "A simple time stamp binding for Org Mode."
  (interactive)
  (insert (format-time-string "\[%Y-%m-%d %a %H:%M\]")))

(defun custom/time-date-stamp ()
  "A custom time/date stamp mainly used in Org Mode for updating notes."
  (interactive)
  (custom/org-subheading)
  (insert "Note ")
  (custom/basic-time-date-stamp)
  (insert ": \n")
  (custom/if-evil-insert-state))

(defun custom/create-snippet ()
  (interactive)
  (yas-new-snippet)
  (if (not file-directory-p "~/config/snippets/")
      (copy-directory (locate-user-emacs-file "snippets") "~/config/snippets")))


;; Org Functions

(defun org-only-function ()
  "Check if we're in an Org buffer: exit if not. Sometimes we don't want this, but often we do."
  (if (not (eq major-mode 'org-mode))
      (user-error "ERROR: We're not in an Org buffer right now.")))

(defun custom/org-copy-under-heading ()
  "Copies text under Org subtree."
  (interactive)
  (org-only-function)
  (org-mark-subtree)
  (next-line 1)
  (kill-ring-save
   (region-beginning)
   (region-end))
  (deactivate-mark))

;; Weekly Specific Org Document Functions (how I track my hours)

(defun weekly/new-hour-track ()
  "Generate a new set of hours for weekly."
  (interactive)
  (org-only-function)
  (insert
   (concat
    "* " (format-time-string "[%d/%m/%Y]")
    "\n** Monday\n** Tuesday\n** Wednesday\n** Thursday\n** Friday")))

(defun org-time-stamp-string ()
  "Basically, I think this creates a temp-buffer to copy the string from.

   https://emacs.stackexchange.com/questions/69009/
   how-to-get-org-time-stamp-to-return-timestamp-rather-than-inserting
  "
  (with-temp-buffer
    (org-mode)
    (org-time-stamp-inactive nil)
    (buffer-substring (point-min) (point-max))))

;; TODO: Somehow replace the [ and ]... \\[ or \\]?
(defun weekly/new-holiday-day ()
  "Insert a standard day of holiday on the given Org point."
  (interactive)
  (org-only-function)
  (org-end-of-line)
  (org-newline-and-indent)
  (setq ts ((org-time-stamp-string)))
  (insert (format ":LOGBOOK:\nCLOCK: [%s 09:00]--[%s 16:30]\n:END:" ts)))

(provide 'init-deez)
