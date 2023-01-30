;;;   init-aesthetics.el  ---  Stuff to do with how Emacs looks. -*- lexical-binding: t; -*-

;; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Author:       Julian Orchard <jorchard@pm.me>
;; Keywords:     lisp, functions
;; Date Created: 2023-01-25
;; Date Updated: 2023-01-30

;;; Description:

;; Contains everything to do with how Emacs looks. I use the modus-themes from
;; the amazing Protesilaos, Doom Emacs modeline, and, of course, Nyan mode.

;; The only bits omitted are the 'remove top menu' and things; they're in
;; `init.el`, because it's nice to have them removed ASAP after loading Emacs.

;;; License:

;; See /LICENSE file in the root of this repository.

;;; Code:

;; Emoji

(use-package emojify
  :hook (after-init . global-emojify-mode))


;; Modus Theme

(use-package modus-themes
  :ensure t)

(defun set-dark-theme ()
    (interactive)
    "Sets the dark version of the default theme"
    (load-theme 'modus-vivendi))

(defun set-light-theme ()
    (interactive)
    "Sets the light version of the default theme"
    (load-theme 'modus-operandi))

(defun timed-theme () 
  "Call either the set-light-theme or set-dark-theme functions, 
depending on the time. If it's late in the evening, we want a nice
dark theme for our vision."
  (interactive)
  (if (member (string-to-number (substring (current-time-string) 11 13))
              (number-sequence 7 20))
      (set-light-theme)
    (set-dark-theme)))

(timed-theme)
;; (run-at-time '15 minutes' (timed-theme))


;; Modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20))
  :config
  (display-time-mode 1)
  (if (eq system-type 'gnu/linux)
      (display-battery-mode 1)))


;; Solaire - makes window colour change on focus

(use-package solaire-mode
  :init (solaire-global-mode +1))


;; All The Icons: better icons

(use-package all-the-icons
  :ensure t)


;; Nyan

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))


(provide 'init-aesthetics)
