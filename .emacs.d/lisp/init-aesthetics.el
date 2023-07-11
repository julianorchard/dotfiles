;;;   init-aesthetics.el  ---  Stuff to do with how Emacs looks. -*- lexical-binding: t; -*-

;; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Author:       Julian Orchard <jorchard@pm.me>
;; Keywords:     lisp, functions
;; Date Created: 2023-01-25
;; Date Updated: 2023-07-11

;;; Description:

;; Contains everything to do with how Emacs looks. I use the modus-themes from
;; the amazing Protesilaos, Doom Emacs modeline, and, of course, Nyan mode.

;; The only bits omitted are the 'remove top menu' and things; they're in
;; `init.el`, because it's nice to have them removed ASAP after loading Emacs.

;;; License:

;; See /LICENSE file in the root of this repository.

;;; Code:

;; Emoji


;; General Guff Removal
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 5)
(menu-bar-mode -1)
(show-paren-mode t)
(setq visible-bell t
      split-width-threshold 1)

;; Lines and Columns
(column-number-mode)
(setq display-line-numbers-type 'relative)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
		   (display-line-numbers-mode 1))))

(dolist (rm-ln-hook '(org-mode-hook))
  (add-hook rm-ln-hook (lambda () (display-line-numbers-mode 0))))

;; General Fonts
(defvar global-text-height 90)
(set-face-attribute 'default nil :font "BlexMono Nerd Font Mono" :height global-text-height)
(set-face-attribute 'italic nil :font "BlexMono Nerd Font" :height global-text-height)

;; Emoji 
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Catppuccin Theme
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'macchiato)
  (catppuccin-reload))

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

;; Startup Message
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Emacs says, 'hello''
")

(provide 'init-aesthetics)
