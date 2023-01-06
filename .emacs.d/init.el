;;; init.el --- My Emacs Configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Julian Orchard

;; Author: Julian Orchard <git@julianorchard.co.uk>
;; Keywords: lisp, init, configuration

;; Date Created: 2022-11-02
;; Date Updated: 2022-12-07

;;; License: 

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

;; I'm not sure if I'm the first to move from a literate version
;; of my Emacs configuration to a normal code version, but it's
;; certainly strange.

;; Thre reasons for doing so are basically along the lines of
;; wanting to feel closer to my config, and also that I've never
;; actually experienced what it's like to work directly with the
;; Emacs files... I feel like that left me initially a little
;; stunted in my Emacs journey. 

;; As I've been writing more, and more, custom Elisp, I've been
;; getting more and more annoyed with having to work within the
;; constraints of a single, massive org document...

;; These constraints are pretty self-imposed; there's no reason
;; for me to be using a single Org document. But splitting it all
;; up just feels a bit annoying.

;; That's why you're reading this now!


;;; Code:


;;; Initial Setup Stuff ----------

;; Version check
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old. This config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old. Please upgrade if possible."))

;; Home setting, load path
(cd (getenv "HOME"))
(add-to-list 'load-path
             (concat (getenv "HOMEPATH") "/.emacs.d/lisp/"))

;; Garbage collection
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))

;; Fullscreen Emacs window... I like this, but it's very slow, and I
;; often find myself manually resizing it far quicker than it loads
;; (especially on Windows 10, where I use Emacs a lot).
;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Startup Message
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Emacs")

;; My details, pretty unused
(setq user-full-name "Julian Orchard")
(setq user-mail-address "hello@julianorchard.co.uk")

;; Lockfiles and Backups
(setq backup-directory-alist `(("." . "~/.cache/emacs/")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 5
      version-control t)
(setq create-lockfiles nil)

;; No dialogues
(setq use-dialog-box nil)

;; Yes No => y/n
(defalias 'yes-or-no-p 'y-or-n-p)


;; Quit with Esc Key
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Text
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) 
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)


;;; Package Init ---------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; Aesthetics -----------------

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

;; Default and Italic
(set-face-attribute 'default nil :font "Fira Code Retina" :height 105)
(set-face-attribute 'italic nil :font "Fira Mono" :height 105)

;; Emoji
(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package modus-themes)

(defun set-dark-theme ()
    (interactive)
    "Sets the dark version of the default theme"
    ;; (set-background-color "black")
    ;; (set-foreground-color "white")
    (load-theme 'modus-vivendi))

(defun set-light-theme ()
    (interactive)
    "Sets the light version of the default theme"
    ;; (set-background-color "white")
    ;; (set-foreground-color "black")
    (load-theme 'modus-operandi))

(if (member (string-to-number (substring (current-time-string) 11 13))
            (number-sequence 7 20))
    (set-light-theme)
    (set-dark-theme))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20))
  :config
  (display-time-mode 1)
  (if (eq system-type 'gnu/linux)
      (display-battery-mode 1)))
(use-package solaire-mode
  :init (solaire-global-mode +1))
(use-package all-the-icons
  :ensure t)


;;; Ivy

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)	
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)


;; Company

(use-package company
  :custom
  (company-global-modes '(not shell-mode eaf-mode))
  :config
  (global-company-mode 1))
;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))


;;; Evil

(use-package evil
  :ensure t
  :demand
  ;; :require undo-fu
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  ;; (evil-set-leader nil (kbd ","))
  :config
  (evil-mode 1)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; (evil-define-key 'normal 'global (kbd "<leader>k") 'jdo/test)
  ;; (evil-define-key 'normal 'global (kbd "<leader>w") 'jdo/lol)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; Evil Collection, Surroud, Commentary
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode +1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package undo-fu)


;;; Avy Bindings
(use-package avy
  :ensure t
  :bind
  ("C-c C-a" . avy-goto-char))


;;; Eshell

(defalias 'ff 'find-file-other-window)
(defalias 'e 'find-file-other-window)
;;(defalias 'vim 'find-file) ;; afaik this isn't working 

(defalias 'less 'find-file-read-only-other-window)

(defalias 'cls '(clear 1))
(defalias 'd 'dired)
(defalias 'll '(ls -la))

(use-package eshell-prompt-extras
  :ensure t
  :bind
  ("C-x C-e" . eshell)
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))


;;; Yasnippet

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind
  ("C-c C-s" . 'jdo/create-snippet)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))


;;; Dired

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))


;;; Elcord

(if (and (eq system-type 'windows-nt)
         (equal user-login-name "julia"))
    (use-package elcord
      :config
      (elcord-mode 1)))


;;; Other

;; Misc Custom Funtions
(require 'jdo)

;; Language Mode Settings
(require 'lang)
