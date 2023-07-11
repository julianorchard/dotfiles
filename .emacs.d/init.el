;;;   init.el  ---  My Emacs Configuration. -*- lexical-binding: t; -*-

;; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Author:       Julian Orchard <jorchard@pm.me>
;; Keywords:     lisp, init, configuration
;; Date Created: 2022-11-02
;; Date Updated: 2023-07-11

;;; Description:

;; Main Emacs init file.

;;; License:

;; See /LICENSE file in the root of this repository.

;;; Code:

;;; Initial -------------------------------

;; Version check
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old. This config requires v%s or higher." minver)))

;; Home setting, load path
(cd (getenv "HOME"))
(add-to-list 'load-path
             (concat (getenv "HOMEPATH") "/.emacs.d/lisp/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Garbage collection
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))

;; Set custom-file to keep it out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


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

;; Set C-a as a keymapping key
(setq map (make-sparse-keymap))
(define-key map (kbd "C-a") nil)
(global-set-key "\C-a" ctl-x-map)

;; Aesthetics/Theme
(require 'init-aesthetics)


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




;;; Includes ------------------------------

;; Misc. Custom Funtions
(require 'init-deez)

;; Language Specific Settings
(require 'init-lang)



;;; Completion ----------------------------

;; Ivy
(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x q") 'kill-this-buffer)

;; Motion Keys (move this probably)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind
  ("C-x C-s" . 'custom/create-snippet)
  :config
  (add-to-list 'yas-snippet-dirs
	       (locate-user-emacs-file "snippets")))

;; Company
(use-package company
  :custom
  (company-global-modes '(not shell-mode eaf-mode))
  :config
  (global-company-mode 1)
  (setq company-backends '((company-capf :with company-yasnippet))))

;;; Evil ----------------------------------

(use-package evil
  :ensure t
  :demand
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
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

;; Org Mode Settings
(require 'init-org)

;; Undo Fu
(use-package undo-fu)

;; Avy Bindings
(use-package avy
  :ensure t
  :bind
  ("C-x f" . avy-goto-char))

;; Eshell
(use-package eshell-prompt-extras
  :ensure t
  :bind
  ("C-x C-e" . eshell)
  :config
  (defalias 'ff 'find-file-other-window)
  (defalias 'e 'find-file-other-window)
  (defalias 'less 'find-file-read-only-other-window)
  (defalias 'cls '(clear 1))
  (defalias 'd 'dired)
  (defalias 'll '(ls -la))
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))


;; Dired
(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; Elcord
(if (and (eq system-type 'windows-nt)
         (equal user-login-name "julia"))
    (use-package elcord
      :config
      (elcord-mode 1)))
