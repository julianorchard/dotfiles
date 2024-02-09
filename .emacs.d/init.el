;;; init.el --- Emacs configuration file

;;; Commentary:

;; A fairly minimal Emacs config heavily inspired in layout (and choice for the nice-to-haves
;; I don't really care as much about) by https://github.com/Ronmi/emacs

;;; Code:

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "This Emacs config requires v%s or higher" minver)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(setq-default straight-use-package-by-default t)
(if (fboundp 'straight-use-package)
    (straight-use-package 'use-package))

(use-package emacs
  :defines display-line-numbers-type
  :init
  ;; Load times
  (setq gc-cons-percentage 0.6
        gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook (lambda ()
                               (setq gc-cons-threshold 800000)))
  ;; Guff removal
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 5)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (display-fill-column-indicator-mode)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Escape escapes
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; Line numbering plz
  (column-number-mode)
  (setq display-line-numbers-type 'relative)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda ()
		     (display-line-numbers-mode 1))))
  (dolist (rm-ln-hook '(org-mode-hook))
    (add-hook rm-ln-hook (lambda () (display-line-numbers-mode 0))))
  ;; I want to be completely transparent with you right now (I'm using Emacs 29.1+)
  (set-frame-parameter nil 'alpha-background 90)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; ComicShanns
  (defvar global-text-height 90)
  (set-face-attribute 'default nil :font "ComicShannsMono Nerd Font Mono" :height global-text-height)
  ;; Prot video on this was very helpful
  (setq display-buffer-alist
        '(
          ("\\*Warnings\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((mode-line-format . none))))
          ("\\*Org \\(Select\\|Note\\)\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ))
  :custom
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (make-backup-files nil)
  (size-indication-mode t)
  (smerge-refine-ignore-whitespace t))
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))
(use-package saveplace
  :hook (after-init . save-place-mode))
(use-package diminish)
(use-package ws-butler
  :diminish ws-butler-mode
  :functions ws-butler-mode
  :config (add-hook 'prog-mode-hook #'ws-butler-mode))
(use-package all-the-icons)
(use-package catppuccin-theme
  :defines catppuccin-flavor
  :config (setq catppuccin-flavor 'macchiato)
  :functions catppuccin-reload
  :config (catppuccin-reload))
(use-package nyan-mode
  :functions nyan-mode
  :config (nyan-mode))

;; Counsel == Consult
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common))
  :functions global-company-mode
  :config (global-company-mode t))
(use-package company-box
  :after (company all-the-icons)
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :defines company-box-icons-alist
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons))
(use-package consult-lsp)
(use-package consult
  :bind
  (("M-s r" . consult-ripgrep)))
(use-package consult-company)

;; Vertico == Ivy ??
(use-package vertico
  :ensure t
  ;; This is exactly how systemcrafters does it
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  ;; :map minibuffer-local-map
  ;; ("M-h" . backward-kill-word))
  :custom (vertico-cycle t)
  :init (vertico-mode))
(use-package savehist
  :init (savehist-mode)
  :after vertico)
(use-package marginalia ;; Annotations for Vertico
  :functions marginalia-mode
  :config (marginalia-mode t)
  :after vertico)
(use-package all-the-icons-completion
  :after marginalia
  :functions all-the-icons-completion-mode
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(use-package yasnippet
  :diminish yas-minor-mode
  :functions yas-reload-all
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package evil
  :after undo-fu
  :demand
  :defines
  evil-want-keybinding
  evil-want-integration
  evil-want-C-u-scroll
  evil-want-C-i-jump
  evil-undo-system
  evil-split-window-below
  evil-vsplit-window-right
  tmux-mappings
  :functions
  evil-mode
  evil-global-set-key
  evil-ex-define-cmd
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  :preface
  ;; From the yay-evil-emacs config:
  ;; https://github.com/ianyepan/yay-evil-emacs/blob/master/config.org#vi-keybindings
  (defun save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (evil-mode 1)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)
  ;; Custom tmux-like key maps
  (keymap-set global-map "C-a" tmux-mappings))
;; Org mappings (this is currently causing evil-mode to fuck up somehow)
(defvar-keymap tmux-org-mappings
  "a" 'org-agenda
  "c" 'org-capture)
;; Navigational mappings with Ctrl+A (as I have configured for Tmux)
;; These rely on Evil (see the line in evil above)
(defvar-keymap tmux-mappings
  "f" 'find-file
  "s" 'split-window-right
  "d" 'split-window-below
  "h" 'evil-window-left
  "k" 'evil-window-up
  "j" 'evil-window-down
  "l" 'evil-window-right
  "n" 'next-buffer
  "p" 'previous-buffer
  "r" 'eval-region
  "!" 'delete-other-windows
  "q" 'delete-window
  "o" tmux-org-mappings)
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :functions evil-collection-init
  :config (evil-collection-init))
(use-package evil-commentary
  :after evil
  :functions evil-commentary-mode
  :config (evil-commentary-mode +1))
(use-package evil-surround
  :after evil
  :functions global-evil-surround-mode
  :config (global-evil-surround-mode 1))
(use-package undo-fu)

;; (use-package lsp-mode
;;   :hook
;;   ((
;;     dockerfile-mode
;;     go-mode
;;     json-mode
;;     markdown-mode+
;;     python-mode
;;     terraform-mode
;;     toml-mode
;;     typescript-mode
;;     yaml-mode
;;     ) . lsp-mode)
;;   :custom
;;   (lsp-dired-mode t)
;;   (lsp-eldoc-enable-hover t)
;;   (lsp-eldoc-render-all nil)
;;   (lsp-prefer-flymake nil)
;;   (lsp-signature-auto-activate
;;    '(:on-trigger-char :after-completion :on-server-request)))
;; (use-package lsp-docker)
;; (use-package dap-mode)
;; (use-package typescript-mode)
;; (use-package web-mode)
(use-package flycheck
  :functions global-flycheck-mode
  :init (global-flycheck-mode))
;; (use-package go-mode
;;   :custom
;;   (gofmt-command "goimports")
;;   (flycheck-go-gofmt-executable "goimports")
;;   (lsp-clients-go-server "gopls")
;;   (flycheck-go-vet-executable "go vet")
;;   (flycheck-go-vet-shadow t)
;;   (go-eldoc-gocode-args '("-cache"))
;;   (godoc-reuse-buffer t)
;;   :hook
;;   (before-save . gofmt-before-save))
;; (use-package yaml-mode)
;; (use-package toml-mode)
;; (use-package markdown-mode+)
;; (use-package markdown-preview-mode)
;; (use-package terraform-mode
;;   :init (setq terraform-format-on-save t))
;; (use-package hcl-mode)

(use-package dired-subtree
  :after dired
  :functions dired-subtree-toggle dired-subtree-cycle
  :defines dired-mode-map
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package org
  :commands (org-capture org-agenda org-tempo)
  :defines
  org-duration-format
  org-capture-templates
  :init
  (setq org-agenda-files
	'("~/Documents/org/"))
  (setq org-duration-format (quote h:mm))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'content)
  (setq org-todo-keywords
        '(( sequence "TODO(t)" "NEXT(n)" "PEND(p)" "|" "DONE(d)" "CANC(c)")))
  (setq org-capture-templates
        '(("w" "Work")
          ("wt" "Task" entry (file+headline "~/Documents/org/work.org" "Tasklist")
           "* TODO  %?\nDEADLINE: %t" :prepend t)
          ("p" "Personal")
          ("pt" "General Task" entry (file+headline "~/Documents/org/personal.org" "Tasklist")
           "* TODO  %?\nDEADLINE: %t" :prepend t)
          ("j" "Journal")
          ("jj" "Journal" entry (file+olp+datetree "~/Documents/org/journal/journal.org")
           "* Entry for %U\n%?"))))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "○")))
(use-package evil-org
  :after (org evil)
  :functions evil-org-agenda-set-keys
  :defines evil-org-mode
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init)
;;; init.el ends here
