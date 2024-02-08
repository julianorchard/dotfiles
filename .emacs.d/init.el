;; A fairly minimal Emacs config heavily inspired in layout (and choice for the nice-to-haves
;; I don't really care as much about) by https://github.com/Ronmi/emacs

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

(setq package-enable-at-startup nil
      straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package emacs
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
  :config (add-hook 'prog-mode-hook #'ws-butler-mode))
(use-package all-the-icons)
(use-package catppuccin-theme
  :config (setq catppuccin-flavor 'macchiato)
  (catppuccin-reload))
(use-package nyan-mode :config (nyan-mode))

;; Counsel == Consult
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common))
  :config (global-company-mode t))
(use-package company-box
  :after (company all-the-icons)
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
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
  :config (marginalia-mode t)
  :after vertico)
(use-package all-the-icons-completion
  :after marginalia
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package evil
  :after undo-fu
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
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode +1))
(use-package evil-surround
  :after evil
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
;; (use-package flycheck
;;   :init (global-flycheck-mode))
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
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package org
  :commands (org-capture org-agenda org-tempo)
  :bind (("C-x C-a" . org-agenda)
         ("C-x C-c" . org-capture))
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
