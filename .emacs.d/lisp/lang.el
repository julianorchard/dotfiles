;;; lang.el --- Language Mode Settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Julian Orchard

;; Author: Julian Orchard <git@julianorchard.co.uk>

;;; Code

(use-package ahk-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ahk" . ahk-mode)))

(setq latex-run-command "xelatex")

(defun here/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(global-set-key (kbd "C-c C-1") 'here/revert-buffer-no-confirm)

(setq revert-without-query '(".pdf"))

(use-package auctex
  :defer t
  :ensure t
  :config 
  (require 'tex-mik)

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

(use-package powershell
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ps1" . powershell)))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(setq auto-mode-alist
      (append '(("\\.\\(vbs\\|wsf\\)$" . vbscript-mode))
              auto-mode-alist))

(use-package web-mode
  :mode
  (("\\.tpl\\.php\\'" . web-mode)
   ("\\.erb\\'" . web-mode)))

(use-package yaml-mode)

(provide 'lang)
