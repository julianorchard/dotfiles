;;;   org-config.el  ---  Org Mode configuration. -*- lexical-binding: t; -*-

;; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Author:       Julian Orchard <jorchard@pm.me>
;; Keywords:     lisp, init, configuration
;; Date Created: 2022-11-02
;; Date Updated: 2023-01-09

;;; Description:

;; My settings for Emacs Org Mode, including Org Capture,
;; export settings, and prettifying settings.

;;; License:

;; See /LICENSE file in the root of this repository.

;;; Code:



(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . here/org-mode-setup)
  (org-mode . here/org-mode-symbols-setup)
  ;; (org-mode . here/org-font-setup)
  ;; (org-mode . here/org-capture-workflow)
  :bind
  ("C-x C-a" . org-agenda)
  ("C-x C-c" . org-capture)
  :config
  (setq org-ellipsis "  ⌄ ")
  (set-face-underline 'org-ellipsis nil)
  (set-face-attribute 'italic nil :font "Fira Mono" :height global-text-height)
  (set-face-attribute 'org-quote nil :inherit 'default :slant 'italic)
  (setq header-line-format " ")

  (setq org-agenda-files
        '("~/org/" "~/config/"))

  (setq org-duration-format (quote h:mm))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'content)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PEND(p)" "|" "DONE(d)" "CANC(c)")
          (sequence "|" "FIVE(5)" "FOUR(4)" "THRE(3)" "TWO(2)" "ONE(1)")))

  (setq org-priority-faces
        '((?A . (:foreground "red"))
          (?B . (:foreground "orange"))
          (?C . (:foreground "yellow"))))

  (setq org-capture-templates

        '(("w" "Wessex")
          ("wt" "Task" entry (file+headline "~/org/wessex.org" "Tasklist")
           "* TODO  %?\nDEADLINE: %t" :prepend t)
          ("wm" "Meeting" entry (file+headline "~/org/wessex-meetings.org" "On-going")
           "* %U %?\n** (Rough) Minutes\n- What we talked about\n- What someone said\n** Tasks\n*** TODO Something to work on\nDEADLINE: %t\n" :prepend t)

          ("p" "Personal")
          ("pt" "General Task" entry (file+headline "~/org/personal.org" "Tasklist")
           "* TODO  %?\nDEADLINE: %t" :prepend t)
          ("pc" "Computing Task" entry (file+headline "~/org/personal.org" "Computer")
           "* TODO  %?\nDEADLINE: %t\n- [[repo][" :prepend t)
          ("pb" "Birthday" entry (file "~/org/misc/birthday.org")
           "* %(config/org-capture-prompt \"Person's Name\" 'persons-name)\nSCHEDULED: %(org-read-date)")

          ("j" "Journal")
          ("jj" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* Entry for %U\n%?")
          ("jt" "T Weekly" entry (file+olp+datetree "~/org/t-journal.org")
           "* %U

Metrics ------------------------------------------

Height:

Weight:


Questions ----------------------------------------

- /How are you feeling?/:

- /What have you done this week?/:

- /What have you done today?/:

- /What is your favourite animal?/:

- /What is your favourite food?/:


Notes --------------------------------------------\n\n%?"

           :tree-type week)
          )
        )
  )

;; By Storax, https://storax.github.io/blog/2016/05/02/org-capture-tricks/
  (defvar org-capture-hist nil
    "History of prompt answers for org capture.")
  (defun config/org-capture-prompt (prompt variable)
    "PROMPT for string, save it to VARIABLE and insert it."
    (make-local-variable variable)
    (set variable (read-string (concat prompt ": ") nil org-capture-hist)))

  (defun org-document-new (path)
    "Create a new document and prompt for a file name."
    (interactive)
    (let ((name (read-string "Document Name: ")))
      (expand-file-name (format "%s.org" name) path))
    (find-file (insert-file "~/Documents/org-pdf/template.org")))

(setq org-publish-project-alist
      '(("config"
         :base-directory "~/config/"
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/julianorchard.github.io/config/"
         :section-numbers nil
         :html-head "
<link rel=\"stylesheet\" href=\"../src/org-style.css\" type=\"text/css\"/>"
         :html-preamble "
  <nav>
    <ul>
      <li>
        <a class=\"link\" href=\"/\">Home</a>
      </li>
      <li>
        <a class=\"link\" href=\"/about/\">About</a>
      </li>
      <li>
        <a class=\"link\" href=\"/posts/\">Posts</a>
      </li>
      <li>
        <a class=\"link\" href=\"/config/\">Config</a>
      </li>
    </ul>
  </nav>"
         :html-postamble "
    <footer>
      <div class=\"left\">
        <p>
          Email: <a href=\"mailto:hello@julianorchard.co.uk\" class=\"highlight\">hello@julianorchard.co.uk</a>
          <br />
          LinkedIn: <a href=\"https://linkedin.com/in/JulianOrchard\" class=\"highlight\">julianorchard</a>
          <br />
          Twitter: <a href=\"https://twitter.com/jdorchard/\">@jdorchard</a>
          <br />
          Mastodon: <a rel=\"me\" href=\"https://mastodon.social/@jdo\" class=\"highlight\">@jdo</a>
        </p>
      </div><!--
    --><div class=\"right\">
        <p>
          Check out this site on <a href=\"https://github.com/julianorchard/julianorchard.github.io\" class=\"highlight\" target=\"_blank\">GitHub</a>!
          <br />
          About <a href=\"/privacy/\" class=\"highlight\">Privacy Policy</a>.
        </p>
        <p>
          Copyright Julian Orchard, 2022
        </p>
      </div>
    </footer>
    <script src=\"/src/script.js\"></script>"
         :force t)))

(defun here/org-mode-symbols-setup ()
  "Prettify Symbols Setup for Org Documents"

  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "○" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "→" ) prettify-symbols-alist)
  (push '("#+END_SRC" . "←" ) prettify-symbols-alist)
  (push '("#+begin_src" . "→" ) prettify-symbols-alist)
  (push '("#+end_src" . "←" ) prettify-symbols-alist)
  (push '("#+BEGIN_EXAMPLE" . "e.g. →" ) prettify-symbols-alist)
  (push '("#+END_EXAMPLE" . "←" ) prettify-symbols-alist)
  (push '("#+begin_example" . "e.g. →" ) prettify-symbols-alist)
  (push '("#+end_example" . "←" ) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "“" ) prettify-symbols-alist)
  (push '("#+END_QUOTE" . "”" ) prettify-symbols-alist)
  (push '("#+begin_quote" . "“" ) prettify-symbols-alist)
  (push '("#+end_quote" . "”" ) prettify-symbols-alist)
  (push '("#+title:" . "⒯") prettify-symbols-alist)
  (push '("#+TITLE:" . "⒯") prettify-symbols-alist)
  (push '("#+options:" . "⌥") prettify-symbols-alist)
  (push '("#+OPTIONS:" . "⌥") prettify-symbols-alist)
  (push '("#+author:" . "⒜") prettify-symbols-alist)
  (push '("#+AUTHOR:" . "⒜") prettify-symbols-alist)
  (push '("#+date:" . "⒟") prettify-symbols-alist)
  (push '("#+DATE:" . "⒟") prettify-symbols-alist)
  (push '("#+description:" . "…") prettify-symbols-alist)
  (push '("#+DESCRIPTION:" . "…") prettify-symbols-alist)
  (push '("#+results:" . " result ⇒ ") prettify-symbols-alist)
  (push '("#+RESULTS:" . " result ⇒ ") prettify-symbols-alist)
  (push '("#+property:" . "∷") prettify-symbols-alist)
  (push '("#+PROPERTY:" . "∷") prettify-symbols-alist)
  (push '("[#A]" . "⬆") prettify-symbols-alist)
  (push '("[#B]" . "■") prettify-symbols-alist)
  (push '("[#C]" . "⬇") prettify-symbols-alist)
  (prettify-symbols-mode)

  ;; Nicer Org Bullets
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun here/org-mode-setup ()
   (org-indent-mode)
   (visual-line-mode 1))

(defun here/org-mode-visual-fill ()
   (setq visual-fill-column-width 100
         visual-fill-column-center-text t)
   (visual-fill-column-mode 1))
(use-package visual-fill-column
   :hook (org-mode . here/org-mode-visual-fill))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  ;; (org-bullets-bullet-list '("\u200b" " " "◉" "-")))
 (org-bullets-bullet-list '("●" "○")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (progn
;;   (add-to-list 'load-path "~/.emacs.d/site-lisp")
;;   (require 'org-pretty-table)
;;   (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode))))

(require 'org-tempo)

(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(provide 'org-config)
