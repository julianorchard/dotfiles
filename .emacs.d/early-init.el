(when (getenv-internal "DEBUG")
  (setq-default
   debug-on-error t
   init-file-debug t))

(setq-default
 load-prefer-newer t
 mode-line-format nil)

(setq-default
 default-frame-alist
 '((background-color . "#3F3F3F")
   (bottom-divider-width . 1)
   (foreground-color . "#DCDCCC")
   (fullscreen . maximized)
   (horizontal-scroll-bars . nil)
   (left-fringe . 8)
   (menu-bar-lines . 0)
   (right-divider-width . 1)
   (right-fringe . 8)
   (tool-bar-lines . 0)
   (undecorated . t)
   (vertical-scroll-bars . nil)))
