;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

(load! "+funcs")
(load! "+ui")
(load! "+misc")

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org-notes/")



(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08))

(load! "+bindings")
(load! "+evil")
