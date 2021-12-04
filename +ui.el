;;; ~/.doom.d/+ui.el -*- lexical-binding: t; -*-


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-solarized-light)
;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

(toggle-frame-maximized)

(when IS-MAC
  (setq ns-pop-up-frames nil))

(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

;; change evil initial mode state
(menu-bar-mode t)

(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)


(use-package highlight-global
  :init
  (progn
    (setq-default highlight-faces
                  '(('hi-red-b . 0)
                    ('hi-aquamarine . 0)
                    ('hi-pink . 0)
                    ('hi-blue-b . 0)))))

(use-package  symbol-overlay
  :defer
  )
