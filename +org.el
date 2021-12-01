;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org-notes/")

 ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
  (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))
