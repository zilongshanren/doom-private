;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-

;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "zilongshanren"
      user-mail-address "guanghui8827@gmail.com")

(global-flycheck-mode -1)

;; (add-hook 'emacs-lisp-mode-hook #'(lambda ()
;;                                     (flycheck-mode -1)))
;; (add-hook! emacs-lisp-mode
;;            #'(lambda () (flycheck-mode -1)))

;; (add-hook! emacs-lisp-mode
;;   (flycheck-mode -1))

(add-hook! 'emacs-lisp-mode-hook
  (flycheck-mode -1))
