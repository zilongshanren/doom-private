;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

(load! "+funcs")
(load! "+ui")
(load! "+misc")

(load! "+bindings")
(load! "+evil")
(load! "+org")
(load! "+tools")

(setq doom-localleader-key ",")

(defun eh-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
    (pyim-cregexp-build result)))

(advice-add 'orderless-regexp :around #'eh-orderless-regexp)
