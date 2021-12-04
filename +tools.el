;;; ~/.doom.d/+tools.el -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(after! expand-region
  (defadvice er/prepare-for-more-expansions-internal
      (around helm-ag/prepare-for-more-expansions-internal activate)
    ad-do-it
    (let ((new-msg (concat (car ad-return-value)
                           ", H to highlight in buffers"
                           ", / to search in project, "
                           "f to search in files, "
                           "b to search in opened buffers"))
          (new-bindings (cdr ad-return-value)))
      (cl-pushnew
       '("H" (lambda ()
               (call-interactively
                'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (call-interactively
                '+default/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
       '("f" (lambda ()
               (call-interactively
                'counsel-find-file)))
       new-bindings)
      (cl-pushnew
       '("b" (lambda ()
               (call-interactively
                '+default/search-buffer)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings)))))
