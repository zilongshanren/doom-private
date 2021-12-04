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

(use-package! visual-regexp
  :commands (vr/replace vr/query-replace))

(use-package! visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace)))

(use-package! discover-my-major
  :defer t
    :init
    (progn
      (map! :leader
            (:prefix "m" :desc "discover major key and action"
             "hm" #'discover-my-major))))
