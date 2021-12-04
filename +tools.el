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
  :defer
  :commands (vr/replace vr/query-replace))

(use-package! visual-regexp-steroids
  :defer
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

(use-package! youdao-dictionary
  :defer)

(use-package! multiple-cursors
  :defer
  :init
  (progn
    (setq mc/cmds-to-run-once
          '(
            counsel-M-x
            zilongshanren/my-mc-mark-next-like-this))
    (bind-key* "C-s-l" 'mc/edit-lines)
    (bind-key* "C-s-f" 'mc/mark-all-dwim)
    (bind-key* "s-." 'mc/mark-next-like-this)
    (bind-key* "C-s-," 'mc/mark-previous-like-this)
    (bind-key* "s->" 'mc/unmark-next-like-this)
    (bind-key* "s-<" 'mc/unmark-previous-like-this)
    (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

    ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
    (define-prefix-command 'endless/mc-map)
    ;; C-x m is usually `compose-mail'. Bind it to something
    ;; else if you use this command.
    (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
    (define-key endless/mc-map "i" #'mc/insert-numbers)
    (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
    (define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
    (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
    (define-key endless/mc-map "r" #'mc/reverse-regions)
    (define-key endless/mc-map "s" #'mc/sort-regions)
    (define-key endless/mc-map "l" #'mc/edit-lines)
    (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
    (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)))
