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

(use-package! cal-china-x
  :defer
  :config
  (progn
    (setq mark-holidays-in-calendar t)
    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays))))

(use-package! helm
  :defer
  (map! :map helm-map
        "C-j" 'helm-next-line
        "C-k" 'helm-previous-line))

(after! prodigy
  (progn
    (map! :map prodigy-mode-map
          "H" 'prodigy-display-process)

    ;; define service
    (prodigy-define-service
      :name "Hugo Server"
      :command "hugo"
      :args '("server" "-D" "--navigateToChanged" "-t" "even")
      :cwd blog-admin-dir
      :tags '(hugo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "hugo Deploy"
      :command "bash"
      :args '("./deploy.sh")
      :cwd blog-admin-dir
      :tags '(hugo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    ;; (defun refresh-chrome-current-tab (beg end length-before)
    ;;   (call-interactively 'zilongshanren/browser-refresh--chrome-applescript))
    ;; ;; add watch for prodigy-view-mode buffer change event
    ;; (add-hook 'prodigy-view-mode-hook
    ;;           #'(lambda() (set (make-local-variable 'after-change-functions) #'refresh-chrome-current-tab)))
    ))
