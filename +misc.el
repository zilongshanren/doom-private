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

(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08))

;; prevent dired window press o to split into three column
;; (setq-default split-width-threshold 200)
(setq split-height-threshold nil)
(setq split-width-threshold 160)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a key press
(delete-selection-mode t)

;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)


(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/


(add-hook 'kill-emacs-hook #'zilongshanren/cleanup-recentf)


(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))


(advice-add 'describe-function-1 :after #'chunyang-advice-remove-button)

;; (defun zilong-ag-edit (function)
;;   (when (get-buffer "*helm-ag-edit*")
;;     (kill-buffer "*helm-ag-edit*"))
;;   (if (not (= (count-windows) 2))
;;       (progn
;;         (split-window-right))))

;; (defun zilong-after-ag-edit (function)
;;   (ivy-occur-grep-mode))

;; (advice-add 'helm-ag--edit :before #'zilong-ag-edit)

;; (advice-add 'helm-ag--edit :after #'zilong-after-ag-edit)
(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))



;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")

;; This line has very bad performance lose!!!!!!!!!!!!!!!!!!!
;; (set-default 'imenu-auto-rescan t)

;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; this settings will cause command `vc-annotate` failed.
;; 如果把 vc-handled-backends去掉，那么 vc-follow-symlinks 这个选项就会失效
;; 进而，如果你访问一个在版本控制里面的alias的话，它不会自动去访问原文件，这个是非常不爽的
;; (setq vc-handled-backends ())


;;Don't ask me when close emacs with process is running
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (flet ((process-list '())) ad-do-it))

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))



(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))



(add-hook 'find-file-hook 'spacemacs/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html

(add-hook 'mouse-leave-buffer-hook 'zilongshanren/stop-using-minibuffer)

(setq tags-add-tables nil)


;; http://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-diff-options "-w")

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; (when (and (spacemacs/system-is-mswindows) window-system)
;;   (setq w32-pipe-read-delay 0.5))

;; FIXME: --vimgrep will break ivy-occur with wgrep
;; (setq counsel-async-split-string-re "\r?\n")
;; (setq counsel-ag-base-command  "ag --vimgrep --nocolor --nogroup %s")


;; search chinse must add this line
;; https://emacs-china.org/t/emacs-helm-ag/6764
(if IS-WINDOWS
    (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos))
  (modify-coding-system-alist 'process "rg" '(utf-8 . utf-8)))



(define-abbrev-table 'global-abbrev-table '(

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")
                                            ;; email
                                            ("8me" "guanghui8827@gmail.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")
                                            ("82dx" "Cocos2D-X")

                                            ;; signature
                                            ("8zl" "zilongshanren")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))


(setq process-coding-system-alist (cons '("es" . (gbk .gbk)) process-coding-system-alist))
