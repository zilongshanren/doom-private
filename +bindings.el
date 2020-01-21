;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'zilongshanren/smart-open-line)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "C-c t") 'org-capture)
(define-key global-map (kbd "<f8>") 'zilongshanren/show-current-buffer-major-mode)

(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; (global-set-key (kbd "C-.") 'company-capf)

;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-d") 'zilongshanren/my-mc-mark-next-like-this)
(global-set-key (kbd "<f5>") 'zilongshanren/run-current-file)

(setq doom-localleader-key ",")

(map! :leader
  "0" 'winum-select-window-0-or-10
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "8" 'split-window-below
  "9" 'split-window-right
  )


(map! (:map override
        "C-s" #'swiper
        "s-x" #'kill-region))

(map! :leader
      :desc "counsel-M-x" :nmv "SPC" #'counsel-M-x

      :nmv "hdk" #'describe-key
      :nmv "hdf" #'describe-function
      :nmv "hdv" #'describe-variable
      :nmv "TAB" #'spacemacs/alternate-buffer
      :n "v" #'er/expand-region

      (:prefix "r"
        "i" #'ivy-resume)

      (:prefix "f"
        "j" #'dired-jump)

      (:prefix "e"
        "l" #'flycheck-list-errors
        "n" #'next-error
        "p" #'previous-error)


      (:prefix "n"
        "l" #'evil-ex-nohighlight)

      (:prefix "g"
        "s" #'magit-status)

      (:prefix "w"
        "/" #'evil-window-vsplit
        "-" #'evil-window-split
        "m" #'doom/window-maximize-buffer))

(map! :map dired-mode-map
      :ng "o" #'dired-find-file-other-window
  "C-k" 'zilongshanren/dired-up-directory
  "<RET>" 'dired-find-alternate-file
  "E" 'dired-toggle-read-only
  "C" 'dired-do-copy
  "<mouse-2>" 'my-dired-find-file
  "`" 'dired-open-term
  "p" 'peep-dired-prev-file
  "n" 'peep-dired-next-file
  ;; "gr" 'revert-buffer
  "z" 'dired-get-size
  "c" 'dired-copy-file-here
  "J" 'counsel-find-file
  "f" 'zilongshanren/open-file-with-projectile-or-counsel-git
  ")" 'dired-omit-mode)
