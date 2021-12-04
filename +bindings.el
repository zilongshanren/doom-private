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

;; (global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
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
        "C-s" #'+default/search-buffer
        "s-x" #'kill-region))

(map! :leader
      :desc "open M-x" "SPC" #'execute-extended-command
      )

(map! :leader
      "TAB" #'spacemacs/alternate-buffer
      "v" #'er/expand-region

      (:prefix "r"
       "i" #'vertico-repeat)

      (:prefix "h"
       "dk" 'describe-key
       "df"  'describe-function
       "dv" 'describe-variable
       "h" 'zilongshanren/highlight-dwim
       "c" 'zilongshanren/clearn-highlight)

      (:prefix "o"
       "o" #'zilongshanren/helm-hotspots)

      (:prefix "b"
       "h" #'+doom-dashboard/open)

      (:prefix-map ("f" . "file")
       "j" #'dired-jump
       (:prefix-map ("e" . "env")
        "d" 'doom/goto-private-config-file
        "i" 'doom/find-file-in-emacsd))

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

(map! :after dired
      (:map dired-mode-map
       :ne "o" #'dired-find-file-other-window
       :ne "C-k" 'zilongshanren/dired-up-directory
       :ne "<RET>" 'dired-find-alternate-file
       :ne "E" 'dired-toggle-read-only
       :ne "C" 'dired-do-copy
       :ne "<mouse-2>" 'my-dired-find-file
       :ne "`" 'dired-open-term
       :ne "p" 'peep-dired-prev-file
       :ne "n" 'peep-dired-next-file
       ;;e"gr" 'revert-buffer
       :ne "z" 'dired-get-size
       :ne "c" 'dired-copy-file-here
       :ne "J" 'counsel-find-file
       :ne "f" 'zilongshanren/open-file-with-projectile-or-counsel-git
       :ne ")" 'dired-omit-mode))
