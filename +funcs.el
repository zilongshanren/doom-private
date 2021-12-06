;;; ~/.doom.d/+funcs.el -*- lexical-binding: t; -*-

(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")


(defmacro th/define-context-key (keymap key dispatch)
  "Define KEY in KEYMAP to execute according to DISPATCH.
        DISPATCH is a form that is evaluated and should return the
        command to be executed.
        If DISPATCH returns nil, then the command normally bound to KEY
        will be executed.
        Example:
          (th/define-context-key hs-minor-mode-map
             (kbd \"<C-tab>\")
             (cond
              ((not (hs-already-hidden-p))
               'hs-hide-block)
              ((hs-already-hidden-p)
               'hs-show-block)))
        This will make <C-tab> show a hidden block.  If the block is
        shown, then it'll be hidden."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
                 :filter ,(lambda (&optional ignored)
                            ,dispatch))))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))




(defadvice persp-switch (after my-quit-helm-perspectives activate)
  (setq hydra-deactivate t))
