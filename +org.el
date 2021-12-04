;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(require 'cl-lib)

(defun zilongshanren/directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))



;; Screenshot
(defun zilongshanren//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun zilongshanren/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (progn
    (setq final-image-full-path (concat basename ".png"))
    (call-process "screencapture" nil nil nil "-s" final-image-full-path)
    (if (executable-find "convert")
        (progn
          (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
          (shell-command-to-string resize-command-str)))
    (zilongshanren//insert-org-or-md-img-link "./" (concat basename ".png")))
  (insert "\n"))

(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
(defun zilongshanren/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; VH: I changed this line from
      ;; (if (bh/is-project-p)
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun zilongshanren/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "typescript" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-show-subtree)
        (goto-char (point-min))
        (beginning-of-line 2)
        (narrow-to-region (point) (point-max))
        (org-map-entries
         '(when (member (org-get-todo-state) org-done-keywords)
            (org-todo (car org-todo-keywords))))))))

(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

(defun zilong/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun zilong/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun zilong/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'zilong/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))



(use-package! org-super-agenda
  :defer
  :init
  (setq org-super-agenda-groups
        '((:name "Important"
           :priority "A")
          (:name "Quick Picks"
           :effort< "0:30")
          (:name "Next Items"
           :tag ("NEXT" "outbox"))
          (:priority<= "B"
           :scheduled future)))

  (add-hook 'org-mode-hook 'org-super-agenda-mode))

(after! org-superstart
    (setq org-superstar-headline-bullets-list '("☰" "☷" "☯" "☭"))
    (setq org-ellipsis " ▼ "))
(after! org
  (progn
    ;; If you intend to use org, it is recommended you change this!
    (setq org-directory "~/org-notes/")

    (defvar org-agenda-dir ""
      "gtd org files location")

    (defvar deft-dir ""
      "deft org files locaiton")

    (defvar blog-admin-dir ""
      "blog-admin files location")

    (if IS-WINDOWS
        (setq
         org-agenda-dir "d:/org-notes"
         deft-dir "d:/org-notes"
         blog-admin-dir "d:/zilongshanren.com")
      (setq
       org-agenda-dir "~/org-notes"
       deft-dir "~/org-notes"
       blog-admin-dir "~/zilongshanren.com"))

    ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
    (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
      (let* ((origin-contents (ad-get-arg 1))
             (fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat
                "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
        (ad-set-arg 1 fixed-contents)))

    ;; disable < auto pair for org mode
    ;; disable {} auto pairing in electric-pair-mode for web-mode
    (add-hook
     'org-mode-hook
     (lambda ()
       (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

    (require 'org-tempo)
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    (setq org-agenda-log-mode-items '(clock closed state))

    ;; 当操作org agenda的buffer的时候自动保存，也可以直接在agenda buffer按save
    ;; 会有bug, 导致org 报错
    ;; (advice-add 'org-agenda-clock-in :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-clock-out :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-schedule :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-store-log-note :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-pomodoro :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-deadline :after 'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-priority :after 'org-save-all-org-buffers)

    ;; (defun th/org-outline-context-p ()
    ;;   (re-search-backward org-outline-regexp))
    ;; ;; Some usages
    ;; (th/define-context-key org-mode
    ;;                        (kbd "RET")
    ;;                        (when (th/outline-context-p)
    ;;                          'org-insert-heading-respect-content))

    ;; Jump out of a TeX macro when pressing TAB twice.
    ;; (th/define-context-key TeX-mode-map (kbd "TAB")
    ;;                        (when (and (= 1 (length (this-command-keys-vector)))
    ;;                                   (equal last-command-event (elt (this-command-keys-vector) 0))
    ;;                                   (TeX-current-macro))
    ;;                          #'th/TeX-goto-macro-end)))

    (defun zilong/org-return (&optional indent)
      "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.
When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
      (interactive)
      (let ((context (if org-return-follows-link (org-element-context)
                       (org-element-at-point))))
        (cond
         ;; In a table, call `org-table-next-row'.  However, before first
         ;; column or after last one, split the table.
         ((or (and (eq 'table (org-element-type context))
                   (not (eq 'table.el (org-element-property :type context)))
                   (>= (point) (org-element-property :contents-begin context))
                   (< (point) (org-element-property :contents-end context)))
              (org-element-lineage context '(table-row table-cell) t))
          (if (or (looking-at-p "[ \t]*$")
                  (save-excursion (skip-chars-backward " \t") (bolp)))
              (insert "\n")
            (org-table-justify-field-maybe)
            (call-interactively #'org-table-next-row)))
         ;; On a link or a timestamp, call `org-open-at-point' if
         ;; `org-return-follows-link' allows it.  Tolerate fuzzy
         ;; locations, e.g., in a comment, as `org-open-at-point'.
         ((and org-return-follows-link
               (or (and (eq 'link (org-element-type context))
                        ;; Ensure point is not on the white spaces after
                        ;; the link.
                        (let ((origin (point)))
                          (org-with-point-at (org-element-property :end context)
                            (skip-chars-backward " \t")
                            (> (point) origin))))
                   (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil t)
                   (org-in-regexp org-any-link-re nil t)))
          (call-interactively #'org-open-at-point))
         ;; Insert newline in heading, but preserve tags.
         ((and (not (bolp))
               (let ((case-fold-search nil))
                 (org-match-line org-complex-heading-regexp)))
          ;; At headline.  Split line.  However, if point is on keyword,
          ;; priority cookie or tags, do not break any of them: add
          ;; a newline after the headline instead.
          (let ((tags-column (and (match-beginning 5)
                                  (save-excursion (goto-char (match-beginning 5))
                                                  (current-column))))
                (string
                 (when (and (match-end 4) (org-point-in-group (point) 4))
                   (delete-and-extract-region (point) (match-end 4)))))
            ;; Adjust tag alignment.
            (cond
             ((not (and tags-column string)))
             (org-auto-align-tags (org-align-tags))
             (t (org--align-tags-here tags-column))) ;preserve tags column
            (end-of-line)
            (org-show-entry)
            (if indent (newline-and-indent) (newline))
            (when string (save-excursion (insert (org-trim string))))))
         ;; In a list, make sure indenting keeps trailing text within.
         ((and indent
               (not (eolp))
               (org-element-lineage context '(item)))
          (let ((trailing-data
                 (delete-and-extract-region (point) (line-end-position))))
            (newline-and-indent)
            (save-excursion (insert trailing-data))))
         ((and (eolp) (org-at-item-p))
          (end-of-visible-line)
          (org-insert-item (org-at-item-checkbox-p)))
         (t
          ;; Do not auto-fill when point is in an Org property drawer.
          (let ((auto-fill-function (and (not (org-at-property-p))
                                         auto-fill-function)))
            (if indent
                (newline-and-indent)
              (newline)))))))


    (define-key org-mode-map (kbd "RET")
      'zilong/org-return)


    (setq org-complete-tags-always-offer-all-agenda-tags t)

    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; 调整orghabit 的显示长度
    (setq org-habit-graph-column 60)

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t) ;; ~50x speedup
    (setq org-agenda-span 'day)
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    (require 'cal-china)
    ;; diary for chinese birthday
    ;; https://emacs-china.org/t/topic/2119/14
    (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
      (if year
          (let* ((d-date (diary-make-date lunar-month lunar-day year))
                 (a-date (calendar-absolute-from-gregorian d-date))
                 (c-date (calendar-chinese-from-absolute a-date))
                 (date a-date)
                 (cycle (car c-date))
                 (yy (cadr c-date))
                 (y (+ (* 100 cycle) yy)))
            (diary-chinese-anniversary lunar-month lunar-day y mark))
        (diary-chinese-anniversary lunar-month lunar-day year mark)))


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-agenda-start-day "+0d")
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    (setq org-tags-match-list-sublevels nil)

    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'zilongshanren/org-insert-src-block)))
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    ;; }}
    (setq org-latex-default-class "ctexart")
    (setq org-latex-pdf-process
          '(
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"))

    (setq org-latex-listings t)

    (defun org-random-entry (&optional arg)
      "Select and goto a random todo item from the global agenda"
      (interactive "P")
      (if org-agenda-overriding-arguments
          (setq arg org-agenda-overriding-arguments))
      (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
      (let* ((today (org-today))
             (date (calendar-gregorian-from-absolute today))
             (kwds org-todo-keywords-for-agenda)
             (lucky-entry nil)
             (completion-ignore-case t)
             (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                                  org-agenda-buffer))
             (org-select-this-todo-keyword
              (if (stringp arg) arg
                (and arg (integerp arg) (> arg 0)
                     (nth (1- arg) kwds))))
             rtn rtnall files file pos marker buffer)
        (when (equal arg '(4))
          (setq org-select-this-todo-keyword
                (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                      (mapcar 'list kwds) nil nil)))
        (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
        (catch 'exit
          (org-compile-prefix-format 'todo)
          (org-set-sorting-strategy 'todo)
          (setq files (org-agenda-files nil 'ifmode)
                rtnall nil)
          (while (setq file (pop files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq rtn (org-agenda-get-day-entries file date :todo))
              (setq rtnall (append rtnall rtn))))

          (when rtnall
            (setq lucky-entry
                  (nth (random
                        (safe-length
                         (setq entries rtnall)))
                       entries))

            (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                             (org-agenda-error)))
            (setq buffer (marker-buffer marker))
            (setq pos (marker-position marker))
            (org-pop-to-buffer-same-window buffer)
            (widen)
            (goto-char pos)
            (when (derived-mode-p 'org-mode)
              (org-show-context 'agenda)
              (save-excursion
                (and (outline-next-heading)
                     (org-flag-heading nil))) ; show the next heading
              (when (outline-invisible-p)
                (show-entry))           ; display invisible text
              (run-hooks 'org-agenda-after-show-hook))))))

    ;;reset subtask
    (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

    ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)

    (setq org-plantuml-jar-path
          (expand-file-name "~/.doom.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.doom.d/ditaa.jar")

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((perl . t)
       (ruby . t)
       (shell . t)
       (dot . t)
       (typescript . t)
       (js . t)
       (latex .t)
       (python . t)
       (emacs-lisp . t)
       (plantuml . t)
       (C . t)
       (ditaa . t)))


    (require 'ox-md nil t)

    ;; define the refile targets
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))

    ;; C-n for the next org agenda item
    (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)
    (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)


    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      ;; 默认显示节假日
      (setq org-agenda-include-diary t)
      )
    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("s" "Code Snippet" entry
             (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "work" entry (file+headline org-agenda-file-work "Work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1)
            ("x" "Web Collections" entry
             (file+headline org-agenda-file-note "Web")
             "* %U %:annotation\n\n%:initial\n\n%?")
            ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	    ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U")
            ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1)))

    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
        "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
        (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
          (mapconcat #'identity
                     `(
                       ,(concat "* TODO " title)
                       ":PROPERTIES:"
                       ,(concat ":EXPORT_FILE_NAME: " fname)
                       ":END:"
                       "\n\n")          ;Place the cursor here finally
                     "\n")))

      (add-to-list 'org-capture-templates
                   '("h"                ;`org-capture' binding + h
                     "Hugo post"
                     entry
                     ;; It is assumed that below file is present in `org-directory'
                     ;; and that it has a "Blog Ideas" heading. It can even be a
                     ;; symlink pointing to the actual location of all-posts.org!
                     (file+headline org-agenda-file-blogposts "Blog Ideas")
                     (function org-hugo-new-subtree-post-capture-template))))

    ;;An entry without a cookie is treated just like priority ' B '.
    ;;So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (add-to-list 'org-agenda-custom-commands
                 '("r" "Daily Agenda Review"
                   ((agenda "" ((org-agenda-overriding-header "今日记录")
                                (org-agenda-span 'day)
                                (org-agenda-show-log 'clockcheck)
                                (org-agenda-start-with-log-mode nil)
                                (org-agenda-log-mode-items '(closed clock state))
                                (org-agenda-clockreport-mode t))))))


    (defvar zilongshanren-website-html-preamble
      "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
    (defvar zilongshanren-website-html-blog-head
      " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
    (setq org-publish-project-alist
          `(
            ("blog-notes"
             :base-directory "~/org-notes"
             :base-extension "org"
             :publishing-directory "~/org-notes/public_html/"

             :recursive t
             :html-head , zilongshanren-website-html-blog-head
             :publishing-function org-html-publish-to-html
             :headline-levels 4         ; Just the default for this project.
             :auto-preamble t
             :exclude "gtd.org"
             :exclude-tags ("ol" "noexport")
             :section-numbers nil
             :html-preamble ,zilongshanren-website-html-preamble
             :author "zilongshanren"
             :email "guanghui8827@gmail.com"
             :auto-sitemap t            ; Generate sitemap.org automagically...
             :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
             :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
             )
            ("blog-static"
             :base-directory "~/org-notes"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "~/org-notes/public_html/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("blog" :components ("blog-notes" "blog-static"))))



    (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
    ;; used by zilong/org-clock-sum-today-by-tags

    (define-key org-mode-map (kbd "s-p") 'org-priority)

    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

    ;; hack for org headline toc
    (defun org-html-headline (headline contents info)
      "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
      (unless (org-element-property :footnote-section-p headline)
        (let* ((numberedp (org-export-numbered-headline-p headline info))
               (numbers (org-export-get-headline-number headline info))
               (section-number (and numbers
                                    (mapconcat #'number-to-string numbers "-")))
               (level (+ (org-export-get-relative-level headline info)
                         (1- (plist-get info :html-toplevel-hlevel))))
               (todo (and (plist-get info :with-todo-keywords)
                          (let ((todo (org-element-property :todo-keyword headline)))
                            (and todo (org-export-data todo info)))))
               (todo-type (and todo (org-element-property :todo-type headline)))
               (priority (and (plist-get info :with-priority)
                              (org-element-property :priority headline)))
               (text (org-export-data (org-element-property :title headline) info))
               (tags (and (plist-get info :with-tags)
                          (org-export-get-tags headline info)))
               (full-text (funcall (plist-get info :html-format-headline-function)
                                   todo todo-type priority text tags info))
               (contents (or contents ""))
               (ids (delq nil
                          (list (org-element-property :CUSTOM_ID headline)
                                (org-export-get-reference headline info)
                                (org-element-property :ID headline))))
               (preferred-id (car ids))
               (extra-ids
                (mapconcat
                 (lambda (id)
                   (org-html--anchor
                    (if (org-uuidgen-p id) (concat "ID-" id) id)
                    nil nil info))
                 (cdr ids) "")))
          (if (org-export-low-level-p headline info)
              ;; This is a deep sub-tree: export it as a list item.
              (let* ((type (if numberedp 'ordered 'unordered))
                     (itemized-body
                      (org-html-format-list-item
                       contents type nil info nil
                       (concat (org-html--anchor preferred-id nil nil info)
                               extra-ids
                               full-text))))
                (concat (and (org-export-first-sibling-p headline info)
                             (org-html-begin-plain-list type))
                        itemized-body
                        (and (org-export-last-sibling-p headline info)
                             (org-html-end-plain-list type))))
            (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                  (first-content (car (org-element-contents headline))))
              ;; Standard headline.  Export it as a section.
              (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                      (org-html--container headline info)
                      (org-export-get-reference headline info)
                      (concat (format "outline-%d" level)
                              (and extra-class " ")
                              extra-class)
                      (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                              level
                              preferred-id
                              extra-ids
                              (concat
                               (and numberedp
                                    (format
                                     "<span class=\"section-number-%d\">%s</span> "
                                     level
                                     (mapconcat #'number-to-string numbers ".")))
                               full-text)
                              level)
                      ;; When there is no section, pretend there is an
                      ;; empty one to get the correct <div
                      ;; class="outline-...> which is needed by
                      ;; `org-info.js'.
                      (if (eq (org-element-type first-content) 'section) contents
                        (concat (org-html-section first-content "" info) contents))
                      (org-html--container headline info)))))))))
