;;; better-defaults/config.el -*- lexical-binding: t; -*-

(after! dired
  (progn
    (require 'dired-x)
    (require 'dired-aux)
    (add-hook 'dired-mode-hook 'dired-async-mode)
    (setq dired-listing-switches "-alh")
    (setq dired-guess-shell-alist-user
          '(("\\.pdf\\'" "open")
            ("\\.docx\\'" "open")
            ("\\.\\(?:djvu\\|eps\\)\\'" "open")
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
            ("\\.\\(?:xcf\\)\\'" "open")
            ("\\.csv\\'" "open")
            ("\\.tex\\'" "open")
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
             "open")
            ("\\.\\(?:mp3\\|flac\\)\\'" "open")
            ("\\.html?\\'" "open")
            ("\\.md\\'" "open")))

    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

    ;; always delete and copy recursively
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)



    (define-key dired-mode-map "e" 'ora-ediff-files)

    (defvar dired-filelist-cmd
      '(("vlc" "-L")))

    ))
