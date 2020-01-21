;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-


(map! (:map override
        "C-s" #'swiper))

(map! :leader
      :desc "counsel-M-x" :nmv "SPC" #'counsel-M-x


      :nmv "hdk" #'describe-key

      ;; (:prefix-map ("f" . "file")
      ;;   "ed" #'doom/open-private-config)
      )
