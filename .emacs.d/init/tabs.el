;; TAB settings
;;
;; Set some basic requirements around input (width 4, don't use tabs), but then also
;; define some magic that will replace tabs if we happen to save a file that has tabs
;; inadvertently added to it, as well as nuke trailing whitespace.  This is the default
;; behaviour; (setq indent-tabs-mode t) in a hook for modes you want left alone.

(setq-default tab-width        4)   ;; used by untabify
(setq-default indent-tabs-mode nil)

;; If indent-tabs-mode is off, nuke whitespace and untabify before saving.  I do this by
;; default now, with modes "opting out" of the practice by setting indent-tabs-mode to
;; non-nil.  (Only enable find-file-hooks hook if you're an aggressive asshole.)

;(add-hook 'find-file-hooks
;  (lambda ()
;    (if (not indent-tabs-mode)
;      (save-excursion
;        (untabify (point-min) (point-max))
;        (delete-trailing-whitespace)
;        ))))

(add-hook 'write-file-hooks
  (lambda ()
    (if (not indent-tabs-mode)
      (save-excursion
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        ))))

