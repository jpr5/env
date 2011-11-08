;; TAB settings
;;
;; Set some basic requirements around input (width 4, don't use tabs), but then
;; also define some magic that will replace tabs if we happen to save a file
;; that has tabs inadvertently added to it, as well as nuke trailing whitespace.
;; This is the default behaviour; (setq indent-tabs-mode t) in a hook for modes
;; you want left alone.

(setq-default tab-width        4)   ;; used by untabify
(setq-default indent-tabs-mode nil)


(setq-default delete-trailing-whitespace-mode t)

(add-hook 'write-file-hooks
  (lambda ()
    (unless indent-tabs-mode)
     (save-excursion
      (untabify (point-min) (point-max))
      (when delete-trailing-whitespace-mode
        (delete-trailing-whitespace))
      )))

;; If indent-tabs-mode is off, nuke whitespace and untabify before saving.  I do this by
;; default now, with modes "opting out" of the practice by setting indent-tabs-mode to
;; non-nil.  (Only enable find-file-hooks hook if you're an aggressive asshole.)

;(add-hook 'find-file-hooks
;  (lambda ()
;    (if (not indent-tabs-mode)
;      (save-excursion
;        (untabify (point-min) (point-max))
;        (if delete-trailing-whitespace-mode
;             (delete-trailing-whitespace))
;        ))))

;; Mechanism to cycle through N tab width settings.  Expected to be mapped to a
;; keybinding.  This is effectively a toggle when the list is of only 2 values.
;; Automatically becomes buffer local; take out make-.. if for some reason you
;; don't want that.

(setq-default tab-widths '(2 4))

(defun cycle-tab-width ()
  "Cycle through tab widths"
  (interactive)
  (make-variable-buffer-local 'tab-widths)
  (nconc tab-widths (list (setq tab-width (pop tab-widths))))
  (setq ruby-indent-level tab-width)
  (message "Tab-width set to %d." tab-width)
  (redraw-display))
