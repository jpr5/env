;;;
;;; Tab/Whitespace settings
;;;

;; Mechanism to cycle through N tab width settings.  Expected to be mapped to a
;; keybinding.  This is effectively a toggle when the list is of only 2 values.
;; Automatically becomes buffer local; take out make-.. if for some reason you
;; don't want that.

(defun cycle-tab-width ()
  "Cycle through tab widths"
  (interactive)
  (make-variable-buffer-local 'tab-widths)
  (nconc tab-widths (list (setq tab-width (pop tab-widths))))
  (setq ruby-indent-level tab-width)
  (message "Tab-width set to %d." tab-width)
  (redraw-display))

(setq-default tab-width       4)
(setq-default tab-widths '(2 4))

;; Define some magic that will replace tabs if we happen to save a file that has
;; tabs inadvertently added to it, as well as nuke trailing whitespace.  This is
;; the default; (setq untabify-mode nil) in a hook for modes you want left
;; alone.

(setq-default untabify-mode                   t)
(setq-default delete-trailing-whitespace-mode t)

(defun untabify-hook ()
  "Untabify the buffer"
  (when untabify-mode
    (save-excursion
      (untabify (point-min) (point-max))
      (when delete-trailing-whitespace-mode
        (delete-trailing-whitespace))
      )))

;; Untabify on file-save (if untabify-mode is enabled).  And if you're a
;; super-aggressive asshole, take it up a notch and untabify on file-load.

(add-hook 'write-file-hooks 'untabify-hook)
;(add-hook 'find-file-hooks 'untabify-hook)

(defun toggle-untabify-mode ()
  "Toggle untabification"
  (interactive)
  (setq untabify-mode (not untabify-mode))
  (message "Untabification is %s." untabify-mode))
