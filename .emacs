;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jordan Ritter's dot-emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Identify ourselves
(setq user-full-name    "Jordan Ritter"
      user-mail-address "jpr5@darkridge.com"
      mail-host-address '"darkridge.com")

;; Load our settings/config
(setq load-path (append '("~/.emacs.d" "~/.emacs.d/lib") load-path))
(mapcar 'load (file-expand-wildcards "~/.emacs.d/*.el"))

;; Set the theme
(if window-system
    (color-theme-jpr5-gui)
    (color-theme-jpr5-tty)
  )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(flymake-gui-warnings-enabled nil)
 '(flymake-mode nil t)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-start-syntax-check-on-newline nil))

;; And finally Emacs custom settings.
(put 'narrow-to-region 'disabled nil)

;;; Notes:
;;;
;;; C-x r k       - cut rectangle (set-mark first) [remove columns from lines]
;;; C-x r o       - insert rectangle (set-mark first) [add columns to lines]
;;; C-x r y       - yank rectangle (at cursor h-coord)
;;; C-x n n       - narrow view
;;; C-x n w       - widen (un-narrow) view
;;; C-x RET f     - choose coding system ("unix" == dos2unix)
;;; C-S backspace - cut whole line at point (without needing to select the line)
;;;
;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x color-themes-select  - show (and select from) all known themes in a buffer
;;;
;;; Compile all .el -> .elc: emacs -l ~/.emacs -batch -f batch-byte-compile .emacs.d/*.el
