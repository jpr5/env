;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jordan Ritter's (jpr5) dot-emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notes:
;;;
;;; C-x r k       - cut rectangle (set-mark first) [remove columns from lines]
;;; C-x r o       - insert rectangle (set-mark first) [add columns to lines]
;;; C-x r y       - yank rectangle (at cursor h-coord)
;;; C-x r i       - string rectangle (replace rect with string; prefix if point at beg-of-line)
;;; C-x n n       - narrow view
;;; C-x n w       - widen (un-narrow) view
;;; C-x RET f     - choose coding system ("unix" == dos2unix)
;;; C-S-backspace - cut whole line at point (without needing to select the line)
;;; M-SPC         - delete all horizontal whitespace around point
;;; M-j           - join current line with next, separated with a space
;;; C-x (         - start recording keyboard macro
;;; C-x )         - stop recording keyboard macro
;;; C-x e         - replay current keyboard macro
;;;
;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x list-faces-display   - show all defined faces and what they look like
;;; M-x color-theme-select   - show (and select from) all known themes in a buffer
;;;
;;; M-x calc                 - RPN calculator
;;;
;;; Manually compile all .el -> .elc:
;;;   find .emacs.d -name *.el | xargs emacs -l ~/.emacs -batch -f batch-byte-compile

;; Identify ourselves
(setq user-full-name    "Jordan Ritter"
      user-mail-address "jpr5@darkridge.com"
      mail-host-address '"darkridge.com")

;; Load our settings/config.  Prefix the load-path with our own first, then loop
;; over all root files as basenames to load (add-to-list uniqifies).
(setq load-path (append '("~/.emacs.d" "~/.emacs.d/lib") load-path))

;; Disable bytecomp for now, while experimenting with newer emacsen.
;(require 'bytecomp)
;(setq byte-compile-verbose nil)
;(setq byte-compile-warnings nil)
;(require 'byte-code-cache)
;(setq bcc-blacklist '("\\.emacs\\.history" "\\.emacs\\.desktop"))
;(setq bcc-cache-directory "~/.emacs.d/elc")

(let (files)
  (dolist (filename (file-expand-wildcards "~/.emacs.d/*.el*") files)
    (when (file-regular-p filename)
      (add-to-list 'files (file-name-sans-extension filename))))
  (dolist (filename files)
    (load-library filename)))

;; And finally Emacs custom settings.
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
 '(flymake-start-syntax-check-on-newline nil)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

(put 'narrow-to-region 'disabled nil)
