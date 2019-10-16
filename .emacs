;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jordan Ritter's (jpr5) dot-emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Useful Shortcuts:
;;
;; C-x r k       - cut rectangle (set-mark first) [remove columns from lines]
;; C-x r o       - insert rectangle (set-mark first) [add columns to lines]
;; C-x r y       - yank rectangle (at cursor h-coord)
;; C-x r i       - string rectangle (replace region-rect with string; will prefix if point @ beg-of-line)
;; C-x n n       - narrow view
;; C-x n w       - widen (un-narrow) view
;; C-x RET f     - choose coding system ("unix" == dos2unix)
;; C-S-backspace - cut whole line at point (without needing to select the line)
;; M-SPC         - delete all horizontal whitespace around point
;; M-j           - join current line with next, separated with a space
;; C-x (         - start recording keyboard macro
;; C-x )         - stop recording keyboard macro
;; C-x e         - replay current keyboard macro
;; C-x 4 c       - clone current buffer
;;
;; M-x list-colors-display  - show what all the colors look like in a buffer
;; M-x list-faces-display   - show all defined faces and what they look like
;; M-x color-theme-select   - show (and select from) all known themes in a buffer
;;
;; M-x calc                 - RPN calculator
;;
;; Manually compile all .el -> .elc:
;;   find .emacs.d -name *.el | xargs emacs -l ~/.emacs -batch -f batch-byte-compile
;;
;; Benchmark lisp function calls: M-x elp-instrument-package RET <pkg> RET
;;   View results: M-x elp-results RET

;; Identify ourselves

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name    "Jordan Ritter"
      user-mail-address "j@darkrid.ge"
      mail-host-address '"darkridge.com")

;; Load our settings/config.  First disable Emacs' initialization warning about
;; .emacs.d in the load path - yeah, whatever.  Then prefix the load-path with
;; our own, and loop over all root files as basenames to load (add-to-list
;; uniqifies).
(defadvice display-warning
  (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    ad-do-it))

(setq load-path (append '("~/.emacs.d" "~/.emacs.d/lib") load-path))

;; Autoload our other config files.  themes will load display for us.
(dolist (filename '(util settings keys themes tabs vcs modes addons))
  (load-library (format "~/.emacs.d/%s.el" filename)))

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
 '(flymake-start-on-flymake-mode t)
 '(flymake-start-syntax-check-on-newline nil)
 '(package-selected-packages
   (quote
    (flymake-php php-mode yaml-mode swift-mode pkg-info magit let-alist go-dlv git-gutter+ flymake-go dash coffee-mode)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t nil)))
 '(magit-log-sha1 ((t (:foreground "SteelBlue1")))))

;; Totally useless but neat.
(defconst animate-n-steps 7)
(defun lets-play ()
  (animate-string (concat ";; Init successful, welcome to "
            (substring (emacs-version) 0 14)
            ".  Shall we play a game?") 0 0)
  (newline-and-indent) (newline-and-indent)
  (sit-for 0.2 nil))
(add-hook 'after-init-hook 'lets-play)
