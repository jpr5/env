;; Get rid of all those annoying things that get in our way.

(setq inhibit-startup-message t)
(setq emacs-query-startup t)
(setq emacs-base-display t)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Tell Emacs to always syntax-hightlight everything to its fullest extent, and don't wait
;; to fontify (do it immediately).

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 0)

(if window-system
    (progn ;; Graphics mode
      (setq default-frame-alist '((cursor-type . (bar . 3))))
      (setq initial-frame-alist (x-parse-geometry "178x45+5+30")) ;; cols x rows (character) +x+y (pixel)
      (modify-frame-parameters (selected-frame) initial-frame-alist) ;; initial window size

      (blink-cursor-mode 0)
      (when (fboundp 'global-hl-line-mode)
        (global-hl-line-mode t))

      (setq default-indicate-empty-lines t)
      (setq indicate-empty-lines t)
      ))

;; Some good settings: visible bell instead of beep, show line and column #, show
;; hilighting when selecting a region, raise the cut buffer size from 20 KiB to 64KB,
;; always add a final newline if there isn't one, don't auto-create newlines when you go
;; past the end of the file, change the yes-or-no prompt to a simple single ``y'' or ``n''
;; across the board.

(setq visible-bell            t)
(setq resize-mini-windows     t)
(setq line-number-mode        t
      column-number-mode      t
      size-indication-mode    t
      transient-mark-mode     t)
(setq search-highlight        t
      query-replace-highlight t)

(setq-default even-window-heights nil)
(setq-default resize-mini-windows nil)

;; Handle escape sequences when shelling out (C-z)
(when (require 'ansi-color nil 'noerror)
  (ansi-color-for-comint-mode-on)
  (setq comint-prompt-read-only t))

;; Mark certain keywords with warning faces
(defun enable-warning-keyword-hiliting (modes)
    "Add hiliting of certain keywords to given modes."
    (dolist (mode modes)
      (font-lock-add-keywords mode
        '(("\\<\\(FIXME\\|WARNING\\|NOTE\\|TODO\\|TBC\\|TBD\\)[: ]" 1
           font-lock-warning-face t))
      )))

(enable-warning-keyword-hiliting '(c-mode c++-mode perl-mode ruby-mode))
