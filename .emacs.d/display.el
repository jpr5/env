;; Get rid of all those annoying things that get in our way.

(setq inhibit-startup-message t)
(setq emacs-query-startup t)
(setq emacs-base-display t)

;(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Tell Emacs to always syntax-hightlight everything to its fullest extent, and don't wait
;; to fontify (do it immediately).

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 0)

(when window-system ;; Graphics mode
  (setq default-frame-alist '((cursor-type . (bar . 3))))
  (set-face-attribute 'default nil :height 150)

  (let ((frame (selected-frame)))
    (set-frame-position frame 0 0)
    (set-frame-size frame 150 40))

  (blink-cursor-mode 0)
  (when (fboundp 'global-hl-line-mode)
    (global-hl-line-mode t))

  (setq default-indicate-empty-lines t)
  (setq indicate-empty-lines t)

  ;; If you're using OSX emacs-app, this makes 'open -a Emacs file.txt' re-use
  ;; an existing frame.
  (setq ns-pop-up-frames nil))

;; Some good settings: show line and column #, show hilighting when selecting a
;; region, raise the cut buffer size from 20 KiB to 64KB, always add a final
;; newline if there isn't one, don't auto-create newlines when you go past the
;; end of the file, change the yes-or-no prompt to a simple single ``y'' or
;; ``n'' across the board.

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

;; Replace that horrible emacs 23.x visible-bell square with a mode-line-based
;; one instead.  Thanks, Miles.

;(setq visible-bell t)
;(setq ring-bell-function 'ignore)

(defcustom mode-line-bell-string "♪♪♪♪♪♪♪♪♪"
  "Message displayed in mode-line by `mode-line-bell' function."
  :group 'user)
(defcustom mode-line-bell-delay 0.1
  "Number of seconds `mode-line-bell' displays its message."
  :group 'user)

(defvar mode-line-bell-cached-string nil)
(defvar mode-line-bell-propertized-string nil)

(defun mode-line-bell ()
  "Briefly display a highlighted message in the mode-line.

The string displayed is the value of `mode-line-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `mode-line-bell-delay'
seconds.

This function is intended to be used as a value of `ring-bell-function'."
  (unless (equal mode-line-bell-string mode-line-bell-cached-string)
    (setq mode-line-bell-propertized-string
      (propertize
       (concat
        (propertize
         " "
         'display
         `(space :align-to (- right ,(string-width mode-line-bell-string))))
        mode-line-bell-string)
       'face '(:background "red")))
    (setq mode-line-bell-cached-string mode-line-bell-string))
  (message mode-line-bell-propertized-string)
  (sit-for mode-line-bell-delay)
  (message ""))

(setq ring-bell-function 'mode-line-bell)
