;; Set buffer name collision (duplicate) styling
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save whatever files you had open at your last open (~/.emacs.desktop), and
;; don't open/save it if already locked (might have to clean out stale lock
;; files on your own).
(when (require 'desktop nil 'noerror)
  (setq desktop-dirname (expand-file-name "~"))
  (setq desktop-path    (list desktop-dirname))
  (setq desktop-load-locked-desktop 'nil)
  (unless (file-exists-p (expand-file-name (desktop-full-lock-name)))
    (desktop-save-mode 1)))

;; Browse your kill ring buffer
(when (require 'browse-kill-ring nil 'noerror)
  (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; Navigate split buffer windows with shift-{left,right,up,down}
;; DISABLED: I never use, but it might be useful to someone else..
;(when window-system
;  (windmove-default-keybindings))

;; DISABLED: Weirdness with linum not being added to features when elc'd.
;(when (or (require 'linum nil 'noerror) (functionp 'global-linum-mode))
;  (global-linum-mode))

(setq package-user-dir (expand-file-name "~/.emacs.d/lib/elpa"))

(add-to-list 'load-path package-user-dir)
(when (require 'package nil 'noerror)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
