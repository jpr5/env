;; Global KeyMappings

(global-set-key (kbd "<delete>")    'delete-char)
(global-set-key (kbd "<kp-delete>") 'delete-char)
(global-set-key (kbd "C-h")         'delete-backward-char)
(global-set-key (kbd "C-x C")       'compile)
(global-set-key (kbd "C-c C-c")     'eval-buffer)
(global-set-key (kbd "C-x C-x")     'end-of-buffer)
(global-set-key (kbd "C-x C-p")     'beginning-of-buffer)
(global-set-key (kbd "M-r")         'revert-buffer)
(global-set-key (kbd "M-g")         'goto-line)

;; Possible window systems: 'x 'ns 'mac

(if (eq window-system 'mac) ;; Carbon Emacs, specifically
    (progn

      ;; Some Mac functions, and keymapping.

      (defun screen-zoom (n)
        "with positive N, increase the font size, otherwise decrease it"
        (set-face-attribute 'default (selected-frame) :height
                            (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))
                            ))

      (defun mac-toggle-max-window ()
        (interactive)
        (set-frame-parameter nil 'fullscreen
                             (if (frame-parameter nil 'fullscreen)
                                 nil
                               'fullboth)
                             ))

      (setq mac-command-modifier       'alt
            mac-option-modifier        'meta
            mac-allow-anti-aliasing    t
            mac-pass-command-to-system t)

      (global-set-key (kbd "A-v")   'yank)
      (global-set-key (kbd "A-c")   'kill-ring-save)
      (global-set-key (kbd "M-RET") 'mac-toggle-max-window)
      (global-set-key (kbd "A-=")   '(lambda nil (interactive) (screen-zoom 1)))
      (global-set-key (kbd "A-+")   '(lambda nil (interactive) (screen-zoom 1)))
      (global-set-key (kbd "A--")   '(lambda nil (interactive) (screen-zoom -1)))
      ))

;; Set things when we're in a windowed environment.

(when window-system
    (progn
      ;; Navigate split buffer windows with shift-{left,right,up,down}
      (windmove-default-keybindings)

      ;; Shell out instead of minimize on C-z
      (global-set-key (kbd "C-z") 'shell)
      ))



