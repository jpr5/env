;; Global KeyMappings

(global-set-key (kbd "<delete>")    'delete-char)
(global-set-key (kbd "<kp-delete>") 'delete-char)
(global-set-key (kbd "C-h")         'delete-backward-char)
(global-set-key (kbd "C-x C")       'compile)
(global-set-key (kbd "C-c C-c")     'eval-buffer)
(global-set-key (kbd "C-x C-x")     'end-of-buffer)
(global-set-key (kbd "C-x C-p")     'beginning-of-buffer)
(global-set-key (kbd "C-x r i")     'string-rectangle)
(global-set-key (kbd "M-r")         'revert-buffer)
(global-set-key (kbd "M-g")         'goto-line)
(global-set-key (kbd "C-M-\\")      'indent-region)
(global-set-key (kbd "C-M-/")       'align)
(global-set-key (kbd "M-j")         'join-line)
(global-set-key (kbd "M-SPC")       'fixup-whitespace)
(global-set-key (kbd "C-M-SPC")     'delete-blank-lines)
(global-set-key (kbd "C-x t")       '(lambda nil
                                       (interactive)
                                       (switch-to-buffer-other-window
                                        (generate-new-buffer "*Temp*"))))
(global-set-key (kbd "C-x C-t")     'cycle-tab-width)
(global-set-key (kbd "C-x M-o")     '(lambda nil
                                       (interactive)
                                       (swap-windows)))

;; Some settings when we're in a windowed environment.
(when window-system
  ;; Navigate split buffer windows with shift-{left,right,up,down}
  (windmove-default-keybindings)

  ;; Shell out instead of minimize on C-z
  (global-set-key (kbd "C-z") 'shell))

;; Some settings when we're specifically in Carbon Emacs
(when (or (eq window-system 'ns) (eq window-system 'mac))
  (setq mac-command-modifier       'alt
        mac-option-modifier        'meta
        mac-allow-anti-aliasing    t
        mac-pass-command-to-system t)

  (global-set-key (kbd "A-v")   'yank)
  (global-set-key (kbd "A-c")   'kill-ring-save)

  (global-set-key (kbd "M-RET") 'mac-toggle-max-window)
  (global-set-key (kbd "A-RET") 'mac-toggle-max-window) ; Hrm, not working..

  (global-set-key (kbd "A-g")   'grep-find)

  (global-set-key (kbd "A-=")   '(lambda nil (interactive) (screen-zoom 1)))
  (global-set-key (kbd "A-+")   '(lambda nil (interactive) (screen-zoom 1)))
  (global-set-key (kbd "A--")   '(lambda nil (interactive) (screen-zoom -1)))
)
