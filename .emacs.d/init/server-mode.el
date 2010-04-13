(when window-system
    (progn
      (unless server-mode (server-start))
      (add-hook 'server-switch-hook
        (lambda ()
          (when (current-local-map)
            (use-local-map (copy-keymap (current-local-map))))
          (local-set-key (kbd "C-x k RET") 'server-edit)))
      ))


