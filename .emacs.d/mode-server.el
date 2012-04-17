(defun server-buffer-done-or-kill ()
  "Close a buffer regardless of whether there are active clients waiting on it."
  (interactive)
  (if (null server-clients)
      (kill-buffer nil)
    (server-done)
    ))

(when window-system
  (unless server-mode (server-start))
  (add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
              (local-set-key (kbd "C-x k RET") 'server-buffer-done-or-kill))))
