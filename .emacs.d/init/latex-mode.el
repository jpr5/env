;; Some useful LaTeX keybindings/settings.

(add-hook 'latex-mode-hook
  (lambda ()
    (setq tex-dvi-view-command "xdvi *")
    (setq tex-output-extension ".dvi")
    (setq tex-default-mode 'latex-mode)
    (define-key latex-mode-map (kbd "C-c C-c") 'tex-file)
    (define-key latex-mode-map (kbd "C-c C-v") 'tex-view)
  ))

