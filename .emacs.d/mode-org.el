(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(add-hook 'org-mode-hook
    (lambda ()
        (turn-on-font-lock)
        (set (make-local-variable 'line-move-visual) 'nil)
        (local-set-key (kbd "<return>") 'org-return)
        (local-set-key (kbd "C-<return>") 'org-return)
        (local-set-key (kbd "M-<return>") 'mac-toggle-max-window)
        (local-set-key (kbd "M-n") 'outline-next-visible-heading)
        (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
))

(setq org-log-done 'time)
(setq org-deadline-warning-days 10)
(setq org-special-ctrl-a/e 'reversed)
;(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

(defadvice pabbrev-global-mode (around org-stop-pabbrev activate)
  (unless (eq major-mode 'org-mode)
    ad-do-it))

(setq org-todo-keywords
      '((sequence "TODO" "DONE")
        (sequence "DEFER" "TODO" "|" "DONE")
        (sequence "TASK" "|" "DONE")))
