;(setq load-path (add-to-list 'load-path "~/.emacs.d/lib/org-mode"))
;(require 'org-install)
;(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-log-done 'time)
(setq org-deadline-warning-days 10)
;(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

(defadvice pabbrev-global-mode (around org-stop-pabbrev activate)
  (unless (eq major-mode 'org-mode)
    ad-do-it))

(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only

;(setq org-todo-keywords
;      '(
;        "WAIT"
;        "TODO"
;        "DELEGATED"
;        "DONE"
;        )
;      org-todo-interpretation 'type)
