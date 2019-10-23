;;;
;;; Global KeyMappings
;;;

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
(global-set-key (kbd "C-x M-o")     '(lambda nil
                                       (interactive)
                                            (swap-windows)))
(global-set-key (kbd "C-x C-t")     'cycle-tab-width)
(global-set-key (kbd "C-x M-t")     'toggle-untabify-mode)

;; Some settings when we're in a windowed environment.
(when window-system
  ;; Shell out instead of minimize on C-z
  (global-set-key (kbd "C-z") 'shell))

;; Some settings when we're specifically in Carbon Emacs
(when (or (eq window-system 'ns) (eq window-system 'mac))
  (setq mac-command-modifier       'alt
        mac-option-modifier        'meta
        mac-pass-command-to-system t)

  (global-set-key (kbd "A-v")   'yank)
  (global-set-key (kbd "A-c")   'kill-ring-save)

  (global-set-key (kbd "M-RET") 'mac-toggle-max-window)
  (global-set-key (kbd "A-RET") 'mac-toggle-max-window)

  (global-set-key (kbd "A-g")   'grep-find)
  (global-set-key (kbd "s-g")   'grep-find)

  (global-set-key (kbd "A-=")   '(lambda nil (interactive) (screen-zoom 1)))
  (global-set-key (kbd "A-+")   '(lambda nil (interactive) (screen-zoom 1)))
  (global-set-key (kbd "A--")   '(lambda nil (interactive) (screen-zoom -1))))

;; Use the better Buffer browser
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x v n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x v p") 'git-gutter+-previous-hunk)
     ;;; Act on hunks
     ;(define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     ;(define-key git-gutter+-mode-map (kbd "C-x v r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     ;(define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x v c") 'git-gutter+-commit)
     ;(define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     ;(define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     ;(define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)
))

;; Quick way to get to GIT status
;(with-eval-after-load 'magit
;  (define-key vc-prefix-map "s" 'magit-status))
(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v b") 'magit-branch-manager)
(global-set-key (kbd "C-x v l") 'magit-log)
