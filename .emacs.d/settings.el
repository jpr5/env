;; Instead of default-mode being lisp, make it text.  Uncomment the other option to force
;; auto-fill for everything (sometimes not good when programming, though -- depends on how
;; you write codex).

(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Encodings

(set-language-environment    "UTF-8")
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-clipboard-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq require-final-newline   t
      next-line-add-newlines  nil)
(setq x-cut-buffer-max        65536)

(setq-default mouse-yank-at-point    t)
(setq-default completion-ignore-case t)
(setq-default fill-column            80)

;; Don't let the mouse overscroll the window!
(setq mouse-wheel-scroll-amount     '(1))
(setq mouse-wheel-progressive-speed nil)

;; Override standard grep settings with better options.
(setq grep-use-null-device nil)
(setq grep-command       '("grep --color=never -nH -wEe  *" . 29))
(setq grep-find-command  '("find . -type f -exec grep --color=never -nH -wEe   {} /dev/null \\;" . 50))
(setq grep-template      "grep <C> --color=never -nH -wEe <R> <F>")
(setq grep-find-template "find <D> <X> -type f <F> -exec grep --color=never <C> -nH -Ee <R> {} /dev/null \\;")

; Under 23.x, grep-compute-defaults stores the above defaults on a per-host
; basis.  We get back the per-buffer behaviour by forcing
; grep-host-defaults-alist to always be buffer-local.  Also, git grep (and
; vc-git-grep) spews so fast that font-lock doesn't actually properly fontify
; all matches, so re-fontify any buffer using compile.el when it finishes.
(when (eq emacs-major-version 23)
  (make-variable-buffer-local 'grep-host-defaults-alist)
  (setq compilation-finish-functions
        (lambda (buf msg) (with-current-buffer buf (font-lock-fontify-buffer)))))

;; Use ssh for tramp.
(setq tramp-default-method "ssh")

;; Put backup files in /tmp
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-reload buffers if they change out from underneath us (as long as we haven't
;; changed them in emacs), highlight searches as you type them, support loading of
;; compressed files directly, overwrite selections when typed.

(global-auto-revert-mode 1)
(auto-compression-mode   1)
(pending-delete-mode     1)
(file-name-shadow-mode   1)
(icomplete-mode          1)
(iswitchb-mode           1)

;; Show offscreen paren matches in the minibuffer
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point
    ;; is a close parentheses type character. Otherwise, there's not
    ;; really any point, and `blink-matching-open' would just echo
    ;; "Mismatched parentheses", which gets really annoying.
    (if (char-equal (char-syntax (char-before (point))) ?\))
        (setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
        (message matching-text))))

;; Special Display Buffers -- if enabled, whatever buffers match names in this list are
;; created as separate windows (frames), instead of splitting the current buffer.

;(setq special-display-buffer-names
;      '("*Completions*" "*grep*" "*tex-shell*"))

; always paste plain text
(setq yank-excluded-properties t)
