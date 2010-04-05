;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jordan Ritter's dot-emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notes:
;;;
;;; C-x r k       - cut rectangle (set-mark first) [remove columns from lines]
;;; C-x r o       - insert rectangle (set-mark first) [add columns to lines]
;;; C-x r y       - yank rectangle (at cursor h-coord)
;;; C-x n n       - narrow view
;;; C-x n w       - widen (un-narrow) view
;;; C-x RET f     - choose coding system ("unix" == dos2unix)
;;; C-S backspace - cut whole line at point (without needing to select the line)
;;;
;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x color-themes-select  - show (and select from) all known themes in a buffer
;;;
;;; Compile all .el -> .elc: emacs -l ~/.emacs -batch -f batch-byte-compile .emacs.d/*.el

;; First get rid of all those annoying things that get in our way.

(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Then set the load path before we get started.

(setq load-path (append
    '("~/.emacs.d" "~/.emacs.d/rinari-rhtml")
    load-path))

;; Set some defaults about ourselves.

(setq user-full-name    "Jordan Ritter"
      user-mail-address "jpr5@darkridge.com"
      mail-host-address '"darkridge.com")

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

;; Mac functions and keymapping

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

;; Possible window systems: 'x 'ns 'mac

(if (eq window-system 'mac) ;; Carbon Emacs, specifically
    (progn
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

;; Encodings

(set-language-environment    "UTF-8")
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-clipboard-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

;; Define themes for all the colors (faces) I like.

(require 'color-theme)

(defun color-theme-jpr5-gui ()
  "jpr5's gui theme"
  (interactive)
  (color-theme-install
    '(color-theme-jpr5-gui
      ((foreground-color . "black") (background-color . "grey75") (cursor-color . "DarkBlue") (background-mode . light))

      (font-lock-warning-face           ((t (:foreground "red" :bold t :italic nil :underline t))))
      (font-lock-comment-face           ((t (:foreground "dark red" :italic t))))
      (font-lock-comment-delimiter-face ((t (:foreground "dark red" :italic t))))

;      (font-lock-string-face            ((t (:foreground "lightyellow1"))))
      (font-lock-string-face            ((t (:foreground "grey35" :background "grey78"))))
      ;(font-lock-variable-name-face     ((t (:foreground "black" :bold t))))
      (font-lock-variable-name-face     ((t (:foreground "slateblue4" :bold t))))
;      (font-lock-type-face              ((t (:foreground "firebrick3" :bold t))))
      (font-lock-type-face              ((t (:foreground "forest green" :bold nil))))
      (font-lock-constant-face          ((t (:foreground "slateblue3" :bold t))))
      (font-lock-builtin-face           ((t (:foreground "dark blue"))))
      (font-lock-keyword-face           ((t (:foreground "dark blue" :bold t))))
      (font-lock-function-name-face     ((t (:foreground "blue" :bold t))))

      (font-lock-doc-face               ((t (:foreground "dark slate grey"))))
      (font-lock-doc-string-face        ((t (:foreground "dark slate grey"))))

      (hl-line                          ((t (:background "grey78"))))
      (mode-line                        ((t (:foreground "white" :background "black"))))
      (region                           ((t (:foreground nil     :background "dark grey"))))
      (show-paren-match-face            ((t (:foreground "white" :background "grey50" :bold t))))
      )))

(defun color-theme-jpr5-tty ()
  "jpr5's tty theme"
  (interactive)
  (color-theme-install
    '(color-theme-jpr5-tty
      ((foreground-color . "white") (background-color . "black") (background-mode . dark))

      (font-lock-comment-face           ((t (:foreground "dark red"))))
      (font-lock-comment-delimiter-face ((t (:foreground "dark red"))))
      (font-lock-string-face            ((t (:foreground "light grey"))))
      (font-lock-warning-face           ((t (:foreground "black" :background "yello"))))
      (region                           ((t (:foreground nil     :background "dark grey"))))
      )))

;; Set things differently based on windowing environment.

(if window-system
    (progn ;; Graphics mode
      (setq default-frame-alist '((cursor-type . (bar . 3))))
      (setq initial-frame-alist (x-parse-geometry "178x45+5+30")) ;; cols x rows (character) +x+y (pixel)
      (modify-frame-parameters (selected-frame) initial-frame-alist) ;; initial window size
      ;(set-frame-parameter nil 'alpha 94) ;; transparency %

      (blink-cursor-mode 0)
      (when (fboundp 'global-hl-line-mode)
        (global-hl-line-mode t))

      (global-set-key (kbd "C-z") 'shell)

      (setq default-indicate-empty-lines t)
      (setq indicate-empty-lines t)

      (color-theme-jpr5-gui)

      ;; If in X, startup server and make it work like regular buffer.
      (unless server-mode (server-start))
      (add-hook 'server-switch-hook
        (lambda ()
          (when (current-local-map)
            (use-local-map (copy-keymap (current-local-map))))
          (local-set-key (kbd "C-x k RET") 'server-edit)))
      )
    (progn ;; Console mode
      (setq default-frame-alist '((cursor-type . box)))

      (blink-cursor-mode t)

      (color-theme-jpr5-tty)
      ))

;; Now tell Emacs to always syntax-hightlight everything to its fullest extent, and don't
;; wait to fontify (do it immediately).

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 0)

;; Some good settings: visible bell instead of beep, show line and column #, show
;; hilighting when selecting a region, raise the cut buffer size from 20 KiB to 64KB,
;; always add a final newline if there isn't one, don't auto-create newlines when you go
;; past the end of the file, change the yes-or-no prompt to a simple single ``y'' or ``n''
;; across the board.

(setq visible-bell            t)
(setq resize-mini-windows     t)
(setq line-number-mode        t
      column-number-mode      t
      size-indication-mode    t
      transient-mark-mode     t)
(setq require-final-newline   t
      next-line-add-newlines  nil)
(setq search-highlight        t
      query-replace-highlight t)
(setq x-cut-buffer-max        65536)

(setq-default even-window-heights    nil)
(setq-default resize-mini-windows    nil)
(setq-default mouse-yank-at-point    t)
(setq-default completion-ignore-case t)
(setq-default fill-column            90)

;; Override standard grep mechanism and use tramp for ssh.
(setq grep-command         "grep -rw -nH ")
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

;; Navigate split buffer windows with shift-{left,right,up,down}
(windmove-default-keybindings)

;; Set buffer name collision (duplicate) styling
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save whatever files you had open at your last open (~/.emacs.desktop), and don't
;; open/save it if already locked (might have to clean out stale lock files on your own).
(when (require 'desktop nil 'noerror)
  (setq desktop-dirname (expand-file-name "~"))
  (setq desktop-path    (list desktop-dirname))
  (setq desktop-load-locked-desktop 'nil)
  (unless (file-exists-p (expand-file-name (desktop-full-lock-name)))
    (desktop-save-mode 1)))

;; Save your minibuffer history
(setq savehist-file                 (expand-file-name "~/.emacs.history")
      savehist-additional-variables '(search ring regexp-search-ring))
(savehist-mode 1)

;; Handle escape sequences when shelling out (C-z)
(when (require 'ansi-color nil 'noerror)
  (ansi-color-for-comint-mode-on)
  (setq comint-prompt-read-only t))

;; Browse your kill ring buffer
(when (require 'browse-kill-ring nil 'noerror)
  (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; Better buffer browser
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; TAB settings
;;
;; Set some basic requirements around input (width 4, don't use tabs), but then also
;; define some magic that will replace tabs if we happen to save a file that has tabs
;; inadvertently added to it, as well as nuke trailing whitespace.  This is the default
;; behaviour; (setq indent-tabs-mode t) in a hook for modes you want left alone.

(setq-default tab-width        4)   ;; used by untabify
(setq-default indent-tabs-mode nil)

;; If indent-tabs-mode is off, nuke whitespace and untabify before saving.  I do this by
;; default now, with modes "opting out" of the practice by setting indent-tabs-mode to
;; non-nil.  (Only enable find-file-hooks hook if you're an aggressive asshole.)

;(add-hook 'find-file-hooks
;  (lambda ()
;    (if (not indent-tabs-mode)
;      (save-excursion
;        (untabify (point-min) (point-max))
;        (delete-trailing-whitespace)
;        ))))

(add-hook 'write-file-hooks
  (lambda ()
    (if (not indent-tabs-mode)
      (save-excursion
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        ))))

;; Instead of default-mode being lisp, make it text.  Uncomment the other option to force
;; auto-fill for everything (sometimes not good when programming, though -- depends on how
;; you write codex).

(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Basic C/C++ settings around parenthesis and how Emacs indents various syntactic
;; structures.  Also, highlight certain keywords differently.

(setq c-basic-indent 4)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode t)

(defun c-style-comments ()
    (setq comment-start "/* "
          comment-end   " */"))

(add-hook 'c-mode-hook   'c-style-comments)
(add-hook 'c++-mode-hook 'c-style-comments)

(add-hook 'c-mode-common-hook
    (lambda ()
      (setq c-basic-offset 4)
      (c-set-offset 'case-label '+)
      (c-set-offset 'substatement-open 0)
      ))

;; JavaScript / HTML stuff

(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist (append
    '(("\\.js$"    . javascript-mode)
      ("\\.xhtml$" . sgml-mode))
    auto-mode-alist))
(setq sgml-basic-offset 4)

;; ActionScript/ECMAScript

;(autoload 'actionscript-mode "actionscript" nil t)
;(setq auto-mode-alist (append
;    '(("\\.as$"    . actionscript-mode))
;    auto-mode-alist))

(autoload 'ecmascript-mode "ecmascript" nil t)
(setq auto-mode-alist (append
    '(("\\.as$"    . ecmascript-mode))
    auto-mode-alist))

;; Ruby settings

(require 'ruby-mode)
(require 'rhtml-mode)

; add-to-list symbol element
(setq auto-mode-alist (append
    '(("\\.rb$"         . ruby-mode)
      ("\\.ru$"         . ruby-mode)
      ("\\.ru\\..*$"    . ruby-mode)
      ("Rakefile$"      . ruby-mode)
      ("\\.rake$"       . ruby-mode)
      ("\\.rb\\.erb$"   . ruby-mode)
      ("\\.erb$"        . rhtml-mode)
      ("\\.html\\.erb$" . rhtml-mode)
      ("\\.rhtml$"      . rhtml-mode)
      ("\\.yml\\..*$"   . yaml-mode))
    auto-mode-alist))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-deep-indent-paren nil)
    (setq ruby-indent-level 4)

    ; Only launch flymake when we could actually write to the temporary file.
    (if (and (not (null buffer-file-name))
             (file-writable-p (concat (file-name-sans-extension buffer-file-name) "_flymake"
                                      (and (file-name-extension buffer-file-name) (concat "." (file-name-extension buffer-file-name))))))
        (flymake-mode))
    ))

;; Flymake for ruby
(require 'flymake)

(defun flymake-ruby-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
           (list "ruby" (list "-c" local-file))))

(setq flymake-allowed-file-name-masks (append
    '((".+\\.rb$"   flymake-ruby-init)
      (".+\\.ru$"   flymake-ruby-init)
      (".+\\.rake$" flymake-ruby-init)
      ("Rakefile$"  flymake-ruby-init))
    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns (append
    '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3))
    flymake-err-line-patterns))

;; Git stuff - load vc-git and "fix" the broken blame invocation syntax.
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat rev)))))

;; Enable this for the the ever-awesome git-blame-mode (kinda broken)
;(require 'git)
;(require 'format-spec)
;(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
;(global-set-key (kbd "C-x v b") 'git-blame-mode) ;; FIXME: doesn't work

(setq diff-switches '"-u")
;(setq vc-diff-switches '"-u")
;(setq cvs-diff-flags '("-u"))
;(setq vc-cvs-diff-switches '"-u")
;(setenv "PATH" (concat "/usr/local/git/bin:" (getenv "PATH")))

;; Some useful LaTeX keybindings/settings.

(add-hook 'latex-mode-hook
  (lambda ()
    (setq tex-dvi-view-command "xdvi *")
    (setq tex-output-extension ".dvi")
    (setq tex-default-mode 'latex-mode)
    (define-key latex-mode-map (kbd "C-c C-c") 'tex-file)
    (define-key latex-mode-map (kbd "C-c C-v") 'tex-view)
  ))

;; Mark certain keywords with warning faces
(defun enable-warning-keyword-hiliting (modes)
    "Add hiliting of certain keywords to given modes."
    (dolist (mode modes)
      (font-lock-add-keywords mode
        '(("\\<\\(FIXME\\|WARNING\\|NOTE\\|TODO\\|TBC\\|TBD\\):" 1
           font-lock-warning-face t))
      )))

(enable-warning-keyword-hiliting '(c-mode c++-mode perl-mode ruby-mode))

;; Special Display Buffers -- if enabled, whatever buffers match names in this list are
;; created as separate windows (frames), instead of splitting the current buffer.

;(setq special-display-buffer-names
;      '("*Completions*" "*grep*" "*tex-shell*"))

(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (when (forward-word 1)
          (setq n (1+ n))))
      (message "Region has %d words" n)
      n)))

(defun emacs-format-function ()
   "Untabify the whole buffer."
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   (save-some-buffers t)
)

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

;; And finally Emacs custom settings.
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(flymake-gui-warnings-enabled nil)
 '(flymake-mode nil t)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-start-syntax-check-on-newline nil))

