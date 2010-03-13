;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jordan Ritter's dot-emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:
;;
;; dos2unix:
;;     - http://www.gnu.org/software/emacs/manual/html_node/emacs/Coding-Systems.html
;;     - http://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/
;; Basically: C-x RET f, then enter "unix".  C-x C-s (save), and voila.

;;; Notes:
;;;
;;; Indent-Rigidly: set-mark, move cursor, C-x tab
;;; Indent-Region:  set-mark, move cursor, C-M-\
;;;
;;; C-x r k    - cut rectangle (set-mark first) [remove rigid space from lines]
;;; C-x r o    - insert rectangle (set-mark first) [add rigid space to lines]
;;; C-x r y    - yank rectangle (at cursor h-coord)
;;; C-x n n    - narrow view
;;; C-x n w    - widen (un-narrow) view
;;;
;;; Compile all .el -> .elc: emacs -l ~/.emacs -batch -f batch-byte-compile .emacs.d/*.el

;; Get rid of all those annoying things that get in our way.
;;

(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Before we start setting colors, we have to tell Emacs to always
;; syntax-hightlight everything to its fullest extent, and don't wait
;; to fontify (do it immediately).
;;

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 0)

;; Basic Color Settings.  These define entire faces themselves, but
;; we'll override individual settings next.  Below are some color
;; combinations I liked.. note that the following font-lock*face
;; definitions are tuned to the current fg/bg color combination, and
;; so you may want to disable all the face definitions if you change
;; the overriding fg/bg colors.
;;

;; Window systems: 'x 'ns 'mac
(if (eq window-system 'mac)
    (progn
      (setq mac-command-modifier       'alt
            mac-option-modifier        'meta
            mac-allow-anti-aliasing    t
            mac-pass-command-to-system t)
      (global-set-key [?\A-v] 'yank)
      (global-set-key [?\A-c] 'kill-ring-save)
      ;(set-frame-parameter nil 'alpha 94) ;; transparency - HOW YOU LIKE ME NOW
    )
)

;; Set things different based on windowing environment.
(if (null window-system) ;; Console mode
    (progn
      (setq default-frame-alist '((cursor-type . box)))

      (blink-cursor-mode t)

      (set-foreground-color                        "white")
      (set-background-color                        "black")
      (set-cursor-color                            "grey")
      (set-face-foreground 'font-lock-warning-face "black")
      (set-face-background 'font-lock-warning-face "yellow")
      (set-face-foreground 'font-lock-string-face  "light grey")
      (set-face-background 'region                 "blue")
      (set-face-background 'highlight              "dark blue")
    )
    (progn ;; Graphics mode

      ;; Set initial size
      (setq initial-frame-alist (x-parse-geometry "165x40+5+30")) ;cols x rows (character) +x+y (pixel)
      (setq default-frame-alist '((cursor-type . (bar . 3))))
      (setq default-indicate-empty-lines t)
      (setq indicate-empty-lines t)

      (blink-cursor-mode 0)
      (modify-frame-parameters (selected-frame) initial-frame-alist)
      (global-set-key "\C-z" 'shell)

      (set-foreground-color                        "black")
      (set-background-color                        "grey75")       ;; easy on eyes
;     (set-background-color                        "white smoke")  ;; high contrast
      (set-cursor-color                            "DarkBlue")
      (set-border-color                            "DarkSlateGray")
      (set-face-background 'font-lock-warning-face "yellow")
      (set-face-foreground 'font-lock-string-face  "grey35")
      (set-face-background 'region                 "dark grey")
      (set-face-background 'highlight              "blue")

      (global-hl-line-mode t)

      ;; If in X, startup server and make it work like regular buffer.
      (unless server-mode (server-start))
      (add-hook 'server-switch-hook
        (lambda ()
          (when (current-local-map)
            (use-local-map (copy-keymap (current-local-map))))
          (local-set-key (kbd "C-x k RET") 'server-edit)))
    )
)

;; Explicitly set colors (faces) of individual objects.  If you like
;; the colors the schemes above give you, then just comment out this
;; entire section.
;;

(set-face-foreground 'font-lock-comment-face       "dark red")
(set-face-foreground 'font-lock-doc-face           "dark slate grey")
(set-face-foreground 'font-lock-type-face          "forest green")
(set-face-foreground 'font-lock-variable-name-face "dark green")
(set-face-foreground 'font-lock-function-name-face "dark blue")
(set-face-foreground 'font-lock-keyword-face       "blue")
(set-face-foreground 'font-lock-builtin-face       "dark blue")
(set-face-foreground 'font-lock-constant-face      "slateblue3")

(set-face-foreground 'modeline                     "white")
(set-face-background 'modeline                     "black")

;; Some defaults about ourselves.
;;

(setq user-full-name    "Jordan Ritter"
      user-mail-address "jpr5@darkridge.com"
      mail-host-address '"darkridge.com")

;; Global KeyMappings
;;

(global-set-key [delete]      'delete-char)
(global-set-key [kp-delete]   'delete-char)
(global-set-key "\C-h"        'delete-backward-char)
(global-set-key "\C-xC"       'compile)
(global-set-key "\C-c\C-c"    'eval-buffer)
(global-set-key "\C-x\C-x"    'end-of-buffer)
(global-set-key "\C-x\C-p"    'beginning-of-buffer)
(global-set-key "\M-r"        'revert-buffer)
(global-set-key "\M-g"        'goto-line)

;; Zoom functions and keymapping

(defun screen-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

(global-set-key [?\A-=] '(lambda nil (interactive) (screen-zoom 1)))
(global-set-key [?\A-+] '(lambda nil (interactive) (screen-zoom 1)))
(global-set-key [?\A--] '(lambda nil (interactive) (screen-zoom -1)))

;; Encodings
;;

;(set-language-environment  "English")
(set-language-environment    "UTF-8")
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-clipboard-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

;; Some good settings: visible bell instead of beep, show line and column #, show
;; hilighting when selecting a region, raise the cut buffer size from 20 KiB to 64KB,
;; always add a final newline if there isn't one, don't auto-create newlines when you go
;; past the end of the file, change the yes-or-no prompt to a simple single ``y'' or
;; ``n'' across the board.
;;

(setq visible-bell            t)
(setq line-number-mode        t
      column-number-mode      t
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

; Override standard grep mechanism and use tramp for ssh.
(setq grep-command         "grep --color=never -rw -nH ")
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

; Override standard grep mechanism.
(setq grep-command "grep --color=never -rw -nH ")

;; Save your minibuffer history
(setq savehist-file (expand-file-name "~/.emacs.history"))
(savehist-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Save whatever files you had open at your last open (~/.emacs.desktop)
(require 'desktop)
(setq desktop-dirname (expand-file-name "~"))
(setq desktop-path    (list desktop-dirname))
(setq desktop-load-locked-desktop 'nil)
(unless (file-exists-p (expand-file-name (desktop-full-lock-name)))
    (desktop-save-mode 1))

;; Navigate split buffer windows with shift-{left,right,up,down}
(windmove-default-keybindings)

;; TAB settings
;;
;; Set some basic requirements around input (width 4, don't use tabs),
;; but then also define some magic that will replace tabs if we happen
;; to save a file that has tabs inadvertently added to it, as well as
;; nuke trailing whitespace.  This is the default behaviour; (setq
;; indent-tabs-mode t) in a hook for modes you want left alone.

(setq-default tab-width        4) ;; used by untabify
(setq-default indent-tabs-mode nil)

;; If indent-tabs-mode is off, nuke whitespace and untabify before
;; saving.  I do this by default now, with modes "opting out" of the
;; practice by setting indent-tabs-mode to non-nil.

(add-hook 'find-file-hooks
  (lambda ()
    (if (not indent-tabs-mode)
      (save-excursion
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        ))))

(add-hook 'write-file-hooks
  (lambda ()
    (if (not indent-tabs-mode)
      (save-excursion
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        ))))

;; Instead of default-mode being lisp, make it text.  Uncomment the
;; other option to force auto-fill for everything (sometimes not good
;; when programming, though -- depends on how you write codex).
;;

(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Basic C/C++ settings around parenthesis and how Emacs indents
;; various syntactic structures.  Also, highlight certain keywords
;; differently.
;;

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
    )
)

;; JavaScript / HTML stuff
;;

(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist (append
    '(("\\.js$"    . javascript-mode)
      ("\\.xhtml$" . sgml-mode))
    auto-mode-alist))
(setq sgml-basic-offset 4)

;; ActionScript/ECMAScript

;;(autoload 'actionscript-mode "actionscript" nil t)
;;(setq auto-mode-alist (append
;;    '(("\\.as$"    . actionscript-mode))
;;    auto-mode-alist))

(autoload 'ecmascript-mode "ecmascript" nil t)
(setq auto-mode-alist (append
    '(("\\.as$"    . ecmascript-mode))
    auto-mode-alist))

;; Flymake settings
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

;; Ruby settings
;;

(setq load-path (append
    '("~/.emacs.d" "~/.emacs.d/rinari-rhtml")
    load-path))

(require 'ruby-mode)
(require 'rhtml-mode)

; add-to-list symbol element
(setq auto-mode-alist (append
    '(("\\.rb$"         . ruby-mode)
      ("\\.ru$"         . ruby-mode)
      ("Rakefile$"      . ruby-mode)
      ("\\.rake$"       . ruby-mode)
      ("\\.rhtml$"      . rhtml-mode)
      ("\\.rb\\.erb$"   . ruby-mode)
      ("\\.html\\.erb$" . rhtml-mode)
      ("\\.erb$"        . rhtml-mode)
      ("\\.yml\\..*$"   . yaml-mode))
    auto-mode-alist))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-deep-indent-paren nil)
    (setq ruby-indent-level 4)
    (setq fill-column 90)

    ; Only launch flymake when we could actually write to the temporary file.
    (if (and (not (null buffer-file-name))
             (file-writable-p (concat (file-name-sans-extension buffer-file-name) "_flymake"
                                      (and (file-name-extension buffer-file-name) (concat "." (file-name-extension buffer-file-name))))))
        (flymake-mode))
    ))

;; Git stuff
;;
;;(require 'egg)
;;(require 'magit)
;;(setq exec-path (append '("/usr/local/git/bin") exec-path))

(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(require 'format-spec)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
(global-set-key "\C-x v b" 'git-blame-mode)

;; Mark certain keywords with warning faces
;;

(defun enable-warning-keyword-hiliting (modes)
    "Add hiliting of certain keywords to given modes."
    (dolist (mode modes)
      (font-lock-add-keywords mode
        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
          ("\\<\\(WARNING\\):" 1 font-lock-warning-face t)
          ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
          ("\\<\\(IMPORTANT\\):" 1 font-lock-warning-face t)
          ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
          ("\\<\\(TBC\\)" 1 font-lock-warning-face t)
          ("\\<\\(TBD\\)" 1 font-lock-warning-face t)))
      ))

(enable-warning-keyword-hiliting '(c-mode c++-mode perl-mode ruby-mode))

;;
;; Some cool settings for making VCS integration better.
;;

(setq diff-switches '"-u")
;;(setq vc-diff-switches '"-u")
;;(setq cvs-diff-flags '("-u"))
;;(setq vc-cvs-diff-switches '"-u")

;(setenv "PATH" (concat "/usr/local/git/bin:" (getenv "PATH")))

;;
;; Some useful LaTeX keybindings/settings.
;;

(add-hook 'latex-mode-hook
  (lambda ()
    (setq tex-dvi-view-command "xdvi *")
    (setq tex-output-extension ".dvi")
    (setq tex-default-mode 'latex-mode)
    (define-key latex-mode-map "\C-c\C-c" 'tex-file)
    (define-key latex-mode-map "\C-c\C-v" 'tex-view)
  ))

;; Special Display Buffers -- if enabled, whatever buffers match names
;; in this list are created as separate windows (frames), instead of
;; splitting the current buffer.
;;

;(setq special-display-buffer-names
;      '("*Completions*" "*grep*" "*tex-shell*"))


;; Util

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
   "Format the whole buffer."
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   (save-some-buffers t)
)

;; And finally Emacs custom settings.
;;

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

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "lightgrey")))))
