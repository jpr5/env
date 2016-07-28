;;;
;;; C/C++
;;;

;; Basic settings around parenthesis and how Emacs indents various syntactic
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
    (c-set-offset 'substatement-open 0)))

;;;
;;; CSharp (using Java mode)
;;;

(require 'cc-mode nil 'noerror)
(setq auto-mode-alist (append
      '(("\\.cs$" . java-mode))
    auto-mode-alist))

;;;
;;; JavaScript / HTML
;;;

(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist (append
    '(("\\.js$"    . javascript-mode)
      ("\\.xhtml$" . sgml-mode))
    auto-mode-alist))
(setq sgml-basic-offset 4)

;;;
;;; ActionScript/ECMAScript
;;;

;(autoload 'actionscript-mode "actionscript" nil t)
;(setq auto-mode-alist (append
;    '(("\\.as$"    . actionscript-mode))
;    auto-mode-alist))

(autoload 'ecmascript-mode "ecmascript" nil t)
(setq auto-mode-alist (append
    '(("\\.as$"    . ecmascript-mode))
    auto-mode-alist))

;;;
;;; SGML/XML-related
;;;

(add-hook 'sgml-mode-hook
  (lambda ()
    (set (make-variable-buffer-local 'standard-indent) 2)
))

;;;
;;; Ruby
;;;

;; ruby-block.el current-block-header hiliting
(when (require 'ruby-block nil 'noerror)
  (ruby-block-mode t)
  (setq ruby-block-highlight-face 'hl-line)
  (setq ruby-block-highlight-toggle 'overlay))

;; Enable Flymake in Ruby
(when (require 'flymake nil 'noerror)
  (setq flymake-allowed-file-name-masks (append
      '((".+\\.rb$"   flymake-ruby-init)
        (".+\\.ru$"   flymake-ruby-init)
        (".+\\.rake$" flymake-ruby-init)
        ("Rakefile$"  flymake-ruby-init))
      flymake-allowed-file-name-masks))

  (setq flymake-err-line-patterns (append
      '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3))
      flymake-err-line-patterns))

  ;; Ensure flymake turds are written to system temp directory
  (defun flymake-create-temp-in-system-tempdir (filename prefix)
    (make-temp-file (or prefix "flymake-ruby")))

  (defun flymake-ruby-init ()
    (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                             'flymake-create-temp-in-system-tempdir))))
)

;; Rinari RHTML package
(add-to-list 'load-path "~/.emacs.d/lib/rhtml")
(require 'rhtml-mode)
;(add-hook 'rhtml-mode-hook
;    (lambda () (rinari-launch)))

(add-to-list 'load-path "~/.emacs.d/lib/elpa/yaml-mode-0.0.5")
(require 'yaml-mode)

;; add-to-list symbol element
(setq auto-mode-alist (append
    '(("\\.rb$"         . ruby-mode)
      ("\\.ru$"         . ruby-mode)
      ("\\.ru\\..*$"    . ruby-mode)
      ("\\.rake$"       . ruby-mode)
      ("\\.rb\\.erb$"   . ruby-mode)
      ("\\.erb$"        . rhtml-mode)
      ("\\.html\\.erb$" . rhtml-mode)
      ("\\.rhtml$"      . rhtml-mode)
      ("\\.yml\\..*$"   . yaml-mode)
      ("Rakefile$"      . ruby-mode)
      ("Capfile$"       . ruby-mode)
      ("Gemfile$"       . ruby-mode))
    auto-mode-alist))

(add-hook 'ruby-mode-hook
  (lambda ()
    (set (make-variable-buffer-local 'ruby-deep-indent-paren) nil)
    (set (make-variable-buffer-local 'ruby-indent-level)      4)
    (flymake-mode nil)
    ))

;; Ganked some auto-align stuff from the compuweb.
;;
;; TODO: find a way to "group" '=' and '=>' align targets differently.
(require 'align)

(defconst align-ruby-modes '(ruby-mode)
  "List of modes that will have align-ruby-rules-list applied.")

(defconst align-ruby-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes))
    (ruby-hash-literal
     (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
     (modes . align-ruby-modes)
     (repeat . t))
    (ruby-assignment-literal
     (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
     (modes . align-ruby-modes)
     (repeat . t))
    )
  "Alignment rules specific to ruby-mode. See the variable `align-rules-list' for more details.")

(dolist (it align-ruby-rules-list)
  (add-to-list 'align-rules-list it))

(add-to-list 'align-dq-string-modes    'ruby-mode)
(add-to-list 'align-sq-string-modes    'ruby-mode)
(add-to-list 'align-open-comment-modes 'ruby-mode)

(setq align-indent-before-aligning t)
(setq align-region-separate 'group)

;; Include rdebug-mode if we've got it
(add-to-list 'load-path "~/.emacs.d/lib/rdebug-mode")
(when (require 'rdebug nil 'noerror)
  (setq-default ruby-many-windows nil))

;; Load up cucumber/feature-mode.
(require 'cucumber-mode nil 'noerror)

;;;
;;; LaTex mode
;;;

;; Useful LaTeX keymappings:
;;
;; C-c C-c           toggles between TeX-file and TeX-view based on state
;; C-c : .. C-c ;    TeX-comment-or-uncomment-region
;;
;; C-c C-p C-b       preview-buffer
;; C-c C-p C-d       preview-document
;; C-c C-p C-s       preview-section
;;
;; C-c C-p C-c C-b   preview-clearout-buffer
;; C-c C-p C-c C-d   preview-clearout-document
;; C-c C-p C-c C-s   preview-clearout-section

(require 'tex nil 'noerror)
(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
    (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (setq tex-dvi-view-command "open *")

    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq TeX-command-default "LaTeX")

    (TeX-global-PDF-mode t)
    (flymake-mode)

    (define-key LaTeX-mode-map (kbd "C-c C-o C-p")
      '(lambda nil (interactive)
         (TeX-command "LaTeX" 'TeX-active-master 0)
         (TeX-command "View" 'TeX-active-master 0)))
  ))


;;;
;;; org-mode
;;;

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(add-hook 'org-mode-hook
  (lambda ()
    (turn-on-font-lock)
    (setq indent-tabs-mode nil)
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

;;;
;;; server-mode for Emacs - re-using GUI instance with cmdline invocations
;;;

(defun server-buffer-done-or-kill ()
  "Close a buffer regardless of whether there are active clients waiting on it."
  (interactive)
  (if (null server-clients)
    (kill-buffer nil)
    (server-done)))

(when window-system
  (unless server-mode (server-start))
  (add-hook 'server-switch-hook
    (lambda ()
      (when (current-local-map)
        (use-local-map (copy-keymap (current-local-map))))
      (local-set-key (kbd "C-x k RET") 'server-buffer-done-or-kill))))
