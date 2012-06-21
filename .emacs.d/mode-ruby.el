
;; ruby-block.el current-block-header hiliting
(when (require 'ruby-block nil 'noerror)
  (ruby-block-mode t)
  (setq ruby-block-highlight-face 'hl-line)
  (setq ruby-block-highlight-toggle 'overlay))

;; Enable Flymake in Ruby
(require 'flymake)

(setq flymake-allowed-file-name-masks (append
    '((".+\\.rb$"   flymake-ruby-init)
      (".+\\.ru$"   flymake-ruby-init)
      (".+\\.rake$" flymake-ruby-init)
      ("Rakefile$"  flymake-ruby-init))
    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns (append
    '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3))
    flymake-err-line-patterns))

; Ensure flymake turds are written to system temp directory
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-ruby")))

(defun flymake-ruby-init ()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-in-system-tempdir))))

;; Rinari RHTML package
(add-to-list 'load-path "~/.emacs.d/lib/rhtml")
(require 'rhtml-mode)
;(add-hook 'rhtml-mode-hook
;    (lambda () (rinari-launch)))

(add-to-list 'load-path "~/.emacs.d/lib/elpa/yaml-mode-0.0.5")
(require 'yaml-mode)

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
    (set (make-variable-buffer-local 'ruby-deep-indent-paren) nil)
    (set (make-variable-buffer-local 'ruby-indent-level)      4)
    (flymake-mode t)
    ))

;; Gank some auto-align stuff from the compuweb.
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
