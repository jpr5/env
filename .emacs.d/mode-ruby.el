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


;; Rinari package

(setq load-path (add-to-list 'load-path "~/.emacs.d/lib/rinari-rhtml"))
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

;; Include rdebug-mode if we've got it
(setq load-path (add-to-list 'load-path "~/.emacs.d/lib/rdebug-mode"))
(require 'rdebug nil 'noerror)
