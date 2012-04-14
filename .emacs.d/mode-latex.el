;; Useful LaTeX keymappings:
;
; C-c C-c           toggles between TeX-file and TeX-view based on state
; C-c : .. C-c ;    TeX-comment-or-uncomment-region
;
; C-c C-p C-b       preview-buffer
; C-c C-p C-d       preview-document
; C-c C-p C-s       preview-section
;
; C-c C-p C-c C-b   preview-clearout-buffer
; C-c C-p C-c C-d   preview-clearout-document
; C-c C-p C-c C-s   preview-clearout-section

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
