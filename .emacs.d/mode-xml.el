;; SGML/XML-related options

(add-hook 'sgml-mode-hook
    (lambda ()
        (set (make-variable-buffer-local 'standard-indent) 2)
))
