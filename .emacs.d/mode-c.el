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

