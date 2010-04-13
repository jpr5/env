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

