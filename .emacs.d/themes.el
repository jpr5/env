(require 'color-theme)

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

(defun color-theme-jpr5-day ()
  "jpr5's daytime theme"
  (interactive)
  (color-theme-install
    '(color-theme-jpr5-day
      ((foreground-color . "black") (background-color . "grey75") (cursor-color . "DarkBlue") (background-mode . light))

      (font-lock-warning-face           ((t (:foreground "red" :bold t :italic nil :underline t))))
      (font-lock-comment-face           ((t (:foreground "dark red" :italic t))))
      (font-lock-comment-delimiter-face ((t (:foreground "dark red" :italic t))))

      (font-lock-string-face            ((t (:foreground "grey35" :background "grey78"))))
      (font-lock-variable-name-face     ((t (:foreground "slateblue4" :bold t))))
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

(defun color-theme-jpr5-night ()
  "jpr5's night time theme"
  (interactive)
  (color-theme-install
   '(color-theme-jpr5-night
     ((font   . "fixed")
      (width  . 130)
      (height . 50)
      (background-color . "black")
      (foreground-color . "grey85")
      (background-mode  . dark)
      (mouse-color      . "grey85")
      (cursor-color     . "grey85"))
     (default ((t (nil))))
     (font-lock-comment-face           ((t (:italic t :foreground "tomato4"))))
     (font-lock-comment-delimiter-face ((t (:foreground "dark red" :italic t))))
     (font-lock-string-face            ((t (:foreground "RosyBrown3" :background "gray10"))))
     (font-lock-keyword-face           ((t (:foreground "DodgerBlue1"))))
     (font-lock-constant-face          ((t (:foreground "gold2"))))
     (font-lock-type-face              ((t (:foreground "DarkCyan"))))
     (font-lock-variable-name-face     ((t (:foreground "SkyBlue1"))))
     (font-lock-function-name-face     ((t (:foreground "SlateBlue"))))
     (font-lock-builtin-face           ((t (:foreground "SkyBlue"))))
     (font-lock-warning-face           ((t (:foreground "red" :bold t :italic nil :underline t))))
     (hl-line                          ((t (:background "grey10"))))
     (region                           ((t (:background "grey15"))))
     (highlight                        ((t (:background "blue"))))
     (secondary-selection              ((t (:background "navy"))))
     (highline-face                    ((t (:background "grey10"))))
     (setnu-line-number-face           ((t (:background "grey15" :foreground "white" :bold t))))
     (show-paren-match-face            ((t (:background "grey30"))))
     (widget-field-face                ((t (:background "navy"))))
     (widget-single-line-field-face    ((t (:background "royalblue"))))
)))

(defun color-theme-jpr5-night-old ()
  "jpr5's night time theme"
  (interactive)
  (color-theme-install
   '(color-theme-jpr5-night-old
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-color . "black")
      (foreground-color . "grey85")
      (background-mode . dark)
      (mouse-color . "grey85")
      (cursor-color . "grey85"))
     (default ((t (nil))))
     (font-lock-comment-face ((t (:italic t :foreground "grey60"))))
     (font-lock-string-face ((t (:foreground "Magenta"))))
     (font-lock-keyword-face ((t (:foreground "Cyan"))))
     ;(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "OliveDrab"))))
     (font-lock-type-face ((t (:foreground "DarkCyan"))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-function-name-face ((t (:foreground "SlateBlue"))))
     (font-lock-builtin-face ((t (:foreground "SkyBlue"))))
     (highline-face ((t (:background "grey12"))))
     (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))
     (show-paren-match-face ((t (:background "grey30"))))
     (region ((t (:background "grey15"))))
     (highlight ((t (:background "blue"))))
     (secondary-selection ((t (:background "navy"))))
     (widget-field-face ((t (:background "navy"))))
     (widget-single-line-field-face ((t (:background "royalblue"))))

     (font-lock-warning-face ((t (:foreground "red" :bold t :italic nil :underline t))))
     (hl-line                ((t (:background "grey12"))))
)))

;; Set the Visual theme
(if window-system
    (color-theme-jpr5-night)
    (color-theme-jpr5-tty))

