;; Generic functions I use from time to time.

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
                         'fullboth)))
;; Take any list-of-list(-of-list) and return a flat list.
(defun flatten (list)
  (mapcan (lambda (x) (if (listp x) x nil)) list))
