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

;; Only works on emacs <= 22
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))

;; Take any list-of-list(-of-list) and return a flat list.
(defun flatten (list)
  (mapcan (lambda (x) (if (listp x) x nil)) list))

;; Swap two frames/windows with each other.
(defun swap-windows ()
  "If you have 2 windows open, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))
