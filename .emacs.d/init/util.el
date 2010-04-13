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

