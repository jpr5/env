;; Git stuff - load vc-git and "fix" the broken blame invocation syntax.
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat rev)))))

;; Enable this for the the ever-awesome git-blame-mode (kinda broken)
;(require 'git)
;(require 'format-spec)
;(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
;(global-set-key (kbd "C-x v b") 'git-blame-mode) ;; FIXME: doesn't work

(setq diff-switches '"-u")
;(setq vc-diff-switches '"-u")
;(setq cvs-diff-flags '("-u"))
;(setq vc-cvs-diff-switches '"-u")
;(setenv "PATH" (concat "/usr/local/git/bin:" (getenv "PATH")))

(setq vc-follow-symlinks t)
