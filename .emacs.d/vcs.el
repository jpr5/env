;;;
;;; Main VCS settings
;;;

(setq vc-follow-symlinks t)
(setq diff-switches '"-u")

;; GIT
(require 'vc-git)
(require 'git)
(require 'git-blame)

;; Fix vc-git's broken blame invocation syntax (OSX Carbon Emacs 22.3.1).
(when (eq emacs-major-version 24)
  (defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 0 name "blame" (if rev (concat rev))))))

;; Whenever we open a file, if we're in a GIT-controlled file, switch M-x grep
;; to use a **much** faster form of grep.
;;
;; Assume search-from-cwd, and limit grep depth to cwd vs. M-x rgrep for
;; unlimited depth (recursive).  Nuking the "-- ."  would separately accomplish
;; a full repository-wide search.
(defun vc-git-override-grep ()
  "Change buffer-local grep mechanism to use git grep (waaay faster)."
  (when (or
         (eq (vc-backend buffer-file-name) 'Git)
         (eq (vc-backend buffer-file-name) 'git))
    (set (make-local-variable 'grep-command)      '("git --no-pager grep --max-depth 0 -En  -- ." . 39))
    (set (make-local-variable 'grep-find-command) '("git --no-pager grep -En  -- ." . 25))))

(add-hook 'find-file-hook 'vc-git-override-grep)

(with-eval-after-load 'git-gutter+
  (global-git-gutter+-mode t))

;; TODO: figure out how to make magit log automatically include graph
;;;###autoload
;(defun magit-file-log (file &optional use-graph)
;  "Display the log for the currently visited file or another one.
;With a prefix argument show the log graph."
;  (interactive
;   (list (magit-read-file-from-rev (magit-get-current-branch)
;                                   (magit-buffer-file-name t))
;         current-prefix-arg))
;  (magit-mode-setup magit-log-buffer-name nil
;                    #'magit-log-mode
;                    #'magit-refresh-log-buffer
;                    'oneline "HEAD"
;                    `(,@(and use-graph (list "--graph"))
;                      ,@magit-custom-options
;                      "--follow")
;                    file))
