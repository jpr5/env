;; Main VCS settings

(setq vc-follow-symlinks t)
(setq diff-switches '"-u")

;; GIT
(require 'vc-git)

;; Fix vc-git's broken blame invocation syntax (OSX Carbon Emacs 22.3.1).
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat rev)))))

;; Whenever we open a file, if we're in a GIT-controlled file, switch M-x grep
;; to use a **much** faster form of grep.  Assume search-from-cwd, and limit
;; grep depth to cwd vs. M-x rgrep for unlimited depth (recursive).  Nuking the
;; "-- ."  would separately accomplish a full repository-wide search.
;;
;; Plain version:
(defun vc-git-override-grep ()
  "Change buffer-local grep mechanism to use git grep (waaay faster)."
  (when (or
         (eq (vc-backend buffer-file-name) 'Git)
         (eq (vc-backend buffer-file-name) 'git))
    (set (make-local-variable 'grep-command)      '("git --no-pager grep --max-depth 0 -En  -- ." . 39))
    (set (make-local-variable 'grep-find-command) '("git --no-pager grep -En  -- ." . 25))))

;; TODO: It's not possible to use search-path yet since vc-git-override-grep is
;; called through find-file-hook.  We should front-end Mx-{r,}grep
;; buffer-locally with a function that could interactively query for a
;; search-path override (if unspecified).
;(defun vc-git-override-grep (&optional search-path)
;  "Change buffer-local grep mechanism to use git grep (waaay faster)."
;  (when (or
;         (eq (vc-backend buffer-file-name) 'Git)
;         (eq (vc-backend buffer-file-name) 'git))
;    (let ((path (if search-path search-path ".")))
;      (set (make-local-variable 'grep-command)      '((concat "git --no-pager grep --max-depth 0 -En  -- " path) . 39))
;      (set (make-local-variable 'grep-find-command) '((concat "git --no-pager grep -En  -- " path) . 25)))))

(add-hook 'find-file-hook 'vc-git-override-grep)
