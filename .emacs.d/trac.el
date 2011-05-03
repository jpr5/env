(require 'xml-rpc)
(require 'trac-wiki)

(add-to-list 'auto-mode-alist '("\\.wiki$" . trac-wiki-mode))

(trac-wiki-define-project "cc" "https://trac.cloudcrowd.com/" t)

(defvar trac-rpc-endpoint-default "https://trac.cloudcrowd.com/login/xmlrpc")

(defun trac-rpc-call-to (ep method &rest args)
  "Call METHOD with ARGS via XML-RPC and return response data.
WARNING: This functionis not use because synchronous
`xml-rpc-method-call' has strange behavour on authentication
retrying.  Use `trac-rpc-call-async' instead."
  (when (< emacs-major-version 22)
    (ad-activate 'encode-coding-string))
  (unwind-protect
      (let* ((url-http-attempt-keepalives trac-wiki-use-keepalive)
         (xml-rpc-base64-encode-unicode nil)
         (xml-rpc-base64-decode-unicode nil)
         (result (with-temp-buffer
               (apply 'xml-rpc-method-call
                  ep method args))))
    (if (and (numberp result) (= result 0))
        nil
      (if (stringp result)
          (apply `concat (split-string result "\r"))
        result)))
    (when (< emacs-major-version 22)
      (ad-deactivate 'encode-coding-string))))

(defun trac-rpc-wiki-to-html-to (ep content)
  "Covnert wiki CONTENT into html via XML-RPC method call."
  (trac-rpc-call-to ep 'wiki.wikiToHtml content))

(defun trac-wiki-preview-default (arg)
  "Preview current wiki content as html page.
   Runs the current buffer through the endpoint
   in variable trac-rpc-endpoint-default."
  (interactive "P")
  (let ((html (trac-rpc-wiki-to-html-to trac-rpc-endpoint-default (buffer-string)))
    (buf (get-buffer-create (if arg " *html-preview-tmp*" "*preview*")))
    (name "TemporaryPage")
    ;; this depen on trac url structure
    (base-url (trac-wiki-strip-url-trailer
               trac-rpc-endpoint-default
               '("xmlrpc" "login" "wiki"))))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil)
            (css (mapconcat
                  (lambda (x)
                    (format "<link rel='stylesheet' href='%s%s%s' type='text/css' />"
                            base-url "/chrome/common/css/" x))
                  '("trac.css" "wiki.css" "site_common.css")
                  "\n")))
        (erase-buffer)
        ;; add some supplements as valid html content
        (insert (format "<html><head><title>%s (preview)</title>" name)
                "\n"
                css
                "\n</head><body>\n"
                "<div id='content' class='wiki'><div class='wikipage'>"
                html
                "</div></div></body>")
        ;; replace links
        (goto-char (point-min))
        (while (re-search-forward "\\(?:href\\|src\\)=\"/" nil t)
          (backward-char 1)
          (insert base-url))
        (require 'browse-url)
        (let ((coding-system-for-write 'utf-8))
          (browse-url-of-buffer)
          (message "Previewing with external browser."))))))

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-preview-default)

