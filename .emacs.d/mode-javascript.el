;; JavaScript / HTML stuff

(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist (append
    '(("\\.js$"    . javascript-mode)
      ("\\.xhtml$" . sgml-mode))
    auto-mode-alist))
(setq sgml-basic-offset 4)

;; ActionScript/ECMAScript

;(autoload 'actionscript-mode "actionscript" nil t)
;(setq auto-mode-alist (append
;    '(("\\.as$"    . actionscript-mode))
;    auto-mode-alist))

(autoload 'ecmascript-mode "ecmascript" nil t)
(setq auto-mode-alist (append
    '(("\\.as$"    . ecmascript-mode))
    auto-mode-alist))

