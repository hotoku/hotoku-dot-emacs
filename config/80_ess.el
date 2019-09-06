(require 'ess-site)




(require 'align)
(add-to-list 'align-rules-list
             '(ess-assignment-operator
               (regexp . "\\(\\s-*\\)<?<-[^#\t\n]")
               (repeat . nil)
               (modes  . '(ess-mode))))
(add-to-list 'auto-mode-alist
             '("\\.runit" . R-mode))




(add-hook 'ess-mode-hook
          (lambda ()
            (let ((ext (replace-regexp-in-string
                        ".*\\.\\([^\\.]+\\)$" "\\1"
                        buffer-file-name)))
              (when (string= ext "runit")
                (setq truncate-lines t)))))



;; change assign mark(" <- ") short cut to ":"
(define-key ess-mode-map (kbd ":") 'ess-insert-assign)
