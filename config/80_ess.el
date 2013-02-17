(require 'ess-site)


(require 'align)
(add-to-list 'align-rules-list
             '(ess-assignment-operator
               (regexp . "\\(\\s-*\\)<?<-[^#\t\n]")
               (repeat . nil)
               (modes  . '(ess-mode))))
(add-to-list 'auto-mode-alist
             '("\\.runit" . R-mode))
