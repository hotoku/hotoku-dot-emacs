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




;; unbind "_" from
(add-hook 'ess-mode-hook
          (lambda ()
            (local-unset-key (kbd "_"))))




;; change assign mark(" <- ") short cut to ":"
(define-key ess-mode-map (kbd ":") 'ess-insert-assign)




;; set coding style
(setq ess-default-style 'RStudio)




;; ess command
(defun R-docker ()
  (interactive)
  (let ((ess-r-customize-alist
         (append ess-r-customize-alist
                 '((inferior-ess-program . "~/project/dot-emacs/bin/R-docker"))))
        (ess-R-readline t))
    (R)))




;; face
(font-lock-add-keywords
 'ess-mode
 '(("=" . ess-assignment-face)
   ("\\$" . ess-operator-face)))
