(defun make-insert-var (var-name)
  (interactive "svariable name:")
  (insert "$(" var-name ")"))
(add-hook 'makefile-bsdmake-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-j" 'make-insert-var)))
