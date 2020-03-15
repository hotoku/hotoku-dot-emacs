(add-hook
 'makefile-bsdmake-mode-hook
  (lambda ()
    (local-set-key "\C-c\C-j" 'make-insert-var)))

(defun make-insert-var (var-name)
  (interactive "svariable name:")
  (insert "$(" var-name ")"))

