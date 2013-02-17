(defadvice sh-mode (after my-shell-script-mode-hook activate)
  (progn
    (set-face-foreground 'sh-heredoc "navy")
    (local-set-key "\C-c\C-j" 'sh-insert-var)))
(defun sh-insert-var (var-name)
  (interactive "svariable name:")
  (insert "${" var-name "}"))
