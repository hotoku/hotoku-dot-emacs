;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defvar yh-sql-supress-formatting nil "When this value is not nil, yh-sql-format function does nothing.
This variable is not supposed to be set by users.")
(make-variable-buffer-local 'yh-sql-supress-formatting)

(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (unless yh-sql-supress-formatting
    (let* ((curbuf (current-buffer))
           (sql (buffer-string))
           (fpath (make-temp-file "zetasql" nil ".sql"))
           (buffer (find-file fpath)))
      (switch-to-buffer buffer)
      (setq yh-sql-supress-formatting t)
      (insert sql)
      (save-buffer)
      (call-process-shell-command (format "zetasql-formatter %s" fpath))
      (revert-buffer t t)
      (let* ((sql2 (buffer-string)))
        (kill-buffer buffer)
        (switch-to-buffer curbuf)
        (delete-region (point-min) (point-max))
        (insert sql2)))))

(provide 'yh-sql)
;;; yh-sql.el ends here
